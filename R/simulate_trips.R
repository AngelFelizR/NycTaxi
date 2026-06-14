#' Simulate full workdays for NYC taxi drivers (baseline or policy-driven)
#'
#' Runs the exact empirical Monte-Carlo simulation described in Warren B. Powell (2022)
#' *Sequential Decision Analytics and Modeling* and formalised in the ADP chapter of the
#' project documentation. For each starting trip in `start_points` it simulates a
#' complete 8-hour + 30-minute workday by repeatedly querying the database for
#' candidate trips that satisfy the search‑radius, time‑window, company, WAV, and
#' shift‑limit constraints.
#'
#' Two operating modes are supported:
#'
#' * **Baseline mode** (`fitted_wf = NULL`): the driver accepts a trip chosen
#'   uniformly at random from the qualifying candidates (reproducible via
#'   `seeds`). This is the myopic policy \eqn{X^\pi(S_t^n \mid \theta)} with
#'   uniform sampling over \eqn{\mathcal{C}_t^n}.
#'
#' * **Policy mode** (`fitted_wf` supplied): the fitted XGBoost workflow
#'   predicts acceptance probability; the trip is accepted only if the decision
#'   (class or probability > `threshold`) is positive. Rejected trips incur a
#'   3‑second waiting penalty and the search continues (possibly expanding the
#'   radius).
#'
#' * **Pre‑optimisation of start times**: if `start_day_fitted_model` and
#' `valid_start_times_dt` are also supplied (together with `fitted_wf`), the
#' function first calls `optimize_trip_start_time()` on `start_points`. This
#' forces the taxi company to Uber and delays each start time to the next
#' "high‑value" hour/day combination according to the decision tree model,
#' ensuring that every simulated shift begins from a favourable state.
#'
#' All simulation assumptions from the baseline chapter are enforced:
#' fixed taxi company, WAV compatibility, 4‑hour break (taken only after
#' completing a trip), acceptance of a final trip even if it overruns 8 hours,
#' expanding search radius (1 → 3 → 5 miles, then +2 miles every 2 minutes),
#' and the “more passengers than taxis” rule (random draw from candidates).
#'
#' @param conn A DBI connection to the DuckDB database that contains the
#'   `NycTrips` table (all trip records) and the `PointMeanDistance` table
#'   (pre‑computed mean miles between every pair of zones).
#' @param start_points A `data.table` of reservoir‑sampled starting trips
#'   (one row per replicate). Must contain at least the columns
#'   `trip_id`, `request_datetime`, `trip_time`, `DOLocationID`,
#'   `hvfhs_license_num`, `wav_match_flag`, `driver_pay`, `tips`.
#'   The function validates column presence and data types.
#' @param seeds Optional integer vector of random seeds (one per replicate).
#'   If supplied, `set.seed()` is called before each uniform draw in baseline
#'   mode, guaranteeing reproducible trajectories. If `NULL` the simulation
#'   becomes deterministic (always selects the first candidate returned by the
#'   SQL query — `ORDER BY request_datetime`).
#' @param fitted_wf Optional fitted `tidymodels` `workflow` (XGBoost policy).
#'   When supplied the random‑acceptance logic is replaced by a model‑based
#'   decision. To also pre‑optimise start times, provide
#'   `start_day_fitted_model` and `valid_start_times_dt` as well.
#' @param threshold Optional numeric scalar in `[0,1]`. Only used when
#'   `fitted_wf` is not `NULL`.
#'   * If `NULL` the model’s default `type = "class"` prediction is used.
#'   * If supplied the model returns probabilities (`type = "prob"`) and a trip
#'     is accepted when `.pred_yes > threshold`.
#' @param start_day_fitted_model Optional fitted `tidymodels` workflow (tree model)
#'   that predicts whether a start time is “high‑value”. Must have a `predict()`
#'   method returning `.pred_class` with levels `"yes"/"no"`. Used together with
#'   `valid_start_times_dt` and `fitted_wf` to pre‑optimise start points via
#'   `optimize_trip_start_time()`. If `NULL`, no pre‑optimisation is performed.
#' @param valid_start_times_dt Optional `data.table` defining valid start hours.
#'   Must contain columns `hour`, `week_day`, and `week_cycle` (see
#'   `optimize_trip_start_time` for details). Only used when
#'   `start_day_fitted_model` is not `NULL`.
#' @param verbose Logical scalar (default `FALSE`).
#'   When `TRUE`, the function prints detailed per‑iteration progress messages
#'   inside the main simulation loop:
#'   * current time / distance / time‑window limits at the start of each search,
#'   * break insertion,
#'   * search expansion when no candidates are found,
#'   * policy rejection (with the 3‑second penalty),
#'   * successful trip acceptance.
#'
#'   The top‑level progress line
#'   (`[timestamp] Simulation X/Y | ID: ... | Pending: ...`) is **always** printed,
#'   regardless of this setting. Useful for debugging long runs or watching the
#'   search behaviour live.
#'
#' @return A `data.table` with **one row per accepted trip** across all
#'   replicates. Columns (all prefixed with `sim_` except `simulation_id`):
#'   \itemize{
#'     \item `simulation_id` – identifier of the starting trip (links replicates)
#'     \item `sim_trip_id`, `sim_hvfhs_license_num`, `sim_wav_match_flag`
#'     \item `sim_PULocationID`, `sim_DOLocationID`
#'     \item `sim_request_datetime`, `sim_dropoff_datetime`
#'     \item `sim_trip_time`, `sim_driver_pay`, `sim_tips`
#'   }
#'   The table is ready for downstream hourly‑wage calculations and bootstrap
#'   confidence intervals.
#'
#' @details
#' The core loop implements the transition function and exogenous information
#' process exactly as formalised in Powell’s mathematical model:
#' \itemize{
#'   \item Candidate set \eqn{\mathcal{C}_t^n} is obtained via a fast SQL join
#'         on the pre‑computed `PointMeanDistance` table.
#'   \item When a trip is rejected by the policy the clock advances by 3 seconds
#'         (simulated driver wait) and the search radius may expand.
#'   \item The 30‑minute break is inserted only after a trip finishes and only
#'         once per day.
#'   \item The final trip is always accepted even if it finishes after the
#'         nominal 8‑hour limit.
#' }
#'
#' @examples
#' \dontrun{
#'   # ---------------------------------------------------------------
#'   # Minimal deterministic baseline example
#'   # ---------------------------------------------------------------
#'
#'   library(DBI)
#'   library(data.table)
#'
#'   con <- dbConnect(
#'     duckdb::duckdb(),
#'     system.file("fixtures/minimal-deterministic-db.duckdb",
#'                 package = "NycTaxi")
#'   )
#'
#'   start_points <- readRDS(
#'     system.file("fixtures/SimulationStartTrips_minimal.rds",
#'                 package = "NycTaxi")
#'   )
#'
#'   # Deterministic baseline (seeds = NULL, no model)
#'   sim <- simulate_trips(con, start_points)
#'
#'   # Expected output (exactly the sequence of accepted trips)
#'   print(sim[, .(simulation_id, sim_trip_id, sim_request_datetime)])
#'
#'   DBI::dbDisconnect(con, shutdown = TRUE)
#'}
#' @export
simulate_trips = function(
  conn,
  start_points,
  seeds = NULL,
  fitted_wf = NULL,
  threshold = NULL,
  start_day_fitted_model = NULL,
  valid_start_times_dt = NULL,
  verbose = FALSE
) {
  # This function confirms if:
  # - The db contains the tables needed for the simulation
  # - The tables and start_points have the expected columns
  # - Confirms if start_points is a data.table
  validate_simulation_data(conn, start_points)

  # Making sure to start trips on good days and time
  if (
    !is.null(fitted_wf) &&
      !is.null(start_day_fitted_model) &&
      !is.null(valid_start_times_dt)
  ) {
    start_points = optimize_trip_start_time(
      start_points,
      start_day_fitted_model = start_day_fitted_model,
      valid_start_times_dt = valid_start_times_dt
    )
  }

  cases_to_simulate = expand.grid(
    start_point_id = 1:nrow(start_points),
    seed = if (!is.null(seeds)) seeds else NA_real_
  )

  # Total number of simulations for progress tracking
  total_cases = nrow(cases_to_simulate)

  # START simulation
  all_simulations_table =
    lapply(1:total_cases, \(simulation_i) {
      # GETTING INITIAL TRIP
      initial_conditions = start_points[
        cases_to_simulate$start_point_id[simulation_i],
      ]
      initial_conditions[,
        `:=`(
          simulation_id = start_points[
            cases_to_simulate$start_point_id[simulation_i],
            trip_id
          ],
          simulation_seed = cases_to_simulate$seed[simulation_i]
        )
      ]

      # TRACKING STATUS
      # Calculate proportion of cases pending
      pending_prop = round((total_cases - (simulation_i - 1)) / total_cases, 4)

      if (verbose) {
        cat(sprintf(
          "[%s] Simulation %d/%d | ID: %s | Pending: %.2f%% | Start Time: %s\n",
          Sys.time(),
          simulation_i,
          total_cases,
          initial_conditions$trip_id,
          pending_prop * 100,
          initial_conditions$request_datetime
        ))
      }

      # DEFINING Vehicle Profile

      # 1. Setting taxi company
      taxi_company_code = initial_conditions$hvfhs_license_num

      # 2. Confirming if WAV trips can be taken with SQL
      can_take_wav =
        if (initial_conditions$wav_match_flag == "Y") {
          "('Y', 'N')"
        } else {
          "('N')"
        }

      # DEFINING Shift Constraints

      # 1. Defining the limit to take new trips
      last_time_to_take_trips =
        initial_conditions$request_datetime +
        lubridate::seconds(initial_conditions$trip_time) +
        lubridate::hours(8) +
        lubridate::minutes(30)

      # 2. Defining the time to take the break
      time_to_take_break =
        initial_conditions$request_datetime +
        lubridate::seconds(initial_conditions$trip_time) +
        lubridate::hours(4)

      # DEFINING Initial Operational State

      # 1. Confirms that the trips hasn't been taken
      taken_break = FALSE

      # 2. Defining datetime to start simulation
      current_time =
        initial_conditions$request_datetime +
        lubridate::seconds(initial_conditions$trip_time)

      # 3. Start position
      current_position = initial_conditions$DOLocationID

      # DEFINING Initial Search Policy

      # 1. Defining time limit for first iteration
      trip_time_limit = current_time + lubridate::minutes(1)

      # 2. Defining distance limit for first iteration
      trip_dist_limit = 1

      # DEFINING vars to store information
      simulated_trips = initial_conditions[FALSE, ]
      if (!is.null(seeds)) {
        set.seed(cases_to_simulate$seed[simulation_i])
      }

      # SIMULATIONG TRIP SEARCH
      while (current_time < last_time_to_take_trips) {
        if (verbose) {
          # 1. TRACK THE START OF THE ITERATION
          cat(sprintf("\n[Simulation %d] Searching...\n", simulation_i))
          cat(sprintf(
            "  Current Time: %s | Dist Limit: %s | Time Limit: %s\n",
            current_time,
            trip_dist_limit,
            trip_time_limit
          ))
        }

        # TAKING Break
        if (taken_break == FALSE && current_time >= time_to_take_break) {
          taken_break = TRUE
          current_time = current_time + lubridate::minutes(30)

          # FIX: Reset search window so it starts after the break
          trip_time_limit = current_time + lubridate::minutes(1)
          trip_dist_limit = 1

          if (verbose) {
            # 2. TRACK THE BREAK
            cat(sprintf(
              "  => Taking 30-minute break. New Current Time: %s\n",
              current_time
            ))
          }
        }

        # The query to extract information from DB
        query_to_find_trips = glue::glue(
          "
        SELECT t1.*
        FROM NycTrips t1
        INNER JOIN (
          SELECT * FROM PointMeanDistance 
          WHERE
            -- current drop-off
            PULocationID = {current_position} 
            -- radius constraint         
            AND trip_miles_mean <= {trip_dist_limit}       
        ) t2
          -- next PU must be a valid close zone
          ON t1.PULocationID = t2.DOLocationID             
        WHERE
          -- same company
          t1.hvfhs_license_num = '{taxi_company_code}'
          -- WAV rule (exactly as coded: ('Y','N') if taxi is WAV, else only 'N')  
          AND t1.wav_match_flag IN {can_take_wav}
          -- after driver is free
          AND t1.request_datetime >= '{current_time}'
          -- inside current search window
          AND t1.request_datetime <= '{trip_time_limit}'
          -- the request cannot table place after limit time
          AND t1.request_datetime <= '{last_time_to_take_trips}'
          -- Ensure the destination is also within your allowed zones
          AND t1.DOLocationID IN (SELECT DISTINCT PULocationID FROM PointMeanDistance)
        ORDER BY t1.request_datetime
      "
        )

        # Running the query
        trips_found = DBI::dbGetQuery(conn, query_to_find_trips)
        data.table::setDT(trips_found)

        trips_found_rows = nrow(trips_found)

        if (trips_found_rows == 0L) {
          # TRIP NOT FOUNDs
          if (trip_dist_limit == 1) {
            current_time = current_time + lubridate::minutes(1)
          } else {
            current_time = current_time + lubridate::minutes(2)
          }

          trip_time_limit = trip_time_limit + lubridate::minutes(2)
          trip_dist_limit = trip_dist_limit + 2

          if (verbose) {
            # 3. TRACK SEARCH EXPANSION
            cat(sprintf("  => NO TRIP FOUND. Expanding search.\n"))
          }
        } else {
          # TRIP FOUND
          if (!is.null(seeds)) {
            # Adding stokastic part based on seed
            sampled_trip = trips_found[sample.int(trips_found_rows, 1L), ]
          } else {
            # Here de determinist part of testing
            sampled_trip = trips_found[1L, ]
          }

          if (!is.null(fitted_wf)) {
            # Tranforming data tipe for modeling
            model_sampled_trip = data.table::copy(sampled_trip)
            model_sampled_trip[, `:=`(
              PULocationID = as.character(PULocationID),
              DOLocationID = as.character(DOLocationID),
              performance_per_hour = NA_real_,
              percentile_75_performance = NA_real_
            )]
            # TRIP FOUND & POLICY
            # Getting model prediction
            decision_dt = predict(
              fitted_wf,
              new_data = model_sampled_trip,
              type = if (is.null(threshold)) "class" else "prob"
            )

            # Taking the decision based on \theta
            if (!is.null(threshold)) {
              decision_dt = dplyr::mutate(
                decision_dt,
                .pred_class = if_else(
                  .pred_yes > threshold,
                  "yes",
                  "no"
                ),
                .pred_class = factor(.pred_class, levels = c("yes", "no"))
              )
            }

            # By rejecting trip we just lost time
            # Need to find another trip from the data
            if (decision_dt$.pred_class == "no") {
              current_time =
                sampled_trip$request_datetime +
                lubridate::seconds(3)

              if (current_time > trip_time_limit) {
                trip_time_limit = trip_time_limit + lubridate::minutes(2)
                trip_dist_limit = trip_dist_limit + 2
              }
              if (verbose) {
                # 4. TRACK POLICY REJECTION
                cat(sprintf(
                  "  => TRIP REJECTED BY POLICY. Lost 3 seconds. New Time: %s\n",
                  current_time
                ))
              }
              sampled_trip = NULL
            }
          }

          if (!is.null(sampled_trip)) {
            # ADDING INFORMATION
            sampled_trip[,
              `:=`(
                simulation_id = start_points[
                  cases_to_simulate$start_point_id[simulation_i],
                  trip_id
                ],
                simulation_seed = cases_to_simulate$seed[simulation_i]
              )
            ]
            # Accept
            simulated_trips <- rbind(
              simulated_trips,
              sampled_trip,
              use.names = TRUE,
              fill = FALSE
            )

            # CORRECT updates
            current_time <- sampled_trip$request_datetime +
              lubridate::seconds(sampled_trip$trip_time)
            current_position <- sampled_trip$DOLocationID
            trip_time_limit <- current_time + lubridate::minutes(1)
            trip_dist_limit <- 1

            if (verbose) {
              # 5. TRACK SUCCESSFUL ACCEPTANCE
              cat(sprintf(
                "  => TRIP ACCEPTED! Trip ID: %s\n",
                sampled_trip$trip_id
              ))
              cat(sprintf(
                "  => Jumped to dropoff time. New Current Time: %s\n",
                current_time
              ))
            }
          }
        }
      }

      if (verbose) {
        cat(sprintf(
          "\n[Simulation %d] Reached time limit. Shift Ended.\n",
          simulation_i
        ))
      }

      data.table::setDT(simulated_trips)

      # Returning the data in the expected shape
      shaped_table =
        simulated_trips[, list(
          simulation_id = simulation_id,
          simulation_seed = simulation_seed,
          sim_trip_id = trip_id,
          sim_hvfhs_license_num = hvfhs_license_num,
          sim_wav_match_flag = wav_match_flag,
          sim_PULocationID = PULocationID,
          sim_DOLocationID = DOLocationID,
          sim_request_datetime = request_datetime,
          sim_dropoff_datetime = request_datetime +
            lubridate::seconds(trip_time),
          sim_trip_time = trip_time,
          sim_driver_pay = driver_pay,
          sim_tips = tips
        )]

      return(shaped_table)
    }) |>
    data.table::rbindlist()

  # Returning final table
  return(all_simulations_table)
}
