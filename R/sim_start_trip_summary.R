#' Summarize Simulation Results by Start Trip
#'
#' This function calculates performance metrics (trips, hours, earnings, and wages)
#' for ride-sharing simulations. It joins simulation results with initial trip data,
#' calculates per-seed statistics, and returns the mean and standard deviation
#' across all seeds for each simulation ID.
#'
#' @param sim_results A \code{data.table} containing simulation results.
#'   Must include \code{simulation_id}, \code{simulation_seed}, \code{sim_dropoff_datetime},
#'   \code{sim_driver_pay}, and \code{sim_tips}.
#' @param sim_start_days A \code{data.table} containing initial trip information.
#'   Must include \code{trip_id} (to join with \code{simulation_id}),
#'   \code{request_datetime}, and \code{trip_time}.
#'
#' @return A \code{data.table} with one row per \code{simulation_id}. Columns include
#'   means and standard deviations for:
#'   \itemize{
#'     \item \code{n_trips}: Total number of trips completed.
#'     \item \code{total_hours_worked}: Time span from the first trip's start to the last drop-off.
#'     \item \code{total_earnings}: Sum of driver pay and tips.
#'     \item \code{daily_hourly_wage}: Total earnings divided by total hours worked.
#'   }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Joins \code{sim_results} with \code{sim_start_days} to establish the
#'     \code{initial_day_time}.
#'   \item Aggregates metrics by \code{simulation_id} and \code{simulation_seed}.
#'   \item Computes the mean and standard deviation across seeds for each simulation.
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' summary <- sim_start_trip_summary(sim_output, start_info)
#' }
sim_start_trip_summary <- function(sim_results, sim_start_days) {
  # 1. Type Validation
  if (!data.table::is.data.table(sim_results)) {
    stop("'sim_results' must be a data.table.")
  }
  if (!data.table::is.data.table(sim_start_days)) {
    stop("'sim_start_days' must be a data.table.")
  }

  # 2. Column Validation
  req_results <- c(
    "simulation_id",
    "simulation_seed",
    "sim_dropoff_datetime",
    "sim_driver_pay",
    "sim_tips"
  )
  req_start <- c("trip_id", "request_datetime", "trip_time")

  missing_res <- setdiff(req_results, names(sim_results))
  if (length(missing_res) > 0) {
    stop(
      "sim_results is missing columns: ",
      paste(missing_res, collapse = ", ")
    )
  }

  missing_start <- setdiff(req_start, names(sim_start_days))
  if (length(missing_start) > 0) {
    stop(
      "sim_start_days is missing columns: ",
      paste(missing_start, collapse = ", ")
    )
  }

  # 3. Data Processing
  sim_results <- data.table::copy(sim_results)

  # Join and calculate initial time
  sim_results[
    sim_start_days,
    on = c("simulation_id" = "trip_id"),
    initial_day_time := request_datetime + lubridate::seconds(trip_time)
  ]

  # Validation: Ensure the join didn't result in all NAs
  if (sim_results[, all(is.na(initial_day_time))]) {
    stop(
      "Join failed: simulation_id in 'sim_results' does not match any trip_id in 'sim_start_days'."
    )
  }

  sim_results_summary <- sim_results[,
    .(
      n_trips = .N,
      total_hours_worked = difftime(
        max(sim_dropoff_datetime, na.rm = TRUE),
        min(initial_day_time, na.rm = TRUE),
        unit = "hours"
      ) |>
        as.double(),
      total_earnings = sum(sim_driver_pay, na.rm = TRUE) +
        sum(sim_tips, na.rm = TRUE)
    ),
    by = c("simulation_id", "simulation_seed")
  ][, daily_hourly_wage := total_earnings / total_hours_worked][,
    j = c(
      stats::setNames(
        lapply(.SD, base::mean, na.rm = TRUE),
        paste0(names(.SD), "_mean")
      ),
      stats::setNames(
        lapply(.SD, stats::sd, na.rm = TRUE),
        paste0(names(.SD), "_sd")
      )
    ),
    .SDcols = !c("simulation_seed"),
    by = "simulation_id"
  ]

  # 4. Final Formatting
  data.table::setcolorder(
    sim_results_summary,
    c(
      "simulation_id",
      paste0("n_trips", c("_mean", "_sd")),
      paste0("total_hours_worked", c("_mean", "_sd")),
      paste0("total_earnings", c("_mean", "_sd")),
      paste0("daily_hourly_wage", c("_mean", "_sd"))
    )
  )

  return(sim_results_summary)
}
