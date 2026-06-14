#' Optimize trip start times to maximize hourly earnings
#'
#' Applies a policy to adjust trip start times and enforce Uber as the taxi
#' company. Trips that are not predicted as high-value by the fitted model are
#' delayed to the next valid start time (hour/day combination) from a lookup
#' table. The function ensures that after adjustment all trips are classified
#' as high-value.
#'
#' @param trips_dt A data.table containing trip information. Must have columns:
#'   `hvfhs_license_num`, `request_datetime` (POSIXct), and `DOLocationID`.
#' @param start_day_fitted_model A fitted tidymodels workflow (or any model
#'   object with a `predict` method that returns a tibble containing a column
#'   `.pred_class` with values "yes"/"no"). The model is used to classify
#'   whether a trip start is high-value. It must have been trained on features
#'   including `hvfhs_license_num`, `request_datetime`, `DOLocationID`, and
#'   `daily_hourly_wage_mean` (the latter is set to 0 by this function).
#' @param valid_start_times_dt A data.table defining valid start hours. Must
#'   have columns: `hour` (integer 0-23), `week_day` (integer 1-7, where
#'   1 = Sunday), and `week_cycle` (integer 0-167, computed as
#'   (week_day-1)*24 + hour).
#'
#' @return A modified data.table with the same structure as `trips_dt`, but with
#'   - `hvfhs_license_num` forced to `"HV0003"` (Uber),
#'   - `request_datetime` adjusted to the next valid start time for trips that
#'     were not initially classified as high-value,
#'   - All trips guaranteed to be classified as high-value after adjustment
#'     (asserted via `stopifnot`).
#'   - Auxiliary columns `daily_hourly_wage_mean` and `simulation_id` (added
#'     internally) are removed before returning.
#'
#' @details
#' The function works as follows:
#' 1. Validates input types and required columns.
#' 2. Makes a copy of `trips_dt` and sets `hvfhs_license_num = "HV0003"`,
#'    `daily_hourly_wage_mean = 0`, `simulation_id = 0` (the latter two are
#'    required by the model but not used in final output).
#' 3. Uses `stats::predict()` with `type = "class"` on the entire dataset to
#'    identify trips classified as "no" (non-high-value).
#' 4. For those trips, computes the current `week_cycle` from
#'    `request_datetime`, then finds the next larger `week_cycle` from
#'    `valid_start_times_dt` (allowing wrap‑around to the following week).
#' 5. Updates `request_datetime` to the nearest hour floor plus the waiting
#'    duration in hours.
#' 6. Verifies that after adjustment all updated trips are now classified as
#'    "yes". If not, an error is thrown.
#' 7. Removes the temporary columns and returns the modified data.table.
#'
#' The function assumes that the model was trained on data where a positive
#' classification corresponds to a high-value start (i.e., the policy that
#' leads to above‑baseline hourly earnings). The valid start times are
#' typically derived from the model's decision tree (e.g., hours and days where
#' the model predicts "yes").
#'
#' @examples
#' \dontrun{
#' # Assuming `fitted_workflow` and `valid_hours` are available
#' library(data.table)
#' trips <- data.table(
#'   hvfhs_license_num = c("HV0005", "HV0003"),
#'   request_datetime = as.POSIXct(c("2023-01-01 09:00:00", "2023-01-01 22:00:00")),
#'   DOLocationID = c("100", "200")
#' )
#' optimized_trips <- optimize_trip_start_time(trips, fitted_workflow, valid_hours)
#' }
#'
#' @export
optimize_trip_start_time <- function(
  trips_dt,
  start_day_fitted_model,
  valid_start_times_dt
) {
  # Validating data imported
  stopifnot(
    "trips_dt must be a data.table" = data.table::is.data.table(trips_dt)
  )
  stopifnot(
    "valid_start_times_dt must be a data.table" = data.table::is.data.table(
      valid_start_times_dt
    )
  )

  # Validating the the dataframes present the expected columns
  if (
    !all(
      c("hvfhs_license_num", "request_datetime", "DOLocationID") %chin%
        names(trips_dt)
    )
  ) {
    stop(
      "trips_dt must have `hvfhs_license_num`, `DOLocationID`, and `request_datetime` as columns."
    )
  }
  if (
    !all(c("hour", "week_day", "week_cycle") %chin% names(valid_start_times_dt))
  ) {
    stop(
      "valid_start_times_dt must have `hour`, `week_day`, and `week_cycle` as columns."
    )
  }

  # We know that to pass the model taxi needs to user Uber
  # We are also adding daily_hourly_wage_mean as was used to train the model
  trips_dt = data.table::copy(trips_dt)
  trips_dt[, `:=`(
    hvfhs_license_num = "HV0003",
    daily_hourly_wage_mean = 0,
    simulation_id = 0
  )]

  # Defining cases that need to updated to start at better time
  update_time_cases = which(
    stats::predict(
      start_day_fitted_model,
      new_data = trips_dt,
      type = "class"
    )[[".pred_class"]] ==
      "no"
  )

  # Adding the possibility that the next valid hour will take place next week
  valid_times_next_week =
    data.table::copy(valid_start_times_dt)[
      j = week_cycle := week_cycle + 7 * 24
    ][j = rbind(valid_start_times_dt, .SD)]

  trips_dt[
    update_time_cases,
    j = request_datetime := {
      # Converting current time to cycle
      current_week_cycle =
        (lubridate::wday(request_datetime) - 1) *
        24 +
        lubridate::hour(request_datetime)

      # Defining the new time to cycle
      next_week_cycle = valid_times_next_week[
        list(current_week_cycle = current_week_cycle),
        on = .(week_cycle > current_week_cycle),
        mult = "first",
        x.week_cycle
      ]

      # Waiting until the correct time
      updated_request_datetime =
        lubridate::floor_date(request_datetime, unit = "hour") +
        lubridate::dhours(next_week_cycle - current_week_cycle)

      # Updating the time
      updated_request_datetime
    }
  ]

  high_value_start = stats::predict(
    start_day_fitted_model,
    new_data = trips_dt[update_time_cases],
    type = "class"
  )[[".pred_class"]] ==
    "yes"

  stopifnot("All trips must be high value" = all(high_value_start))

  # We don't need to keep this variable implemented during training
  trips_dt[, `:=`(daily_hourly_wage_mean = NULL, simulation_id = NULL)]

  # Retorning updated trips
  return(trips_dt)
}
