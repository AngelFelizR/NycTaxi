test_that("sim_start_trip_summary validates inputs correctly", {
  # Setup valid data for use in negative tests
  valid_results <- data.table::data.table(
    simulation_id = 1L,
    simulation_seed = 1L,
    sim_dropoff_datetime = base::as.POSIXct("2024-01-01 10:00:00", tz = "UTC"),
    sim_driver_pay = 10,
    sim_tips = 5
  )

  valid_start <- data.table::data.table(
    trip_id = 1L,
    request_datetime = base::as.POSIXct("2024-01-01 08:00:00", tz = "UTC"),
    trip_time = 3600
  )

  # 1. Test: Non-data.table inputs
  expect_error(
    sim_start_trip_summary(base::as.data.frame(valid_results), valid_start),
    "must be a data.table"
  )

  # 2. Test: Missing columns in sim_results
  bad_results <- data.table::copy(valid_results)
  bad_results[, sim_tips := NULL]
  expect_error(
    sim_start_trip_summary(bad_results, valid_start),
    "sim_results is missing columns: sim_tips"
  )

  # 3. Test: Join failure (No matching IDs)
  mismatched_start <- data.table::copy(valid_start)
  mismatched_start[, trip_id := 999L] # ID that doesn't exist in results
  expect_error(
    sim_start_trip_summary(valid_results, mismatched_start),
    "Join failed"
  )
})

test_that("sim_start_trip_summary calculates correct aggregates across seeds", {
  # 1. Setup Dummy Data
  # -------------------
  # Create starting conditions for two distinct simulations
  sim_start_days <- data.table::data.table(
    trip_id = c(1L, 2L),
    request_datetime = as.POSIXct(
      c("2024-01-01 08:00:00", "2024-01-02 08:00:00"),
      tz = "UTC"
    ),
    trip_time = c(3600, 1800) # 1 hr (initial time 09:00), 0.5 hr (initial time 08:30)
  )

  # Create simulation results with multiple seeds
  sim_results <- data.table::data.table(
    simulation_id = c(1L, 1L, 1L, 1L, 2L, 2L),
    simulation_seed = c(1L, 1L, 2L, 2L, 1L, 1L),
    sim_dropoff_datetime = as.POSIXct(
      c(
        # Sim 1, Seed 1 (Start = 09:00) -> Max dropoff 10:00 = 1 hour worked
        "2024-01-01 09:30:00",
        "2024-01-01 10:00:00",
        # Sim 1, Seed 2 (Start = 09:00) -> Max dropoff 11:00 = 2 hours worked
        "2024-01-01 10:00:00",
        "2024-01-01 11:00:00",
        # Sim 2, Seed 1 (Start = 08:30) -> Max dropoff 10:30 = 2 hours worked
        "2024-01-02 09:30:00",
        "2024-01-02 10:30:00"
      ),
      tz = "UTC"
    ),
    sim_driver_pay = c(10, 15, 20, 20, 15, 15),
    sim_tips = c(5, 5, 5, 5, 0, 0)
  )

  # 2. Execute the Function
  # -----------------------
  res <- sim_start_trip_summary(sim_results, sim_start_days)

  # 3. Assertions
  # -------------

  # Verify object type and dimensions
  expect_s3_class(res, "data.table")
  expect_equal(nrow(res), 2L)

  # Verify column order and naming
  expected_cols <- c(
    "simulation_id",
    "n_trips_mean",
    "n_trips_sd",
    "total_hours_worked_mean",
    "total_hours_worked_sd",
    "total_earnings_mean",
    "total_earnings_sd",
    "daily_hourly_wage_mean",
    "daily_hourly_wage_sd"
  )
  expect_equal(names(res), expected_cols)

  # Verify mathematical logic for Simulation 1
  # Sim 1, Seed 1 totals: Hrs = 1, Earnings = 35, Wage = 35
  # Sim 1, Seed 2 totals: Hrs = 2, Earnings = 50, Wage = 25
  res_sim1 <- res[simulation_id == 1L]

  # Means
  expect_equal(res_sim1$total_hours_worked_mean, 1.5)
  expect_equal(res_sim1$total_earnings_mean, 42.5)
  expect_equal(res_sim1$daily_hourly_wage_mean, 30.0)
  expect_equal(res_sim1$n_trips_mean, 2.0)

  # Standard Deviations
  expect_equal(res_sim1$total_hours_worked_sd, stats::sd(c(1, 2)))
  expect_equal(res_sim1$total_earnings_sd, stats::sd(c(35, 50)))
  expect_equal(res_sim1$daily_hourly_wage_sd, stats::sd(c(35, 25)))

  # Verify NA handling for single-seed standard deviations
  # Simulation 2 only has Seed 1, so its SDs should evaluate to NA
  res_sim2 <- res[simulation_id == 2L]
  expect_true(is.na(res_sim2$total_hours_worked_sd))
})
