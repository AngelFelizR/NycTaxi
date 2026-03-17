testthat::test_that("calculate_costs computes correct opportunity and loss costs", {
  # 1. Setup mock data representing the 4 quadrants
  # Row 1: True Positive (Good trip, predicted Good)
  # Row 2: False Negative (Good trip, predicted Bad) -> Opportunity Cost
  # Row 3: False Positive (Bad trip, predicted Good) -> Loss Cost
  # Row 4: True Negative (Bad trip, predicted Bad) -> Current Method Cost only

  mock_predictions <- tibble::tibble(
    trip_id = c("A1", "B2", "C3", "D4"),
    take_current_trip = c("yes", "yes", "no", "no"),
    .pred_class = c("yes", "no", "yes", "no"),
    performance_per_hour = c(50, 60, 30, 20),
    percentile_75_performance = c(40, 40, 40, 40),
    trip_time = c(3600, 3600, 3600, 3600) # 3600s = 1 hour
  )

  # 2. Execute the function
  result <- calculate_costs(mock_predictions)

  # 3. Assertions

  # --- Scenario 1: True Positive ---
  # Good trip correctly accepted. No costs incurred.
  testthat::expect_equal(result$current_method_cost[1], 0)
  testthat::expect_equal(result$cost_wrong_total[1], 0)

  # --- Scenario 2: False Negative (cost_wrong_no) ---
  # A "yes" trip was predicted as "no".
  # We earned 60, but benchmark was 40. We missed 20.
  # Math: (60 - 40) * (3600 / 3600) = 20
  testthat::expect_equal(result$cost_wrong_no[2], 20)
  testthat::expect_equal(result$cost_wrong_total[2], 20)
  testthat::expect_equal(result$current_method_cost[2], 0)

  # --- Scenario 3: False Positive (cost_wrong_yes) ---
  # A "no" trip was predicted as "yes".
  # We earned 30, benchmark was 40. We lost 10.
  # Current method also incurs this cost because it accepts everything.
  testthat::expect_equal(result$cost_wrong_yes[3], 10)
  testthat::expect_equal(result$current_method_cost[3], 10)
  testthat::expect_equal(result$cost_wrong_total[3], 10)

  # --- Scenario 4: True Negative ---
  # Bad trip correctly rejected.
  # Current method loses 20 (40 - 20), but model cost is 0.
  testthat::expect_equal(result$current_method_cost[4], 20)
  testthat::expect_equal(result$cost_wrong_total[4], 0)

  # 4. Integrity Checks
  # Ensure trip_id is preserved
  testthat::expect_identical(result$trip_id, mock_predictions$trip_id)

  # Ensure all expected columns are present
  expected_cols <- c(
    "current_method_cost",
    "cost_wrong_no",
    "cost_wrong_yes",
    "cost_wrong_total"
  )
  testthat::expect_true(all(expected_cols %chin% names(result)))
})
