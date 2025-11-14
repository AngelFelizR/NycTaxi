# Test collect_predictions_best_config ----------------------------------------

test_that("collect_predictions_best_config returns predictions with model_name", {
  # Create mock tuned workflow with predictions
  mock_predictions <- tibble::tibble(
    .config = c("Config1", "Config1", "Config2", "Config2"),
    .pred_yes = c(0.7, 0.8, 0.6, 0.5),
    .row = 1:4
  )

  mock_metrics <- tibble::tibble(
    .config = c("Config1", "Config2"),
    brier_class = c(0.15, 0.20),
    .metric = "brier_class"
  )

  mock_tuned <- list(
    logistic_reg = list(
      predictions = mock_predictions,
      metrics = mock_metrics
    )
  )

  # Mock the tune functions
  with_mocked_bindings(
    select_best = function(x, metric) tibble::tibble(.config = "Config1"),
    collect_predictions = function(x) mock_predictions,
    .package = "tune",
    {
      result <- collect_predictions_best_config(
        mock_tuned,
        "logistic_reg",
        "brier_class"
      )

      expect_s3_class(result, "tbl_df")
      expect_true("model_name" %in% names(result))
      expect_equal(result$model_name[1], "logistic_reg")
      expect_equal(nrow(result), 2)
      expect_true(all(result$.config == "Config1"))
    }
  )
})

test_that("collect_predictions_best_config filters correct configuration", {
  mock_predictions <- tibble::tibble(
    .config = c("A", "A", "B", "B"),
    .pred_yes = c(0.7, 0.8, 0.6, 0.5)
  )

  with_mocked_bindings(
    select_best = function(x, metric) tibble::tibble(.config = "B"),
    collect_predictions = function(x) mock_predictions,
    .package = "tune",
    {
      result <- collect_predictions_best_config(
        list(model1 = list()),
        "model1",
        "accuracy"
      )

      expect_equal(nrow(result), 2)
      expect_true(all(result$.config == "B"))
    }
  )
})

# Test add_performance_variables -----------------------------------------------

test_that("add_performance_variables joins correctly", {
  training_data <- tibble::tibble(
    trip_id = c("T1", "T2", "T3"),
    performance_per_hour = c(100, 150, 120),
    percentile_75_performance = c(110, 140, 130),
    other_col = c("a", "b", "c")
  )

  predictions <- tibble::tibble(
    .row = c(1, 2, 3),
    .pred_yes = c(0.7, 0.8, 0.6),
    model_name = "test_model"
  )

  result <- add_performance_variables(training_data, predictions)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_true(all(
    c(
      "trip_id",
      "performance_per_hour",
      "percentile_75_performance",
      ".pred_yes"
    ) %in%
      names(result)
  ))
  expect_equal(result$trip_id, c("T1", "T2", "T3"))
  expect_false("other_col" %in% names(result))
})

test_that("add_performance_variables handles partial matches", {
  training_data <- tibble::tibble(
    trip_id = c("T1", "T2", "T3", "T4"),
    performance_per_hour = c(100, 150, 120, 130),
    percentile_75_performance = c(110, 140, 130, 135)
  )

  predictions <- tibble::tibble(
    .row = c(1, 3),
    .pred_yes = c(0.7, 0.6)
  )

  result <- add_performance_variables(training_data, predictions)

  expect_equal(nrow(result), 2)
  expect_equal(result$trip_id, c("T1", "T3"))
})

test_that("add_performance_variables preserves prediction columns", {
  training_data <- tibble::tibble(
    trip_id = "T1",
    performance_per_hour = 100,
    percentile_75_performance = 110
  )

  predictions <- tibble::tibble(
    .row = 1,
    .pred_yes = 0.7,
    .pred_no = 0.3,
    model_name = "test"
  )

  result <- add_performance_variables(training_data, predictions)

  expect_true("model_name" %in% names(result))
  expect_true(".pred_no" %in% names(result))
})

# Test add_pred_class ----------------------------------------------------------

test_that("add_pred_class creates binary classifications", {
  predictions <- tibble::tibble(
    .pred_yes = c(0.3, 0.6, 0.75, 0.9),
    .row = 1:4
  )

  result <- add_pred_class(predictions, threshold = 0.5)

  expect_true(".pred_class" %in% names(result))
  expect_true("threshold" %in% names(result))
  expect_equal(result$.pred_class, c("no", "yes", "yes", "yes"))
  expect_equal(result$threshold, rep(0.5, 4))
})

test_that("add_pred_class handles edge cases at threshold", {
  predictions <- tibble::tibble(
    .pred_yes = c(0.5, 0.500001, 0.499999)
  )

  result <- add_pred_class(predictions, threshold = 0.5)

  expect_equal(result$.pred_class, c("yes", "yes", "no"))
})

test_that("add_pred_class returns early if .pred_class exists", {
  predictions <- tibble::tibble(
    .pred_yes = c(0.3, 0.7),
    .pred_class = c("yes", "no") # Intentionally wrong
  )

  result <- add_pred_class(predictions, threshold = 0.5)

  expect_equal(result$.pred_class, c("yes", "no"))
  expect_false("threshold" %in% names(result))
})

test_that("add_pred_class works with different thresholds", {
  predictions <- tibble::tibble(.pred_yes = c(0.2, 0.4, 0.6, 0.8))

  result_low <- add_pred_class(predictions, threshold = 0.3)
  result_high <- add_pred_class(predictions, threshold = 0.7)

  expect_equal(sum(result_low$.pred_class == "yes"), 3)
  expect_equal(sum(result_high$.pred_class == "yes"), 1)
})

# Test calculate_costs ---------------------------------------------------------

test_that("calculate_costs computes all cost columns", {
  predictions <- tibble::tibble(
    take_current_trip = c("yes", "no", "yes", "no"),
    .pred_class = c("yes", "no", "no", "yes"),
    performance_per_hour = c(100, 80, 120, 90),
    percentile_75_performance = c(110, 100, 110, 110)
  )

  result <- calculate_costs(predictions)

  expect_true(all(
    c(
      "current_method_cost",
      "cost_wrong_no",
      "cost_wrong_yes",
      "cost_wrong_total"
    ) %in%
      names(result)
  ))
  expect_equal(nrow(result), 4)
})

test_that("calculate_costs correctly computes current_method_cost", {
  predictions <- tibble::tibble(
    take_current_trip = c("yes", "no", "no"),
    .pred_class = c("yes", "no", "no"),
    performance_per_hour = c(100, 80, 120),
    percentile_75_performance = c(110, 100, 110)
  )

  result <- calculate_costs(predictions)

  # When take_current_trip == "yes", cost should be 0
  # When take_current_trip == "no", cost = percentile_75 - performance
  expect_equal(result$current_method_cost, c(0, 20, -10))
})

test_that("calculate_costs correctly computes cost_wrong_no", {
  predictions <- tibble::tibble(
    take_current_trip = c("yes", "yes", "no"),
    .pred_class = c("no", "yes", "no"),
    performance_per_hour = c(100, 80, 120),
    percentile_75_performance = c(110, 100, 110)
  )

  result <- calculate_costs(predictions)

  # cost_wrong_no only applies when actual != predicted AND predicted == "no"
  # Row 1: yes != no AND .pred_class == no: 100 - 110 = -10
  # Row 2: yes == yes: 0
  # Row 3: no == no: 0
  expect_equal(result$cost_wrong_no, c(-10, 0, 0))
})

test_that("calculate_costs correctly computes cost_wrong_yes", {
  predictions <- tibble::tibble(
    take_current_trip = c("no", "yes", "no"),
    .pred_class = c("yes", "yes", "no"),
    performance_per_hour = c(100, 80, 120),
    percentile_75_performance = c(110, 100, 110)
  )

  result <- calculate_costs(predictions)

  # cost_wrong_yes only applies when actual != predicted AND predicted == "yes"
  # Row 1: no != yes AND .pred_class == yes: 110 - 100 = 10
  # Row 2: yes == yes: 0
  # Row 3: no == no: 0
  expect_equal(result$cost_wrong_yes, c(10, 0, 0))
})

test_that("calculate_costs correctly computes cost_wrong_total", {
  predictions <- tibble::tibble(
    take_current_trip = c("yes", "no", "yes", "no"),
    .pred_class = c("no", "yes", "yes", "no"),
    performance_per_hour = c(100, 80, 120, 90),
    percentile_75_performance = c(110, 100, 110, 110)
  )

  result <- calculate_costs(predictions)

  expect_equal(
    result$cost_wrong_total,
    result$cost_wrong_no + result$cost_wrong_yes
  )
})

test_that("calculate_costs handles all correct predictions", {
  predictions <- tibble::tibble(
    take_current_trip = c("yes", "no"),
    .pred_class = c("yes", "no"),
    performance_per_hour = c(100, 80),
    percentile_75_performance = c(110, 100)
  )

  result <- calculate_costs(predictions)

  expect_equal(result$cost_wrong_no, c(0, 0))
  expect_equal(result$cost_wrong_yes, c(0, 0))
  expect_equal(result$cost_wrong_total, c(0, 0))
})

test_that("calculate_costs handles negative performance differences", {
  predictions <- tibble::tibble(
    take_current_trip = c("no", "yes"),
    .pred_class = c("no", "yes"),
    performance_per_hour = c(120, 150),
    percentile_75_performance = c(100, 100)
  )

  result <- calculate_costs(predictions)

  # Current method cost when rejecting trip with high performance
  expect_equal(result$current_method_cost, c(-20, 0))
})
