#' Get Best Model Predictions Based on Metric
#'
#' Extracts predictions from the best performing configuration of a tuned
#' workflow based on a specified metric.
#'
#' @param tuned_wf A list containing tuned workflow objects
#' @param model_name Character string specifying which model to extract from tuned_wf
#' @param metric Character string specifying the metric to use for selecting
#'   the best configuration (default: "brier_class")
#'
#' @return A tibble with predictions from the best model configuration,
#'   including a model_name column
#'
#' @export
collect_predictions_best_config <- function(tuned_wf, model_name, metric) {
  best_config = tune::select_best(
    tuned_wf[[model_name]],
    metric = metric
  ) |>
    dplyr::pull(.config)

  best_predictions =
    tune::collect_predictions(tuned_wf[[model_name]]) |>
    dplyr::filter(.config == best_config) |>
    dplyr::mutate(model_name = model_name)

  return(best_predictions)
}

#' Add Performance Variables to Predictions
#'
#' Joins predictions with performance-related variables from the training dataset.
#' Only includes variables needed for cost calculations: trip_id,
#' performance_per_hour, and percentile_75_performance.
#'
#' @param training_data A data frame containing the original training data
#' @param predictions A data frame with model predictions, must contain a .row column
#'
#' @return A tibble combining predictions with cost-related variables from
#'   training data, joined by .row
#'
#' @export
add_performance_variables <- function(training_data, predictions) {
  predict_with_performace =
    training_data |>
    dplyr::transmute(
      .row = dplyr::row_number(),
      trip_id,
      performance_per_hour,
      percentile_75_performance
    ) |>
    dplyr::right_join(predictions, by = ".row")

  return(predict_with_performace)
}

#' Apply Probability Threshold to Create Predicted Classes
#'
#' Converts probability predictions to binary classes based on a specified
#' threshold value.
#'
#' @param predictions A data frame with predictions and .pred_yes column
#' @param threshold Numeric value between 0 and 1 for classification threshold
#'
#' @return A tibble with added threshold and .pred_class columns
#'
#' @export
add_pred_class <- function(predictions, threshold) {
  if (".pred_class" %in% names(predictions)) {
    return(predictions)
  }
  training_data_with_class =
    predictions |>
    dplyr::mutate(
      threshold = threshold,
      .pred_class = dplyr::if_else(.pred_yes >= threshold, "yes", "no")
    )

  return(training_data_with_class)
}

#' Calculate Prediction Costs
#'
#' Computes various cost metrics based on prediction accuracy, including
#' current method cost and costs for different types of prediction errors.
#'
#' @param predictions_with_threshold A data frame with predictions, thresholds,
#'   and cost-related variables (performance_per_hour, percentile_75_performance,
#'   take_current_trip, .pred_class)
#'
#' @return A tibble with added cost columns: current_method_cost, cost_wrong_no,
#'   cost_wrong_yes, and cost_wrong_total
#'
#' @export
calculate_costs <- function(predictions_with_threshold) {
  predictions_with_cost =
    predictions_with_threshold |>
    dplyr::mutate(
      current_method_cost = dplyr::if_else(
        take_current_trip == "no",
        percentile_75_performance - performance_per_hour,
        0
      ),
      cost_wrong_no = dplyr::if_else(
        take_current_trip != .pred_class & .pred_class == "no",
        performance_per_hour - percentile_75_performance,
        0
      ),
      cost_wrong_yes = dplyr::if_else(
        take_current_trip != .pred_class & .pred_class == "yes",
        percentile_75_performance - performance_per_hour,
        0
      ),
      cost_wrong_total = cost_wrong_no + cost_wrong_yes
    )

  return(predictions_with_cost)
}
