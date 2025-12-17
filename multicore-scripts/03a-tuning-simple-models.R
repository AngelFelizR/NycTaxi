# Importing ----

## Libraries -----
library(tidymodels)
library(themis)
library(future)

## Removing limit for parallel processing ----
options(future.globals.maxSize = 16.5 * 1e9)

## Pin boards to use ----
BoardLocal <- pins::board_folder(here::here("../NycTaxiPins/Board"))

## Resample to use -----
TrainingSampleJoined <- pins::pin_read(BoardLocal, "TrainingSampleJoined")

set.seed(5878)
TrainingSampleJoinedResamples <- vfold_cv(TrainingSampleJoined, v = 5)

## Metrics to eval -----
MetricsToEval <- metric_set(roc_auc, brier_class)

## Workflow to eval ----
WorkFlowSimple <-
  pins::pin_read(BoardLocal, "WorkFlowSimple") |>
  extract_workflow(id = "normalized_reg_logistic")

## Grid to eval ----
WfParams <- extract_parameter_set_dials(WorkFlowSimple)

set.seed(14005)
initial_grid <- grid_space_filling(
  WfParams,
  size = 5,
  type = "audze_eglais",
  original = FALSE
)

print("Tuning normalized_reg_logistic with grid:")
print(initial_grid)

plan(multicore, workers = 5)

WorkFlowSimpleTuned <- tune_grid(
  object = WorkFlowSimple,
  resamples = TrainingSampleJoinedResamples,
  param_info = WfParams,
  grid = initial_grid,
  metrics = MetricsToEval,
  control = control_grid(
    verbose = TRUE,
    parallel_over = "resamples",
    save_pred = TRUE
  )
)

plan(sequential)

print("Completed normalized_reg_logistic")


# Save all results together
pins::pin_write(
  BoardLocal,
  list(normalized_reg_logistic = WorkFlowSimpleTuned),
  "WorkFlowSimpleTuned",
  type = "qs2",
  title = "Work Flow Simple Tuned"
)

print("All simple models completed!")
