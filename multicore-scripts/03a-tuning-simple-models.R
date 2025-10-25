# Importing ----

## Libraries -----
library(tidymodels)
library(embed)
library(themis)
library(discrim)
library(baguette)
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
WorkFlowSimple <- pins::pin_read(BoardLocal, "WorkFlowSimple")

# Tunning and saving ----
results_list <- list()

for (wf_id_i in WorkFlowSimple$wflow_id) {
  wf_i <- extract_workflow(WorkFlowSimple, id = wf_id_i)
  wf_param_i <- extract_parameter_set_dials(wf_i)

  set.seed(14005)
  initial_grid <- grid_space_filling(
    wf_param_i,
    size = 5,
    type = "audze_eglais",
    original = FALSE
  )

  print(paste("Tuning", wf_id_i, "with grid:"))
  print(initial_grid)

  plan(multicore, workers = 3)

  results_list[[wf_id_i]] <- tune_grid(
    object = wf_i,
    resamples = TrainingSampleJoinedResamples,
    param_info = wf_param_i,
    grid = initial_grid,
    metrics = MetricsToEval,
    control = control_grid(
      verbose = TRUE,
      parallel_over = "resamples",
      save_pred = FALSE
    )
  )

  plan(sequential)

  print(paste("Completed", wf_id_i))
}

# Save all results together
pins::pin_write(
  BoardLocal,
  results_list,
  "WorkFlowSimpleTuned",
  type = "qs2",
  title = "Work Flow Simple Tuned"
)

print("All simple models completed!")
