# Importing ----

## Libraries -----
library(tidymodels)
library(themis)
library(baguette)

## Pin boards to use ----
BoardLocal <- pins::board_folder(here::here("../NycTaxiPins/Board"))

## Resample to use -----
TrainingSampleJoined <- pins::pin_read(BoardLocal, "TrainingSampleJoined")

set.seed(5878)
TrainingSampleJoinedResamples <- vfold_cv(TrainingSampleJoined, v = 5)

## Metrics to eval -----
MetricsToEval <- metric_set(roc_auc, brier_class)

## Workflow to eval ----
WorkFlowTrees <- pins::pin_read(BoardLocal, "WorkFlowTrees")


# Tunning and saving ----
results_list <- list()

for (wf_id_i in WorkFlowTrees$wflow_id) {
  wf_i <- extract_workflow(WorkFlowTrees, id = wf_id_i)
  wf_param_i <- extract_parameter_set_dials(wf_i)

  wf_param_i <- update(
    wf_param_i,
    min_n = min_n(range = c(2L, 25L)),
    mtry = mtry(range = c(1L, ncol(TrainingSampleJoined) - 1L))
  )

  set.seed(14005)
  initial_grid <- grid_space_filling(
    wf_param_i,
    size = 5,
    type = "audze_eglais",
    original = FALSE
  )

  print(paste("Tuning", wf_id_i, "with grid:"))
  print(initial_grid)

  results_list[[wf_id_i]] <- tune_grid(
    object = wf_i,
    resamples = TrainingSampleJoinedResamples,
    param_info = wf_param_i,
    grid = initial_grid,
    metrics = MetricsToEval,
    control = control_grid(
      verbose = TRUE,
      parallel_over = NULL,
      save_pred = TRUE
    )
  )

  print(paste("Completed", wf_id_i))

  # Free memory
  gc()
}

# Save all results together
pins::pin_write(
  BoardLocal,
  results_list,
  "WorkFlowTreesTuned",
  type = "qs2",
  title = "Work Flow Trees Tuned"
)


print("All tree-based models completed!")
