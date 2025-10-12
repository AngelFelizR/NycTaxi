# Importing ----

## Libraries -----

library(tidymodels)
library(embed)
library(discrim)
library(baguette)

library(future)


## Removing limit for parralell processing ----

options(future.globals.maxSize = +Inf)


## Pin boards to use ----

BoardRemote <- pins::board_url(
  "https://raw.githubusercontent.com/AngelFelizR/NycTaxiPins/refs/heads/main/Board/",
  cache = here::here("../NycTaxiBoardCache")
)


## Resample to use -----

TrainingSampleJoined <- pins::pin_read(BoardRemote, "TrainingSampleJoined")

set.seed(5878)
TrainingSampleJoinedResamples <- vfold_cv(TrainingSampleJoined, v = 5)

## Metrics to eval -----

MetricsToEval <- metric_set(roc_auc, brier_class)


## Workflow to eval ----

WorkFlowToTune <- qs2::qs_read(here("../NycTaxiBigFiles/WorkFlowToTune.qs"))


# Saving the outputs ----

WorkFlowToTuned <- WorkFlowToTune

for (flow_i in WorkFlowToTune$wflow_id) {
  wf_i = extract_workflow(WorkFlowToTune, id = flow_i)
  wf_param_i = extract_parameter_set_dials(wf_i)

  wf_param_i <- update(
    wf_param_i,
    threshold = threshold(range = c(0.7, 1.0)),
    num_comp = num_comp(range = c(2L, 50L)),
    min_n = min_n(range = c(2L, 25L)),
    mtry = mtry(range = c(1L, ncol(TrainingSampleJoined) - 1L))
  )

  set.seed(14005)
  initial_grid <-
    grid_space_filling(
      wf_param_i,
      size = 10,
      type = "audze_eglais",
      original = FALSE
    )

  print(paste("tunning", flow_i, "with grid:"))
  print(initial_grid)

  if (flow_i != "reduce_levels_bag_tree") {
    plan(multicore, workers = 3)
  }

  tunning_results =
    tune_grid(
      object = wf_i,
      resamples = resample_i,
      param_info = wf_param_i,
      grid = initial_grid,
      metrics = MetricsToEval,
      control = control_grid(
        verbose = TRUE,
        parallel_over = "resamples",
        save_pred = TRUE
      )
    )

  if (flow_i != "reduce_levels_bag_tree") {
    plan(sequential)
  }

  print(paste("saving results for", flow_i))

  qs2::qs_save(
    WorkFlowToTuned,
    here::here("../NycTaxiBigFiles/WorkFlowToTuned.qs")
  )

  print(paste("saved results for", flow_i))
}
