# Importing ----

## Libraries -----

library(tidymodels)
library(embed)
library(discrim)
library(baguette)
library(future)


## Removing limit for parralell processing ----

options(future.globals.maxSize = 16.5 * 1e9) ## 16.5 GB


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

WorkFlowToTune <- qs2::qs_read(here::here(
  "../NycTaxiBigFiles/WorkFlowToTune.qs"
))


# Saving the outputs ----

WorkFlowToTuned <- WorkFlowToTune

for (wf_id_i in WorkFlowToTune$wflow_id) {
  wf_i <- extract_workflow(WorkFlowToTune, id = wf_id_i)
  wf_param_i <- extract_parameter_set_dials(wf_i)

  if (grepl("^rm_corr", wf_id_i)) {
    wf_param_i <- update(
      wf_param_i,
      threshold = threshold(range = c(0.7, 1.0))
    )
  }

  if (grepl("^(pca|pls|umap)_", wf_id_i)) {
    wf_param_i <- update(
      wf_param_i,
      num_comp = num_comp(range = c(2L, 50L))
    )
  }

  if (grepl("^reduced_levels_", wf_id_i)) {
    wf_param_i <- update(
      wf_param_i,
      min_n = min_n(range = c(2L, 25L))
    )
  }

  if (wf_id_i == "reduced_levels_random_forest") {
    wf_param_i <- update(
      wf_param_i,
      mtry = mtry(range = c(1L, ncol(TrainingSampleJoined) - 1L))
    )
  }

  set.seed(14005)
  initial_grid <-
    grid_space_filling(
      wf_param_i,
      size = 10,
      type = "audze_eglais",
      original = FALSE
    )

  print(paste("tunning", wf_id_i, "with grid:"))
  print(initial_grid)

  plan(multicore, workers = 3)

  tunning_results =
    tune_grid(
      object = wf_i,
      resamples = TrainingSampleJoinedResamples,
      param_info = wf_param_i,
      grid = initial_grid,
      metrics = MetricsToEval,
      control = control_grid(
        verbose = TRUE,
        parallel_over = "resamples",
        save_pred = TRUE
      )
    )

  plan(sequential)

  print(paste("saving results for", wf_id_i))

  qs2::qs_save(
    WorkFlowToTuned,
    here::here("../NycTaxiBigFiles/WorkFlowToTuned.qs")
  )

  print(paste("saved results for", wf_id_i))
}
