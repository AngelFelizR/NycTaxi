
## Importing libraries

library(tidymodels)
library(baguette)
library(rules)
library(future)
library(here)


## Defining source data paths

TrainingSampleNormalizedFilePath <- here("output/cache-data/09-data-preparation/TrainingSampleNormalized.qs")
TrainingSampleForTreesFilePath <- here("output/cache-data/09-data-preparation/TrainingSampleForTrees.qs")
NormalizedWorkFlowToTuneFilePath <- here("output/cache-data/09-data-preparation/NormalizedWorkFlowToTune.qs")
TreeWorkFlowsToTuneFilePath <- here("output/cache-data/09-data-preparation/TreeWorkFlowsToTune.qs")
NormalizedWorkFlowTunedFilePath <- here("output/cache-data/09-data-preparation/NormalizedWorkFlowTuned.qs")
TreeWorkFlowsTunedFilePath <- here("output/cache-data/09-data-preparation/TreeWorkFlowsTuned.qs")


## Importing source data

TrainingSampleNormalized <- qs2::qs_read(TrainingSampleNormalizedFilePath)
TrainingSampleForTrees <- qs2::qs_read(TrainingSampleForTreesFilePath)
NormalizedWorkFlowToTune <- qs2::qs_read(NormalizedWorkFlowToTuneFilePath)
TreeWorkFlowsToTune <- qs2::qs_read(TreeWorkFlowsToTuneFilePath)

## Defining resamples

set.seed(5878)
TrainingSampleNormalizedResamples <- vfold_cv(TrainingSampleNormalized, v = 10)

set.seed(1245)
TrainingSampleForTreesResamples <- vfold_cv(TrainingSampleForTrees, v = 10)


## Defining metrics to use

MetricsToEval <- metric_set(roc_auc, brier_class)


## Saving the outputs

NormalizedWorkFlowTuned <- NormalizedWorkFlowToTune
TreeWorkFlowsTuned <- TreeWorkFlowsToTune

for(flow_i in c(NormalizedWorkFlowToTune$wflow_id,
                TreeWorkFlowsToTune$wflow_id)) {
  
  is_normalized_recipe = flow_i %in% NormalizedWorkFlowToTune$wflow_id
  
  if(is_normalized_recipe){
    wf_i = extract_workflow(NormalizedWorkFlowToTune, id = flow_i)
    resample_i = TrainingSampleNormalizedResamples
  }else{
    wf_i = extract_workflow(TreeWorkFlowsToTune, id = flow_i)
    resample_i = TrainingSampleForTreesResamples
  }
  
  wf_param_i = extract_parameter_set_dials(wf_i)
  
  if(flow_i == "reduce_levels_random_forest"){
    wf_param_i =
    update(wf_param_i,
           mtry = mtry(c(1, ncol(TrainingSampleForTrees))))
  }
  
  set.seed(14005)
  initial_grid <- 
    grid_space_filling(wf_param_i,
                       size = 15, 
                       type = "audze_eglais",
                       original = FALSE)
  
  print(paste("tunning", flow_i))
  
  plan(multicore, workers = 3)
  
  tunning_results =
    tune_grid(object = wf_i,
              resamples = resample_i,
              param_info = wf_param_i,
              grid = initial_grid,
              metrics = MetricsToEval,
              control = control_grid(verbose = TRUE, parallel_over = "resamples"))
  
  plan(sequential)
  
  print(paste("saving results for", flow_i))
  
  if(is_normalized_recipe){
    NormalizedWorkFlowTuned =
      NormalizedWorkFlowTuned |>
      option_add(param_info = wf_param_i,
                 initial = tunning_results,
                 id = flow_i)
    
    qs2::qs_save(NormalizedWorkFlowTuned, NormalizedWorkFlowTunedFilePath)
    
  }else{
    TreeWorkFlowsTuned =
      TreeWorkFlowsTuned |>
      option_add(param_info = wf_param_i,
                 initial = tunning_results,
                 id = flow_i)
    qs2::qs_save(TreeWorkFlowsTuned, TreeWorkFlowsTunedFilePath)
  }
  
  print(paste("saved results for", flow_i))
  
}


