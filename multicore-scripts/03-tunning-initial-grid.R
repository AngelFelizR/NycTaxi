# Importing ----

## Libraries -----

library(tidymodels)
library(baguette)
library(rules)
library(future)
library(here)
library(pins)
library(qs2)

## Pin boards to use ----

BoardRemote <- board_url(
  "https://raw.githubusercontent.com/AngelFelizR/NycTaxiPins/refs/heads/main/Board/",
  cache = here("../NycTaxiBoardCache")
)
BoardLocal <- board_folder(here("../NycTaxiPins/Board"))


## Formated data ----

TrainingSampleNormalized <- pin_read(BoardRemote, "TrainingSampleNormalized")
TrainingSampleForTrees <- pin_read(BoardRemote, "TrainingSampleForTrees")


# Defining options -----

## Resample to use -----

set.seed(5878)
TrainingSampleNormalizedResamples <- vfold_cv(TrainingSampleNormalized, v = 5)

set.seed(1245)
TrainingSampleForTreesResamples <- vfold_cv(TrainingSampleForTrees, v = 5)


## Metrics to eval -----

MetricsToEval <- metric_set(roc_auc, brier_class)


# Defining workflow ----

## Models ----

# Logistic regression via glmnet
GlmnetSpec <-
  logistic_reg(penalty = tune(), mixture = tune()) |>
  set_mode("classification") |>
  set_engine("glmnet")

# Bagged trees via rpart
RpartBagSpec <-
  bag_tree(tree_depth = tune(), min_n = tune(), cost_complexity = tune()) |>
  set_mode("classification") |>
  set_engine("rpart")

# Random forests via ranger
RangerSpec <-
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) |>
  set_mode("classification") |>
  set_engine("ranger")

# Boosted trees via xgboost
XgboostSpec <-
  boost_tree(
    trees = tune(),
    min_n = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    loss_reduction = tune(),
    sample_size = tune()
  ) |>
  set_mode("classification") |>
  set_engine("xgboost")

# RuleFit models via xrf
XrfSpec <-
  rule_fit(
    mtry = tune(),
    trees = tune(),
    min_n = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    loss_reduction = tune(),
    sample_size = tune(),
    penalty = tune()
  ) |>
  set_mode("classification") |>
  set_engine("xrf")

# We are not using SVM models as the training time is too high for this initial tuning phase.
# A focused tuning could be performed later if other models are not satisfactory.
# Polynomial support vector machines (SVMs) via kernlab
# KernlabPolySpec <-
#   svm_poly(cost = tune(), degree = tune(), scale_factor = tune()) |>
#   set_mode("classification") |>
#   set_engine("kernlab")

# Radial basis function support vector machines (SVMs) via kernlab
# KernlabRbfSpec <-
#   svm_rbf(cost = tune(), rbf_sigma = tune()) |>
#   set_mode("classification") |>
#   set_engine("kernlab")

## Recipes ----

start_recipe <- function(df) {
  new_recipe =
    recipe(take_current_trip ~ ., data = df) |>
    update_role(
      trip_id,
      performance_per_hour,
      percentile_75_performance,
      new_role = "additional info"
    )
}

NormalizedCorrRecipe <-
  start_recipe(TrainingSampleNormalized) |>
  step_zv(all_numeric_predictors()) |>
  step_corr(all_numeric_predictors(), threshold = tune())

NormalizedPcaRecipe <-
  start_recipe(TrainingSampleNormalized) |>
  step_zv(all_numeric_predictors()) |>
  step_pca(all_numeric_predictors(), num_comp = tune()) |>
  step_normalize(all_numeric_predictors())

NormalizedPlsRecipe <-
  start_recipe(TrainingSampleNormalized) |>
  step_zv(all_numeric_predictors()) |>
  step_pls(
    all_numeric_predictors(),
    outcome = "take_current_trip",
    num_comp = tune()
  ) |>
  step_normalize(all_numeric_predictors())

BasicTreeRecipe <-
  start_recipe(TrainingSampleForTrees) |>
  step_zv(all_predictors()) |>
  step_nzv(all_predictors())

XgboostRecipe <-
  BasicTreeRecipe |>
  step_other(all_nominal_predictors(), threshold = tune()) |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
  step_nzv(all_predictors())

XrfRecipe <-
  XgboostRecipe |>
  step_normalize(all_numeric_predictors())


## Consolidate workflow ----

NormalizedWorkFlowToTune <- workflow_set(
  preproc = list(
    rm_corr = NormalizedCorrRecipe,
    pca = NormalizedPcaRecipe,
    pls = NormalizedPlsRecipe
  ),
  models = list(
    logistic = GlmnetSpec #,
    # svm_rbf = KernlabRbfSpec,
    # svm_poly = KernlabPolySpec
  )
)

TreeWorkFlowsToTune <- bind_rows(
  workflow_set(
    preproc = list(reduce_levels = BasicTreeRecipe),
    models = list(bag_tree = RpartBagSpec, random_forest = RangerSpec)
  ),
  as_workflow_set(
    xgboost = workflow(
      preprocessor = XgboostRecipe,
      spec = XgboostSpec
    ),
    rulefit = workflow(
      preprocessor = XrfRecipe,
      spec = XrfSpec
    )
  )
)


# Saving the outputs ----

NormalizedWorkFlowTuned <- NormalizedWorkFlowToTune
TreeWorkFlowsTuned <- TreeWorkFlowsToTune

for (flow_i in c(
  NormalizedWorkFlowToTune$wflow_id,
  TreeWorkFlowsToTune$wflow_id
)) {
  is_normalized_recipe = flow_i %in% NormalizedWorkFlowToTune$wflow_id

  if (is_normalized_recipe) {
    wf_i = extract_workflow(NormalizedWorkFlowToTune, id = flow_i)
    resample_i = TrainingSampleNormalizedResamples
  } else {
    wf_i = extract_workflow(TreeWorkFlowsToTune, id = flow_i)
    resample_i = TrainingSampleForTreesResamples
  }

  wf_param_i = extract_parameter_set_dials(wf_i)

  # Now we need to set the ranges to tune

  if (flow_i == "rm_corr_logistic") {
    # Prevent the correlation threshold from being too low
    wf_param_i <- update(wf_param_i, threshold = threshold(range = c(0.7, 1.0)))
  } else if (flow_i %in% c("pca_logistic", "pls_logistic")) {
    # Ensure we always have at least 2 components
    wf_param_i <- update(wf_param_i, num_comp = num_comp(range = c(2L, 15L)))
  } else if (flow_i == "reduce_levels_bag_tree") {
    # Restrict min_n to a safer range to avoid rpart failures
    wf_param_i <- update(wf_param_i, min_n = min_n(range = c(2L, 25L)))
  } else if (flow_i == "reduce_levels_random_forest") {
    # Your original code for random_forest (it's perfect)
    wf_param_i <- update(
      wf_param_i,
      mtry = mtry(range = c(1L, ncol(TrainingSampleForTrees) - 1L))
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

  print(paste("tunning", flow_i))

  plan(multicore, workers = 3)

  tunning_results =
    tune_grid(
      object = wf_i,
      resamples = resample_i,
      param_info = wf_param_i,
      grid = initial_grid,
      metrics = MetricsToEval,
      control = control_grid(verbose = TRUE, parallel_over = "resamples")
    )

  plan(sequential)

  print(paste("saving results for", flow_i))

  if (is_normalized_recipe) {
    NormalizedWorkFlowTuned =
      NormalizedWorkFlowTuned |>
      option_add(
        param_info = wf_param_i,
        initial = tunning_results,
        id = flow_i
      )

    pin_write(
      BoardLocal,
      NormalizedWorkFlowTuned,
      "NormalizedWorkFlowTuned",
      type = "qs2",
      title = "Tuned Normalized Work Flow"
    )
  } else {
    TreeWorkFlowsTuned =
      TreeWorkFlowsTuned |>
      option_add(
        param_info = wf_param_i,
        initial = tunning_results,
        id = flow_i
      )
    pin_write(
      BoardLocal,
      TreeWorkFlowsTuned,
      "TreeWorkFlowsTuned",
      type = "qs2",
      title = "Tuned Tree Work Flow"
    )
  }

  print(paste("saved results for", flow_i))
}
