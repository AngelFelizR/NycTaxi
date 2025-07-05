#' Join Geospatial Features to Recipe Data
#'
#' @description
#' `step_join_geospatial_features` creates a specification of a recipe step that
#' joins geospatial features to the data based on specified columns. This step
#' allows you to enrich your dataset with spatial information by joining on
#' matching geographical identifiers.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose variables from the data
#'   to use for joining. See [recipes::selections()] for more details.
#' @param role For model terms created by this step, what analysis role should
#'   they be assigned? By default, the new columns created by this step from
#'   the original variables will be assigned the same role as the original
#'   column. If the original variable has role `NA`, then `predictor` is used.
#' @param trained A logical to indicate if the quantities for preprocessing
#'   have been estimated.
#' @param spatial_features A data frame containing the geospatial features to
#'   join with the recipe data. This should contain the spatial variables and
#'   the joining columns.
#' @param col_pattern A character vector of patterns to match column names.
#'   If specified, columns matching these patterns will be used to create
#'   the join relationship. This is useful when you have prefixed column names
#'   that need to be matched without the prefix.
#' @param skip A logical. Should the step be skipped when the recipe is baked
#'   by [recipes::bake()]? While all operations are baked when [recipes::prep()]
#'   is run, some operations may not be able to be conducted on new data
#'   (e.g. processing the outcome variable(s)). Care should be taken when
#'   using `skip = TRUE` as it may affect the computations for subsequent
#'   operations.
#' @param id A character string that is unique to this step to identify it.
#'
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of any existing operations.
#'
#' @details
#' This step performs a join operation between the recipe data and provided
#' geospatial features. The join is based on matching column names or patterns.
#' 
#' When `col_pattern` is specified, the step will:
#' 1. Remove the specified patterns from column names to create the join keys
#' 2. Perform the join operation for each pattern
#' 3. Add prefixes to the resulting spatial feature columns
#' 
#' When `col_pattern` is NULL, a direct join is performed using the specified
#' column names.
#'
#' @section Tidying:
#' When you [`tidy()`][generics::tidy()] this step, a tibble is returned with
#' columns `terms`, `spatial_features`, and `id`:
#' 
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{spatial_features}{character, names of the spatial features added}
#'   \item{id}{character, id of this step}
#' }
#'
#' @section Case weights:
#' This step performs an inner join. The case weights are not modified by this
#' step and are assumed to be based on the original data structure.
#'
#' @examples
#' # Create sample data
#' main_data <- data.frame(
#'   region_id = c("A", "B", "C"),
#'   value = c(10, 20, 30)
#' )
#' 
#' # Create spatial features
#' spatial_data <- data.frame(
#'   region_id = c("A", "B", "C"),
#'   latitude = c(40.7, 34.1, 41.9),
#'   longitude = c(-74.0, -118.2, -87.6),
#'   population = c(1000, 2000, 1500)
#' )
#' 
#' # Adding geospatial features
#' recipes::recipe(value ~ ., data = main_data) |>
#'   step_join_geospatial_features(
#'     region_id,
#'     spatial_features = spatial_data
#'   ) |>
#'   recipes::prep() |>
#'   recipes::bake(new_data = NULL)
#' 
#' 
#' # Expanding data start and end region id
#' main_data_prefix = main_data
#' main_data_prefix$region_id <- NULL
#' main_data_prefix$start_region_id <- main_data$region_id
#' main_data_prefix$end_region_id <- rev(main_data$region_id)
#' 
#' # Adding geospatial features
#' recipes::recipe(value ~ ., data = main_data_prefix) |>
#'   step_join_geospatial_features(
#'    ends_with("region_id"),
#'     spatial_features = spatial_data,
#'     col_pattern = c("start_", "end_")
#'   ) |>
#'   recipes::prep() |>
#'   recipes::bake(new_data = NULL)
#'
#' @export
step_join_geospatial_features <- function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    spatial_features,
    col_pattern = NULL,
    skip = FALSE,
    id = recipes::rand_id("join_geospatial_features")
) {
  recipes::add_step(
    recipe,
    step_join_geospatial_features_new(
      terms = rlang::enquos(...),
      trained = trained,
      role = role,
      spatial_features = spatial_features,
      col_pattern = col_pattern,
      skip = skip,
      id = id
    )
  )
}

#' @rdname step_join_geospatial_features
#' @export
step_join_geospatial_features_new <- function(
    terms,
    role,
    trained,
    spatial_features,
    col_pattern,
    skip,
    id
) {
  recipes::step(
    subclass = "join_geospatial_features",
    terms = terms,
    role = role,
    trained = trained,
    spatial_features = spatial_features,
    col_pattern = col_pattern,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_join_geospatial_features <- function(
    x,
    training,
    info = NULL,
    ...
) {
  
  # We need to confirm that the data can be joined
  # Before saving the data as an step
  
  col_to_join <- recipes::recipes_eval_select(x$terms, training, info)
  
  attr(x$spatial_features, "col_to_join") <- col_to_join
  
  if(!is.null(x$col_pattern)){
    
    col_to_join_clean <-
      gsub(pattern = paste(x$col_pattern, collapse = "|"),
           replacement = "",
           col_to_join) |>
      unique()
    
  } else {
    col_to_join_clean <- col_to_join
  }
  
  if (!any(col_to_join_clean %in% names(x$spatial_features))) {
    rlang::abort("The defined terms cannot be found in spatial_features. Confirm if you need to define some `col_pattern` to define the join relation to apply.")
  }
  
  step_join_geospatial_features_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    spatial_features = x$spatial_features,
    col_pattern = x$col_pattern,
    skip = x$skip,
    id = x$id
  )
  
}

#' @export
bake.step_join_geospatial_features <- function(
    object,
    new_data,
    ...
) {
  spatial_features <- object$spatial_features
  
  data.table::setDT(new_data)
  data.table::setDT(spatial_features)
  col_to_join <- attr(spatial_features, "col_to_join")
  
  if(is.null(object$col_pattern)){
    
    new_data <- spatial_features[new_data,
                                 on = col_to_join]
    
    new_data <- tibble::as_tibble(new_data)
    
    return(new_data)
    
  }
  
  old_names <- names(spatial_features)
  
  for(prefix_i in object$col_pattern){
    
    col_to_join_i <- 
      grep(pattern = prefix_i, 
           x = col_to_join, 
           value = TRUE)
    
    # Create a copy of spatial features with prefixed names
    spatial_copy <- data.table::copy(spatial_features)
    new_names <- paste0(prefix_i, old_names)
    data.table::setnames(spatial_copy, old_names, new_names)
    
    # Perform the join
    new_data <- spatial_copy[new_data,
                             on = col_to_join_i]
    
  }
  
  new_data <- tibble::as_tibble(new_data)
  
  return(new_data)
  
}

#' @export
print.step_join_geospatial_features <- function(
    x, 
    width = max(20, options()$width - 35), 
    ...
) {
  
  title <- "Adding spatial features "
  
  recipes::print_step(
    # Names after prep:
    tr_obj = if(x$trained) names(x$spatial_features) else NULL,
    # Names before prep (could be selectors)
    untr_obj = x$terms,
    # Has it been prepped?
    trained = x$trained,
    # What does this step do?
    title = title,
    # An estimate of how many characters to print on a line:
    width = width
  )
  invisible(x)
}

#' @rdname step_join_geospatial_features
#' @param x A `step_join_geospatial_features` object.
#' @export
tidy.step_join_geospatial_features <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble::tibble(
      terms = names(attr(x$spatial_features, "col_to_join")),
      spatial_features = names(x$spatial_features),
      id = x$id
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble::tibble(
      terms = term_names,
      spatial_features = NA_character_,
      id = x$id
    )
  }
  res
}