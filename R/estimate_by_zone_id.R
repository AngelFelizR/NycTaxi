#' Estimate ACS Variables by Zone ID Using Area-Weighted Averages
#'
#' This function retrieves American Community Survey (ACS) data for specified variables
#' and geographic areas, then spatially joins the data with custom zone shapes to 
#' calculate area-weighted estimates for each zone. Variables are processed in parallel
#' for improved performance when multiple variables are requested.
#'
#' @param variables Character vector. ACS variable codes to retrieve (e.g., "B08013_001").
#' @param zone_shapes sf object. Spatial polygons representing the zones of interest.
#'   Must contain a unique identifier column specified by `zone_id_col`.
#' @param zone_id_col Character. Name of the column in `zone_shapes` that contains
#'   unique zone identifiers. Default is "LocationId".
#' @param year Numeric. Year for ACS data retrieval. Default is 2020.
#' @param state Character. State abbreviation for ACS data retrieval. Default is "NY".
#' @param county Character vector. County names for ACS data retrieval. 
#'   Default is c("New York", "Kings", "Queens") for Manhattan, Brooklyn, and Queens.
#' @param survey Character. ACS survey type. Default is "acs5" for 5-year estimates.
#' @param remove_na Logical. Whether to remove tracts with missing estimates. Default is TRUE.
#' @param cache_dir Character. Optional directory path for caching intermediate results.
#'   If NULL, no caching is performed.
#'
#' @return A data.table with zone identifiers and area-weighted estimates for each variable.
#'   The output is in wide format with one column per variable.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Processes each variable in parallel using \code{future.apply::future_lapply()}
#'   \item For each variable: retrieves ACS tract-level data with geometry
#'   \item Transforms coordinate reference system to match the zone shapes
#'   \item Performs spatial joins to identify tracts within and overlapping zones
#'   \item Clips overlapping tracts to zone boundaries using spatial intersection
#'   \item Calculates area-weighted averages for each zone using tract areas as weights
#'   \item Combines results from all variables in wide format with one row per zone
#' }
#'
#' @note
#' \itemize{
#'   \item Requires packages: tidycensus, sf, data.table, future.apply
#'   \item Census API key must be configured for tidycensus
#'   \item Zone shapes must have a valid CRS with EPSG code
#'   \item Function assumes tract-level geography for ACS data
#'   \item Uses parallel processing for multiple variables via future.apply
#'   \item Set up parallel backend with future::plan() before calling function
#' }
#'
#' @examples
#' \dontrun{
#' # Set up parallel processing
#' library(future)
#' plan(multisession, workers = 4)
#' 
#' # Example zone shapes (replace with your actual data)
#' zones <- sf::st_read("path/to/zone_shapes.shp")
#' 
#' # Single variable
#' travel_time_estimates <- estimate_by_zone_id(
#'   variables = "B08013_001",
#'   zone_shapes = zones,
#'   year = 2020
#' )
#' 
#' # Multiple variables with parallel processing
#' transport_estimates <- estimate_by_zone_id(
#'   variables = c("B08013_001", "B08301_001", "B08301_010"),
#'   zone_shapes = zones,
#'   zone_id_col = "LocationID",
#'   year = 2020,
#'   cache_dir = "cache/"
#' )
#' 
#' # Clean up parallel backend
#' plan(sequential)
#' }
#'
#' @seealso
#' \code{\link[tidycensus]{get_acs}}, \code{\link[sf]{st_join}}, 
#' \code{\link[sf]{st_intersection}}, \code{\link[data.table]{dcast}},
#' \code{\link[future.apply]{future_lapply}}, \code{\link[future]{plan}}
#'
#' @export
estimate_by_zone_id <- function(variables,
                                zone_shapes,
                                zone_id_col = "LocationId",
                                year = 2020,
                                state = "NY",
                                county = c("New York", "Kings", "Queens"),
                                survey = "acs5",
                                remove_na = TRUE) {
  
  # Input validation
  if (!inherits(zone_shapes, "sf")) {
    stop("zone_shapes must be an sf object")
  }
  
  if (!zone_id_col %in% names(zone_shapes)) {
    stop("zone_id_col '", zone_id_col, "' not found in zone_shapes")
  }
  
  if (length(variables) == 0) {
    stop("At least one variable must be specified")
  }
  
  if (length(missing_packages) > 0) {
    stop("Missing required packages: ", paste(missing_packages, collapse = ", "))
  }
  
  # Helper function for intersection by ID
  apply_intersection_by_id <- function(features, shapes, id_col) {
    
    intersection_by_id <- function(id, features, shapes, id_col) {
      features_sub <- features[features[[id_col]] == id, ]
      shapes_sub_geo <- shapes[shapes[[id_col]] == id, ] |> sf::st_geometry()
      
      new_df <- 
        sf::st_intersection(features_sub, shapes_sub_geo) |>
        sf::st_cast(to = "MULTIPOLYGON")
      
      return(new_df)
    }
    
    # Use regular lapply for intersection by ID
    new_df <-
      lapply(unique(features[[id_col]]),
             FUN = intersection_by_id,
             features = features,
             shapes = shapes,
             id_col = id_col) |>
      do.call(what = "rbind")
    
    return(new_df)
  }
  
  # Function to process a single variable
  process_single_variable <- function(variable, 
                                      zone_shapes, 
                                      zone_id_col, 
                                      year, 
                                      state, 
                                      county, 
                                      survey, 
                                      remove_na) {
    
    # Step 1: Get data with geometry for single variable
    tract_data <-
      if(survey %like% "^acs"){
        tidycensus::get_acs(geography = "tract",
                            variables = variable,
                            year = year,
                            state = state,
                            county = county,
                            geometry = TRUE,
                            survey = survey)
      }else{
        tidycensus::get_decennial(geography = "tract",
                                  variables = variable,
                                  year = year,
                                  state = state,
                                  county = county,
                                  geometry = TRUE)
      }
    
    # Step 2: Transforming coordinate reference system
    tract_data <- sf::st_transform(tract_data, crs = sf::st_crs(zone_shapes)$epsg)
    
    # Step 3: Remove missing data if requested
    if (remove_na) {
      tract_data <- tract_data[!is.na(tract_data[["estimate"]]), ]
    }
    
    # Step 4: Spatial joins - within and overlaps
    tract_joined_list <-
      lapply(list(within = sf::st_within,
                  overlaps = sf::st_overlaps), 
             FUN = sf::st_join,
             x = tract_data,
             y = zone_shapes,
             join = spatial_fun,
             left = FALSE)
    
    # Step 5: Handle overlapping tracts by clipping to zone boundaries
    if (nrow(tract_joined_list$overlaps) > 0) {
      tract_joined_list$overlaps <-
        apply_intersection_by_id(tract_joined_list$overlaps,
                                 zone_shapes,
                                 id_col = "OBJECTID")
    }
    
    # Step 6: Combine within and overlaps data
    tract_joined <- do.call("rbind", tract_joined_list)
    
    # Step 7: Calculate area-weighted estimates
    tract_joined$tract_area <- sf::st_area(tract_joined$geometry)
    
    # Convert to data.table for efficient processing
    data.table::setDT(tract_joined)
    
    # Calculate weighted estimates by zone
    estimates_by_zone <-
      tract_joined[
        j = .(estimate = sum(estimate * 
                               as.double(tract_area / 
                                           sum(tract_area, na.rm = TRUE)),
                             na.rm = TRUE)),
        by = c(zone_id_col, "variable")
      ]
    
    return(estimates_by_zone)
  }
  
  # Process all variables in parallel
  message("Processing ", length(variables), " variable(s) in parallel...")
  
  all_estimates <- 
    future.apply::future_lapply(variables,
                                FUN = process_single_variable,
                                zone_shapes = zone_shapes,
                                zone_id_col = zone_id_col,
                                year = year,
                                state = state,
                                county = county,
                                survey = survey,
                                remove_na = remove_na,
                                future.seed = TRUE)
  
  # Combine results from all variables
  combined_estimates <- data.table::rbindlist(all_estimates)
  
  # Convert to wide format
  estimates_wide <- 
    data.table::dcast(combined_estimates, 
                      as.formula(paste0(zone_id_col," ~ variable")), 
                      value.var = "estimate")
  
  message("Successfully processed ", nrow(estimates_wide), " zones for ", 
          length(variables), " variable(s)")
  
  return(estimates_wide)
}