#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom data.table :=
#' @importFrom data.table .N
#' @importFrom data.table %between%
#' @importFrom data.table %chin%
#' @importFrom data.table %like%
#' @importFrom data.table as.data.table
#' @importFrom data.table copy
#' @importFrom data.table data.table
#' @importFrom data.table fcase
#' @importFrom data.table fifelse
#' @importFrom data.table is.data.table
#' @importFrom data.table rbindlist
#' @importFrom data.table setattr
#' @importFrom data.table setDT
#' @importFrom data.table setkeyv
#' @importFrom data.table setnames
#' @importFrom data.table uniqueN
#' @importFrom DBI dbConnect
#' @importFrom DBI dbDisconnect
#' @importFrom DBI dbGetQuery
#' @importFrom duckdb duckdb
#' @importFrom future.apply future_lapply
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 geom_histogram
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_step
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 geom_tile
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 scale_fill_gradient
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_x_discrete
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 stat_ecdf
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 waiver
#' @importFrom glue glue
#' @importFrom glue glue_safe
#' @importFrom leaflet addCircleMarkers
#' @importFrom leaflet addLegend
#' @importFrom leaflet addProviderTiles
#' @importFrom leaflet addTiles
#' @importFrom leaflet colorFactor
#' @importFrom leaflet leaflet
#' @importFrom leaflet markerClusterOptions
#' @importFrom lubridate as_datetime
#' @importFrom lubridate cyclic_encoding
#' @importFrom lubridate hours
#' @importFrom lubridate make_datetime
#' @importFrom lubridate minutes
#' @importFrom lubridate month
#' @importFrom lubridate year
#' @importFrom maptiles get_tiles
#' @importFrom patchwork plot_annotation
#' @importFrom patchwork plot_layout
#' @importFrom recipes add_step
#' @importFrom recipes bake
#' @importFrom recipes prep
#' @importFrom recipes print_step
#' @importFrom recipes rand_id
#' @importFrom recipes recipes_eval_select
#' @importFrom recipes step
#' @importFrom recipes tidy
#' @importFrom rlang abort
#' @importFrom rlang enquo
#' @importFrom rlang enquos
#' @importFrom rlang quo_name
#' @importFrom scales comma
#' @importFrom scales comma_format
#' @importFrom scales percent
#' @importFrom sf st_polygon
#' @importFrom sf st_sfc
#' @importFrom stats median
#' @importFrom stats quantile
#' @importFrom stats reorder
#' @importFrom stats sd
#' @importFrom tibble as_tibble
#' @importFrom timeDate listHolidays
#' @importFrom tmap tm_basemap
#' @importFrom tmap tm_borders
#' @importFrom tmap tm_shape
#' @importFrom utils tail
#' @importFrom withr local_seed
## usethis namespace: end
NULL



# Solving Global Variables problem

utils::globalVariables(c(

  ## From plot_map
  "color_variable",

  ## From simulate_trips
  "trip_id",
  "hvfhs_license_num",
  "wav_match_flag",
  "PULocationID",
  "DOLocationID",
  "request_datetime",
  "dropoff_datetime",
  "driver_pay",
  "tips",

  # From add_take_current_trip
  ".",
  ".SD",
  "wav_request_flag",
  "percentile_75_performance",
  "performance_per_hour",
  "take_current_trip",
  "trip_miles_mean",
  "trip_time",
  "waiting_secs",
  
  # plot_bar
  "n_trips",
  "is_top",
  "cat_summary",
  
  # plot_heap_map
  "N",
  "cat1",
  "cat2",
  "prop"
  

))
