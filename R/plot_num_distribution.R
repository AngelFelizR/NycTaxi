
#' Plot Numerical Distribution with Multiple Views
#'
#' Creates a comprehensive visualization of numerical distributions using three
#' different plot types: a standard histogram, a log-transformed histogram, and
#' an empirical cumulative distribution function (ECDF) curve. The plots are
#' arranged vertically using patchwork.
#'
#' @param dt A data.table object containing the data to be plotted.
#' @param x The column name (unquoted) to be plotted on the x-axis.
#' @param fill Optional column name (unquoted) for grouping/coloring the data.
#'   Default is NULL.
#' @param manual_fill_values Optional character vector of custom colors for fill
#'   groups. Should match the number of groups in the fill variable. Default is NULL.
#' @param title Character string for the overall plot title. Default is empty string.
#' @param title_size Numeric value for the title font size. Default is 15.
#' @param hist_binwidth Numeric value for histogram bin width in the standard
#'   histogram. Default is NULL (automatic binning).
#' @param hist_n_break Integer for the number of breaks on the x-axis of the
#'   standard histogram. Default is NULL.
#' @param log_trans A transformation object for the log-transformed histogram.
#'   Default uses a signed log2 transformation that handles negative values.
#' @param log_binwidth Numeric value for histogram bin width in the log-transformed
#'   histogram. Default is NULL.
#' @param log_breaks Numeric vector specifying break points for the log-transformed
#'   x-axis. Default covers powers of 2 from -2^10 to 2^10, including 0.
#' @param curve_important_points Numeric vector of x-values to highlight on the
#'   ECDF curve with points and percentage labels. Default is NULL.
#' @param curve_nudge_y Numeric value to adjust the vertical position of percentage
#'   labels on important points. Default is 0.
#' @param curve_breaks_x Numeric vector specifying custom x-axis breaks for the
#'   ECDF plot. Default is NULL (automatic breaks).
#' @param curve_limits Numeric vector of length 2 specifying x-axis limits for
#'   the ECDF plot. Default is NULL (automatic limits).
#'
#' @return A patchwork object containing three ggplot2 plots arranged vertically:
#'   \itemize{
#'     \item Top: Standard histogram
#'     \item Middle: Log-transformed histogram
#'     \item Bottom: ECDF curve
#'   }
#'
#' @details
#' The function creates three complementary views of the same numerical distribution:
#' \itemize{
#'   \item \strong{Standard Histogram}: Shows the raw distribution of values
#'   \item \strong{Log-transformed Histogram}: Uses a signed log2 transformation
#'     to better visualize data with wide ranges or skewed distributions
#'   \item \strong{ECDF Curve}: Shows the cumulative proportion of observations
#'     below each value, useful for understanding percentiles
#' }
#'
#' When a fill variable is provided, the histograms show overlapping distributions
#' with transparency, while the ECDF shows separate curves for each group.
#'
#' The signed log2 transformation handles both positive and negative values using:
#' \code{sign(x) * log2(1 + abs(x))} for the forward transformation and
#' \code{sign(x) * (2^(abs(x)) - 1)} for the inverse.
#'
#' @examples
#' library(data.table)
#' library(ggplot2)
#' library(patchwork)
#' 
#' # Basic usage with single variable
#' dt <- data.table(values = rnorm(1000, mean = 100, sd = 50))
#' plot_num_distribution(dt, values, title = "Normal Distribution")
#' 
#' # With grouping variable
#' dt <- data.table(
#'   values = c(rnorm(500, 50, 20), rnorm(500, 150, 30)),
#'   group = rep(c("A", "B"), each = 500)
#' )
#' plot_num_distribution(dt, values, fill = group, 
#'                       title = "Distribution by Group",
#'                       manual_fill_values = c("red", "blue"))
#' 
#' # With important points highlighted on ECDF
#' plot_num_distribution(dt, values, 
#'                       curve_important_points = c(50, 100, 150),
#'                       title = "Distribution with Key Points")
#'
#' @seealso
#' \code{\link[ggplot2]{geom_histogram}}, \code{\link[ggplot2]{stat_ecdf}},
#' \code{\link[patchwork]{plot_annotation}}
#'
#' @export
plot_num_distribution <- function(dt,
                                  x,
                                  fill = NULL,
                                  manual_fill_values = NULL,
                                  title = "",
                                  title_size = 15,
                                  hist_binwidth = NULL,
                                  hist_n_break = NULL,
                                  log_trans =   scales::new_transform("signed_log2",
                                                                      transform = function(x) sign(x) * log2(1 + abs(x)),
                                                                      inverse = function(x) sign(x) * (2^(abs(x)) - 1),
                                                                      breaks = scales::pretty_breaks()),
                                  log_binwidth = NULL,
                                  log_breaks = c(-2^(0:10), 0, 2^(0:10)),
                                  curve_important_points = NULL,
                                  curve_nudge_y = 0,
                                  curve_breaks_x = NULL,
                                  curve_limits = NULL){
  
  # Lazy evaluation
  x = rlang::enquo(x)
  x_str <- rlang::quo_name(x)
  fill = rlang::enquo(fill)
  fill_str <- rlang::quo_name(fill)
  
  # Confirming correct format
  stopifnot("dt must be data.table" = data.table::is.data.table(dt))
  
  # Histogram 1 ----------------------------------------------------------------
  
  simple_hist =
    ggplot2::ggplot(dt, ggplot2::aes({{x}})) +
    ggplot2::geom_histogram(binwidth = hist_binwidth)
  
  if(fill_str != "NULL") {
    simple_hist =
      ggplot2::ggplot(dt, ggplot2::aes({{x}}, fill = {{fill}})) +
      ggplot2::geom_histogram(binwidth = log_binwidth,
                              alpha=0.5,
                              position="identity")
  }
  
  simple_hist = 
    simple_hist +
    ggplot2::scale_x_continuous(n.breaks = hist_n_break) +
    ggplot2::scale_y_continuous(labels = scales::comma_format()) +
    ggplot2::labs(x = "",
                  y = "Count") +
    ggplot2::theme_minimal()
  
  
  if(!is.null(manual_fill_values) && fill_str != "NULL") {
    simple_hist = 
      simple_hist +
      ggplot2::scale_fill_manual(values = manual_fill_values)
  }
  
  
  # Log Histogram --------------------------------------------------------------
  
  log_hist =
    ggplot2::ggplot(dt, ggplot2::aes({{x}}))+
    ggplot2::geom_histogram(binwidth = log_binwidth)
  
  if(fill_str != "NULL") {
    log_hist =
      ggplot2::ggplot(dt, ggplot2::aes({{x}}, fill = {{fill}}))+
      ggplot2::geom_histogram(binwidth = log_binwidth,
                              alpha=0.5,
                              position="identity") +
      theme(legend.position = "none")
  }
  
  log_hist =
    log_hist +
    ggplot2::scale_y_continuous(labels = scales::comma_format()) +
    ggplot2::scale_x_continuous(n.breaks = hist_n_break,
                                transform = log_trans,
                                breaks = log_breaks,
                                labels = \(x) round(x, 2)) +
    ggplot2::labs(x = "",
                  y = "Count") +
    ggplot2::theme_minimal()
  
  if(!is.null(manual_fill_values) && fill_str != "NULL") {
    log_hist = 
      log_hist +
      ggplot2::scale_fill_manual(values = manual_fill_values)
  }
  
  
  # ECDF Curve ----------------------------------------------------------------
  
  if(fill_str == "NULL") {
    
    get_prop = stats::ecdf(dt[[x_str]])
    
    ecdf_plot =
      ggplot2::ggplot(dt, ggplot2::aes({{x}}, get_prop({{x}}))) +
      ggplot2::geom_step(show.legend = FALSE)
    
    
    if(!is.null(curve_important_points)){
      
      important_df = data.table::data.table(x = curve_important_points)
      setnames(important_df, "x", x_str)
      
      
      ecdf_plot =
        ecdf_plot +
        ggplot2::geom_point(data = important_df) +
        ggplot2::geom_text(data = important_df,
                           ggplot2::aes(label = scales::percent(get_prop(!!x), accuracy = 1)),
                           nudge_y = curve_nudge_y)
    }
    
  }else{
    
    ecdf_plot =
      ggplot2::ggplot(dt, ggplot2::aes({{x}},
                                       color = {{fill}},
                                       group = {{fill}})) +
      ggplot2::stat_ecdf(show.legend = FALSE)
    
  }
  
  ecdf_plot =
    ecdf_plot +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                                breaks = seq(0, 1, by = 0.25)) +
    ggplot2::scale_x_continuous(labels = scales::comma_format(accuracy = 1),
                                breaks = if(is.null(curve_breaks_x)) ggplot2::waiver() else curve_breaks_x,
                                limits = curve_limits) +
    ggplot2::labs(y = "Prop of trips",
                  x = "") +
    ggplot2::theme_minimal()
  
  
  if(!is.null(manual_fill_values) && fill_str != "NULL") {
    ecdf_plot = 
      ecdf_plot +
      ggplot2::scale_color_manual(values = manual_fill_values)
  }
  
  
  # Consolidating plots using patchwork
  
  (simple_hist / log_hist / ecdf_plot) +
    patchwork::plot_annotation(title = title) +
    patchwork::plot_layout(guides = "collect") & 
    ggplot2::theme(plot.title = element_text(face = "bold", size = title_size),
                   legend.position = "top")
  
}