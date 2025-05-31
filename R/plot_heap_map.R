#' Plot a heatmap of two categorical variables.
#'
#' This function takes a data.table and two categorical variables to create a
#' heatmap visualizing the counts and proportions of their combinations.
#'
#' @param dt A data.table containing the data.
#' @param cat_vars A character vector of length 2 specifying the names of the
#'   two categorical variables in `dt` to be plotted.
#' @param color_high The color to represent high counts in the heatmap.
#' @param color_low The color to represent low counts in the heatmap.
#' @param sep The separator string to use between the proportion (percentage)
#'   and the count in the text labels on the heatmap tiles. Defaults to " | ".
#'
#' @return A ggplot2 object representing the heatmap.
#' 
#' @export
plot_heap_map <- function(dt, 
                          cat_vars,
                          color_high = "lightslateblue",
                          color_low = "gray80",
                          sep = " | ") {
  
  stopifnot("cat_vars most be available in dt" = all(cat_vars %in% names(dt)))
  stopifnot("cat_vars must have 2 variables" = length(cat_vars) == 2L)
  
  dt_summary = dt[, .N, by = cat_vars]
  
  data.table::setnames(dt_summary, cat_vars, c("cat1", "cat2"))
  
  dt_summary[,`:=`(prop  = N / sum(N),
                   cat1 = stats::reorder(cat1, N),
                   cat2 = stats::reorder(cat2, N))] 
  
  
  ggplot2::ggplot(dt_summary, ggplot2::aes(cat1, cat2)) +
    ggplot2::geom_tile(ggplot2::aes(fill = N))+
    ggplot2::geom_text(ggplot2::aes(label = paste0(scales::percent(prop, accuracy = 1),
                                                   sep, 
                                                   scales::comma(N, accuracy = 1)))) +
    ggplot2::scale_fill_gradient(low = color_low, 
                                 high = color_high,
                                 labels = scales::comma)+
    ggplot2::scale_x_discrete(position = "top")+
    ggplot2::labs(title = cat_vars[2L],
                  x = "",
                  y = "") +
    ggplot2::theme_minimal()+
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(face = "bold", size = 15),
                   legend.position = "none") 
  
}