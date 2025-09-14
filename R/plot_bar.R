#' Create a bar plot highlighting top categories
#'
#' @description
#' Creates a horizontal bar plot of counts by a categorical variable, highlighting
#' the top n categories with a distinct color.
#'
#' @param dt A data.table object containing the data to be plotted
#' @param var_name Character string specifying the column name in `dt` to use for categorization
#' @param color_highlight Character string specifying the color for the top categories. Default is "lightslateblue"
#' @param color_gray Character string specifying the color for non-top categories. Default is "gray80"
#' @param n_top Integer specifying the number of top categories to highlight. Default is 4
#'
#' @return A ggplot2 object showing the count of trips by category
#'
#' @details
#' The function calculates the count of observations for each unique value in the specified
#' variable, then creates a horizontal bar chart with the top `n_top` categories highlighted
#' in the specified highlight color. All other categories are displayed in the gray color.
#' The bars are sorted in descending order by count.
#'
#'
#' @export

plot_bar <- function(dt,
                     var_name,
                     color_highlight = "lightslateblue",
                     color_gray = "gray80",
                     n_top = 4L){
  
  dt_summary = dt[ 
    j = .(is_top = FALSE,
          n_trips = .N),
    by = var_name
  ]
  
  data.table::setnames(dt_summary, var_name, "cat_summary")
  
  if(n_top > 0L){
    dt_summary[order(-n_trips)[seq_len(n_top)], 
               is_top := TRUE]
  }
  
  
  ggplot2::ggplot(dt_summary,
                  aes(n_trips, factor(cat_summary, sort(cat_summary, decreasing = TRUE)))) +
    ggplot2::geom_col(aes(fill = is_top),
                      color = "black",
                      width = 0.8) +
    ggplot2::scale_fill_manual(values = c("TRUE" = color_highlight, 
                                          "FALSE" = color_gray))+
    ggplot2::scale_x_continuous(labels = scales::comma_format()) +
    ggplot2::labs(y = "",
                  x = "Number of Trips") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(face = "bold", size = 15))
  
}