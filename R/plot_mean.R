#' Plot the top x value as a function of an other with horizontal bars
#'
#' @param data Data frame e.g. Group, Mean per group
#' @param colGroup Name of the column to group by
#' @param colMean Name of the mean column
#' @param n_top Number of rows you want to plot
#' @param image_path Path to the image for the points
#'
#' @returns A plot 
#' @export
#'
#' @examples data(choco)
#' choco_mean <- mean_per_group(choco, "company_location", "rating")
#' plot_mean(choco_mean, "company_location", "mean_rating", 5, "/Users/au761602/Downloads/canva-yellow-star-MAFXSVQWSuc.png.webp")
plot_mean <- function(data, colGroup, colMean, n_top, image_path) {
  
  data_top <- data |>
    dplyr::top_n(!!as.symbol(colMean), n = n_top) |>
    dplyr::mutate(image = image_path)
  
  print(data_top)
  
  ggplot2::ggplot(data_top, ggplot2::aes(x = !!as.symbol(colMean),
                                         y = forcats::fct_reorder(!!as.symbol(colGroup), !!as.symbol(colMean)))) +
    ggplot2::geom_bar(stat = "identity", fill = 'yellow3', alpha = 0.5, width = 0.1) +
    ggimage::geom_image(ggplot2::aes(image = image))+
    ggplot2::labs(
    x = paste0(stringr::str_to_title(colMean)), 
    y = paste0("Top ", n_top, " ", stringr::str_to_title(colGroup))
  ) +
    ggplot2::theme_classic() +
    ggplot2::xlim(0,4)
}

