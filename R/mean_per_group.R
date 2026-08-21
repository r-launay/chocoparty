#' Calculate the mean of a column for data grouped by another column
#'
#' @param data Data frame 
#' @param colGroup Name of the column you want to group by 
#' @param colMean Name of the column you want to calculate the mean of
#'
#' @returns A data frame with the group and the mean per group
#' @export
#'
#' @examples data(choco)
#' mean_per_group(choco, "company_location", "rating")
mean_per_group <- function(data,colGroup,colMean){
  col_name <- paste0("mean_", colMean)
  new_data <- data |> 
    dplyr::group_by(!!as.symbol(colGroup)) |>
    dplyr::summarise(!!as.symbol(col_name) := mean(!!as.symbol(colMean),na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::arrange(desc(!!as.symbol(col_name)))
  
  new_data
}
