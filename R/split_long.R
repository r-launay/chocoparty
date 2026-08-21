#' Separate columns containing delimited info into several rows 
#'
#' @param data Data frame
#' @param col_long Name of the column to split into multiple rows
#' @param other_cols Name of the other columns you want to include in the output table
#' @param delim Delimiter
#'
#' @returns A long data frame 
#' @export
#'
#' @importFrom dplyr select
#' @importFrom tidyr separate_longer_delim
#'
#' @examples data(choco)
#' separate_longer(choco, "ingredients", "rating", "-")
separate_longer <- function(data, col_long, other_cols, delim) {
  data_long <- data |>
    select(col_long, all_of(other_cols)) |>
    separate_longer_delim(col_long, delim = delim)
}


