#' Clean the columns in the choco data
#'
#' @param data Choco data frame 
#'
#' @returns Clean choco data frame: number of ingredients separate from ingredients
#' list. Trim stings, numbers as numeric, ...
#' @export
#'
#' @examples data(choco)
#' clean_choco(choco)
clean_choco <- function(data) {
  data |>
    tidyr::separate_wider_delim(ingredients, delim = "-", names = c("number_ing", "ingredients")) |>
    dplyr::mutate(number_ing = as.numeric(number_ing)) |>
    dplyr::mutate(ingredients = stringr::str_trim(ingredients)) |>
    dplyr::mutate(cocoa_percent = as.numeric(gsub('%', '', cocoa_percent)))
}

