clean_choco <- function(data) {
  data %>%
    separate_wider_delim(ingredients, delim = "-", names = c("number_ing", "ingredients")) %>%
    mutate(number_ing = as.numeric(number_ing)) %>%
    mutate(ingredients = str_trim(ingredients)) %>%
    mutate(cocoa_percent = as.numeric(gsub('%', '', cocoa_percent)))
}

