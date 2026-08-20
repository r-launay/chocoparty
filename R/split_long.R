separate_longer <- function(data, col_long, other_cols, delim) {
  data_long <- data %>%
    select(col_long, other_cols) %>%
    separate_longer_delim(col_long, delim = delim)
}