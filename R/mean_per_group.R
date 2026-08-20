mean_per_group <- function(data,colGroup,colMean){
  col_name <- paste0("mean_", colMean)
  new_data <- data %>% 
    group_by(!!sym(colGroup)) %>%
    summarise(!!as.symbol(col_name) := mean(!!sym(colMean),na.rm = TRUE)) %>%
    ungroup %>%
    arrange(desc(!!as.symbol(col_name)))
  
  new_data
}
