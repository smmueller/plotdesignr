#' Get a yields to use in simulation
#' @param var_of_interest string; can be multiple
#' @return An sf data frame of the original point yields, converted to UTM
get_test_data <- function(path, file, var_of_interest){
  field <- st_read(paste0(path, file))
  field <- field %>% distinct(.keep_all = TRUE)

  field <- update_field_crs(field)
  field_sub <- field %>% select(var_of_interest)

  return(field_sub)
}
