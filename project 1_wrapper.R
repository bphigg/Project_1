process_data <- function(url, var_name = "enrollment") {
  a <- read_csv(url)
  b <- select_and_pivot(a, var_name)
  c <- create_edu_yr(b)
  d <- split_county_state(c)
  return(d)
}

combine_data <- function(data_1, data_2) {
  a <- bind_rows(data_1[[1]], data_2[[1]])
  b <- bind_rows(data_1[[2]], data_2[[2]])
  return(list(a,b))
}

