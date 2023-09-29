###########################
#### Part 4 Plot ##########
##########################

plot.state <- function(df, var_name = "enrollment") {
  new_df <- df %>%
    group_by(division, yr) %>% 
    summarise(ENRmean = mean(get(var_name))) %>%
    filter(division != "ERROR")
  
  ggplot(new_df, aes(x = yr, y = ENRmean, color = division)) + 
    geom_line()
}

plot.county <- function(df, var_name = "enrollment", st = "NC", order = "top", n = "5") {
  if(order == "top"){
  new_df <- df %>%
    filter(state == st) %>%
    group_by(area_name) %>%
    summarise(ENRmean = mean(get(var_name))) %>%
    arrange(desc(ENRmean))
  }
  else if(order == "bottom"){
    new_df <- df %>%
      filter(state == st) %>%
      group_by(area_name) %>%
      summarise(ENRmean = mean(get(var_name))) %>%
      arrange(ENRmean)
  }
  else{
    stop("Specify Order (top or bottom)")
  }

  new_df <- new_df[1:n, ]
  filter_df <- df %>%
    filter(area_name %in% new_df$area_name)
  
  ggplot(filter_df, aes(x = yr, y = get(var_name), color = area_name)) +
    geom_line()
}

create_county <- function(df, var_name = "enrollment", st = "NC", n = "5") {
  new_df <- df %>%
    filter(state == st) %>%
    group_by(area_name) %>%
    summarise(ENRmean = mean(get(var_name))) %>%
    arrange(ENRmean)
  
  new_df <- new_df[1:n, ]
  filter_df <- df %>%
    filter(area_name %in% new_df$area_name)
  return(filter_df)
}
