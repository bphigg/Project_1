########################
#### Part 1 ###########
######################

library(tidyverse)
library(readr)

library(readr)
sheet1 <- read_csv("https://www4.stat.ncsu.edu/~online/datasets/EDU01a.csv")

sheet1 <- sheet1 %>% 
  select(Area_name, STCOU, ends_with("D")) %>%
  rename("area_name" = Area_name)

sheet1 <- sheet1 %>%
  pivot_longer(cols=3:12, names_to="edu_code", values_to = "enrollment")

sheet1 <- sheet1 %>%
  mutate(edu_code=substr(sheet1$edu_code, 1, 9)) %>%
  separate(edu_code, into=c("edu_code", "yr"), sep=7, remove = TRUE, convert = FALSE)

sheet1 <- sheet1 %>%
  mutate(yr = if_else(sheet1$yr > 50, as.numeric(paste0("19", sheet1$yr, sep="")), as.numeric(paste0("20", sheet1$yr, sep=""))))

county_df <- sheet1[grep(pattern = ", \\w\\w", sheet1$area_name), ]
class(county_df) <- c("county", class(county_df))
state_df <- sheet1[-c(grep(patter = ", \\w\\w", sheet1$area_name)), ]
class(state_df) <- c("state", class(state_df))

county_df <- county_df %>%
  mutate("state" = substr(county_df$area_name, nchar(county_df$area_name)-1, nchar(county_df$area_name)))

new_england <- c("CONNECTICUT", "MAINE", "MASSACHUSETTS", "NEW HAMPSHIRE", "RHODE ISLAND", "VERMONT")
mid_atlantic <- c("NEW JERSEY", "NEW YORK", "PENNSYLVANIA")
east_north_central <- c("ILLINOIS", "INDIANA", "MICHIGAN", "OHIO", "WISCONSIN")
west_north_central <- c("IOWA", "KANSAS", "MINNESOTA", "MISSOURI", "NEBRASKA", "NORTH DAKOTA", "SOUTH DAKOTA")
south_atlantic <- c("DELAWARE", "FLORIDA", "GEORGIA", "MARYLAND", "NORTH CAROLINA", "SOUTH CAROLINA", "VIRGINIA", "DISTRICT OF COLUMBIA", "WEST VIRGINIA", "DISTRICT OF COLUMBIA")
east_south_central <- c("ALABAMA", "KENTUCKY", "MISSISSIPPI", "TENNESSEE")
west_south_central <- c("ARKANSAS", "LOUISIANA", "OKLAHOMA", "TEXAS")
mountain <- c("ARIZONA", "COLORADO", "IDAHO", "MONTANA", "NEVADA", "NEW MEXICO", "UTAH", "WYOMING")
pacific <- c("ALASKA", "CALIFORNIA", "HAWAII", "OREGON", "WASHINGTON")
# created division column using if/else logic
state_df <- state_df %>%
  mutate("division" = if_else(area_name %in% new_england, "New England", 
              if_else(area_name %in% mid_atlantic, "Mid Atlantic", 
              if_else(area_name %in% east_north_central, "East North Central", 
              if_else(area_name %in% west_north_central, "West North Central", 
              if_else(area_name %in% south_atlantic, "South Atlantic", 
              if_else(area_name %in% east_south_central, "East South Central", 
              if_else(area_name %in% west_south_central, "West South Central", 
              if_else(area_name %in% mountain, "Mountain", 
              if_else(area_name %in% pacific, "Pacific", "ERROR"))))))))))

#####################
#### Part 2 ########
####################

sheet1 <- read_csv("https://www4.stat.ncsu.edu/~online/datasets/EDU01b.csv")
# 1
select_and_pivot <- function(sheet, var_name = "population") {
  df <- sheet %>%
    select(Area_name, STCOU, ends_with("D")) %>%
    rename("area_name" = Area_name) %>%
    pivot_longer(cols=3:12, names_to="census_code", values_to = var_name)
  return(df)
}

# 2
create_code_yr <- function(sheet) {
  dfa <- sheet %>% 
    mutate(census_code=substr(sheet$census_code, 1, 9)) %>%
    separate(census_code, into=c("census_code", "yr"), sep=7, remove = TRUE, convert = FALSE)
  dfb <- dfa %>%
    mutate(yr = if_else(dfa$yr > 50, as.numeric(paste0("19", dfa$yr, sep="")), as.numeric(paste0("20", dfa$yr, sep=""))))
  return(dfb)
}

#3
create_state <- function(sheet) {
  df <- sheet %>%
    mutate("state" = substr(sheet$area_name, nchar(sheet$area_name)-1, nchar(sheet$area_name)))
  return(df)
}
#4
create_division <- function(sheet) {
  df <- sheet %>%
    mutate("division" = if_else(area_name %in% new_england, "New England", 
            if_else(area_name %in% mid_atlantic, "Mid Atlantic", 
            if_else(area_name %in% east_north_central, "East North Central", 
            if_else(area_name %in% west_north_central, "West North Central", 
            if_else(area_name %in% south_atlantic, "South Atlantic", 
            if_else(area_name %in% east_south_central, "East South Central", 
            if_else(area_name %in% west_south_central, "West South Central", 
            if_else(area_name %in% mountain, "Mountain", 
            if_else(area_name %in% pacific, "Pacific", "ERROR"))))))))))
  return(df)
}

#5
split_county_state <- function(sheet) {
  county <- sheet[grep(pattern = ", \\w\\w", sheet$area_name), ]
  class(county) <- c("county", class(county))
  county <- create_state(county)
  state <- sheet[-c(grep(patter = ", \\w\\w", sheet$area_name)), ]
  state <- state %>%
    filter(area_name != "District of Columbia")
  class(state) <- c("state", class(state))
  state <- create_division(state)
  # return a list with two tibbles. Labeled each item in the list to access each tibble later with $ query syntax
  return(list(county = county, state = state))
}

#6 Wrapper Function
process_data <- function(url, var_name = "population") {
  a <- read_csv(url)
  b <- select_and_pivot(a, var_name)
  c <- create_code_yr(b)
  d <- split_county_state(c)
  return(d)
}

#7 Combine Function
combine_data <- function(data_1, data_2) {
  a <- bind_rows(data_1$county, data_2$county)
  b <- bind_rows(data_1$state, data_2$state)
  # Labeled each item in the list to access each tibble later with $ query syntax
  return(list(county = a,state = b))
}

##########################
### Part 3 Plot ######
########################

plot.state <- function(df, var_name = "population") {
  new_df <- df %>%
    group_by(division, yr) %>% 
    summarise(ENRmean = mean(get(var_name))) %>%
    filter(division != "ERROR")
  
  # plots a line graph of the mean var_name (population) by year for each regional division
  ggplot(new_df, aes(x = yr, y = ENRmean, color = division)) + 
    geom_line() + scale_y_continuous(labels = scales::comma) + ylab(var_name)
}

plot.county <- function(df, var_name = "population", st = "IL", order = "top", n = 5) {
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
  # the above function returns the highest or lowest means of var_name (population) grouped by county for the state entered in the argument.
  # the below function filters the original input dataframe based on the area_names of the above results, so that the new dataframe (filter_df) contains non-aggregated data for the counties identified in the above results
  new_df <- new_df[1:n, ]
  filter_df <- df %>%
    filter(area_name %in% new_df$area_name)
  
  # plots a line graph of the var_name (population) for each year for the top or bottom 'n' counties for the state entered in the function argument  
  ggplot(filter_df, aes(x = yr, y = get(var_name), color = area_name)) +
    geom_line() + scale_y_continuous(labels = scales::comma) + ylab(var_name)
}
