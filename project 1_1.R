########################
#### Part 1 ###########
######################

library(readr)
sheet1 <- read_csv("https://www4.stat.ncsu.edu/~online/datasets/EDU01a.csv")
sheet1 <- sheet1 %>% 
  select(Area_name, STCOU, ends_with("D")) %>%
  rename("area_name" = Area_name)
sheet1 <- sheet1 %>%
  pivot_longer(cols=3:12, names_to="edu", values_to = "enrollment")
sheet1 <- sheet1 %>%
  mutate(edu=substr(sheet1$edu, 1, 9)) %>%
  separate(edu, into=c("edu", "yr"), sep=7, remove = TRUE, convert = FALSE)
sheet1 <- sheet1 %>%
  mutate(yr = if_else(sheet1$yr > 50, as.numeric(paste0("19", sheet1$yr, sep="")), as.numeric(paste0("20", sheet1$yr, sep=""))))
county_df <- sheet1[grep(pattern = ", \\w\\w", sheet1$area_name), ]
state_df <- sheet1[-c(grep(patter = ", \\w\\w", sheet1$area_name)), ]
class(county_df) <- c("county", class(county_df))
class(state_df) <- c("state", class(state_df))
county_df <- county_df %>%
  mutate("state" = substr(county_df$area_name, nchar(county_df$area_name)-1, nchar(county_df$area_name)))
new_england <- c("CONNECTICUT", "MAINE", "MASSACHUSETTS", "NEW HAMPSHIRE", "RHODE ISLAND", "VERMONT")
mid_atlantic <- c("NEW JERSEY", "NEW YORK", "PENNSYLVANIA")
east_north_central <- c("ILLINOIS", "INDIANA", "MICHIGAN", "OHIO", "WISCONSIN")
west_north_central <- c("IOWA", "KANSAS", "MINNESOTA", "MISSOURI", "NEBRASKA", "NORTH DAKOTA", "SOUTH DAKOTA")
south_atlantic <- c("DELAWARE", "FLORIDA", "GEORGIA", "MARYLAND", "NORTH CAROLINA", "SOUTH CAROLINA", "VIRGINIA", "DISTRICT OF COLUMBIA", "WEST VIRGINIA", "District of Columbia")
east_south_central <- c("ALABAMA", "KENTUCKY", "MISSISSIPPI", "TENNESSEE")
west_south_central <- c("ARKANSAS", "LOUISIANA", "OKLAHOMA", "TEXAS")
mountain <- c("ARIZONA", "COLORADO", "IDAHO", "MONTANA", "NEVADA", "NEW MEXICO", "UTAH", "WYOMING")
pacific <- c("ALASKA", "CALIFORNIA", "HAWAII", "OREGON", "WASHINGTON")
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
select_and_pivot <- function(sheet1, var_name = "enrollment") {
  df <- sheet1 %>%
    select(Area_name, STCOU, ends_with("D")) %>%
    rename("area_name" = Area_name) %>%
    pivot_longer(cols=3:12, names_to="edu", values_to = var_name)
  return(df)
}
# 2
create_edu_yr <- function(sheet1) {
  dfa <- sheet1 %>% 
    mutate(edu=substr(sheet1$edu, 1, 9)) %>%
    separate(edu, into=c("edu_code", "yr"), sep=7, remove = TRUE, convert = FALSE)
  dfb <- dfa %>%
    mutate(yr = if_else(dfa$yr > 50, as.numeric(paste0("19", dfa$yr, sep="")), as.numeric(paste0("20", dfa$yr, sep=""))))
  return(dfb)
}
# mutate(year = as.numeric(paste0("19", dfa$yr, sep="")))
#3
create_state <- function(sheet1) {
  df <- sheet1 %>%
    mutate("state" = substr(sheet1$area_name, nchar(sheet1$area_name)-1, nchar(sheet1$area_name)))
  return(df)
}
#4
create_division <- function(sheet1) {
  df <- sheet1 %>%
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
split_county_state <- function(sheet1) {
  county <- sheet1[grep(pattern = ", \\w\\w", sheet1$area_name), ]
  class(county) <- c("county", class(county))
  county <- create_state(county)
  state <- sheet1[-c(grep(patter = ", \\w\\w", sheet1$area_name)), ]
  class(state) <- c("state", class(state))
  state <- create_division(state)
  return(list(county, state))
}

##########################
### Part 3 Wrapper ######
########################

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
