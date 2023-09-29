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
