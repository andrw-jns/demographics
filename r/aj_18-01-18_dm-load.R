######################################
"DEMOGRAPHICS:"
"LOAD DATA"
######################################

library(here)
library(stringr)
library(tidyverse)


# Functions:

# Age groupings could/should be based on literature, but provisionally:
# 0-19 # 20-44 # 45-64 # 65-84 # 85+

add_age_band <- function(df, df.age_var){
  
  var_enq <- enquo(df.age_var)
  
  df %>% mutate(age_band = case_when(
    str_detect(!!var_enq, "^0|^1")      ~ "00to19",
    str_detect(!!var_enq, "^2|^3|^40")  ~ "20to44",
    str_detect(!!var_enq, "^45|^5|^60") ~ "45to64",
    str_detect(!!var_enq, "^65|^7|^80") ~ "65to84",
    str_detect(!!var_enq, "^85|^90")    ~ "85plus"
    ))
}


# Load --------------------------------------------------------------------


ip_data <- read_rds(here("data", "ip_data.RDS")) %>% 
  mutate(year = as.numeric(str_sub(fyear, 1, 4))) %>% 
  select(fyear, year, everything()) %>% 
  add_age_band(., .$age_group) %>% 
  as.tibble()

population_estimates <- read_rds(here("data", "AJ_2018-01-05_pop_est.RDS")) %>% 
  add_age_band(., .$age_group) %>%
  as.tibble()

population_projections <- read_rds(here("data", "AJ_2018-01-05_pop_proj.RDS")) %>% 
  add_age_band(., .$age_group) %>%
  as.tibble()

# Collate above into: 

ip_base <- left_join(ip_data, population_estimates,
                     by = c("year", "gender", "age_group")) %>% 
  rename(age_band = age_band.x) %>% 
  select(-age_band.y)

