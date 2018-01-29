##########################
"DEMOGRAPHICS:"
"Wrangle Mortality Rates"
##########################

library(tidyverse)
library(readxl)
library(here)
# library(fs)
library(janitor)

raw_death_rates <- read_xls(here("data", "tables14and614_tcm77-422542.xls"
                             ), 
                            sheet = "Table 1",
                            skip = 4)

death_rates <- raw_death_rates %>% 
  clean_names() %>% 
  remove_empty_cols() %>% 
  remove_empty_rows() %>% 
  slice(1:43)

gather_cols <- function(df, string_like, col_new_name) {
  col_string <- deparse(substitute(string_like))
  
  df %>%
    select(starts_with(col_string)) %>%
    gather(title, col_new_name) %>%
    select(-title)
  
}

mort_rates <- bind_cols(
  gather_cols(death_rates, age, ages),
  gather_cols(death_rates, male, male),
  gather_cols(death_rates, female, female)
) %>% `colnames<-`(c("ages", "M", "F")) %>% 
  drop_na() 

mort_rates <- mort_rates %>%
  gather(gender, death_rate, 2:3) %>% 
  mutate(death_rate = as.numeric(death_rate))

mort_groups <- mort_rates %>% 
  filter(str_detect(.$ages, "[:punct:]|and") ) %>%
  mutate(age_group = str_replace_all(.$ages, "[:punct:]", "to")) %>% 
  mutate(age_group = str_replace(.$age_group, "^0to4", "00to04")) %>% 
  mutate(age_group = str_replace(.$age_group, "5to9", "05to09")) %>%
  mutate(age_group = str_replace(.$age_group, " and over", "plus")) %>% 
  select(-ages)

