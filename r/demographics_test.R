##################################
"TEST DATA PULLED FROM SQL SERVER"
##################################

library(here)
library(stringr)
library(tidyverse)


# Notes:
# proximity_to_death and admissions only 14/15
# population all years.

# 20171230 ----------------------------------------------------------------

# Load --------------------------------------------------------------------


admissions <- read_csv(here("tmp","admissions.csv"), na = "NULL") %>% 
  mutate(year = as.numeric(str_sub(fyear, 1, 4))) %>% 
  select(fyear, year, everything())

proximity_to_death <- read_csv(here("tmp", "proximity_to_death.csv")) %>% 
  mutate_at(vars(2:5), funs(ifelse(. == "NULL", NA, .))) %>% 
  mutate(proximity_death = as.numeric(proximity_death))

# A Few NAs introduced by coersion due to the "Error". Recode in SQL to 999 or smthg.

# Population only correct from 2014 (ie. for year 14/15) onwards
population_estimates <- read_csv(here("tmp", "population_estimates.csv"), na = "NULL")


# Wrangle -----------------------------------------------------------------

rate_admissions <- left_join(admissions, population_estimates, by = c("year", "gender", "age_group")) %>% 
  mutate(adm_rate = admissions/population,
         fyear    = as.factor(fyear))


# group by age, group by gender
# Age groupings should be based on list, but provisionally
# 0-19
# 20-44
# 45-64
# 65-84
# 85+

grp_rate_admissions <- rate_admissions %>% 
  mutate(age_band = case_when(
    str_detect(rate_admissions$age_group, "^0|^1")      ~ "00to19",
    str_detect(rate_admissions$age_group, "^2|^3|^40")  ~ "20to44",
    str_detect(rate_admissions$age_group, "^45|^5|^60") ~ "45to64",
    str_detect(rate_admissions$age_group, "^65|^7|^80") ~ "65to84",
    str_detect(rate_admissions$age_group, "^85|^90")    ~ "85plus"
  ))

# Plots -------------------------------------------------------------------

ggplot(grp_rate_admissions %>% drop_na %>% group_by(fyear, age_band) %>% summarise(adm_rate_10k = (sum(admissions)/sum(population)*10000)),
       aes(fyear, adm_rate_10k, colour = age_band, group = age_band))+
  geom_point()



# 20171229 ----------------------------------------------------------------

# prox_death_test <- read_csv("C:/2017_projects/tmp/prox_death_test.csv", 
#                             col_names = FALSE
#                             , na = "NULL" )
# 
# test_master_table <- read_csv("C:/2017_projects/tmp/test_master_table.csv", 
#                               col_names = FALSE)
# 
# 
# ggplot(prox_death_test %>% mutate(X6 = as.numeric(X6)) %>% drop_na(X6), aes(X6))+
#   geom_histogram(stat = "count")
# 
# ggplot(prox_death_test, aes(X6))+
#   geom_histogram(stat = "count")
# 
# 
# colnames(test_master_table) <- c("fyear", 
#                                  "age_group",
#                                  "gender",
#                                  "proximity",
#                                  "lunney",
#                                  "population",
#                                  "admissions")
# 
# 
# 
# 
# tmp <- test_master_table %>% 
#   mutate_at(vars(population, admissions, proximity), funs("as.numeric")) %>% 
#   group_by(age_group, gender, proximity) %>% 
#   summarise(# population = sum(population, na.rm = T),
#             admisssions = sum(admissions, na.rm = T)) %>% 
#   arrange(gender, age_group, proximity)
