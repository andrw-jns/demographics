##################################
"TEST DATA PULLED FROM SQL SERVER"
##################################

"Notes: If we are to use period effect in a model (according to Steven's outline)
we will need to make assumptions about future period effects"

library(here)
library(stringr)
library(tidyverse)

# Turn off scientific notation:
options(scipen = 999)

# Notes:
# proximity_to_death and admissions: Only 14/15
# population: all years.

# 20171230 ----------------------------------------------------------------

# Load --------------------------------------------------------------------

"After testing, could now perhaps combine admissions and proximity to death into one query"

admissions <- read_csv(here("tmp","admissions.csv"), na = "NULL") %>% 
  mutate(year = as.numeric(str_sub(fyear, 1, 4))) %>% 
  select(fyear, year, everything())

proximity_to_death <- read_csv(here("tmp", "proximity_to_death.csv")) %>% 
  mutate_at(vars(2:5), funs(ifelse(. == "NULL", NA, .))) %>% 
  mutate(proximity_death = as.numeric(proximity_death)) %>% 
# A Few NAs introduced by coersion due to the "Error". Recode in SQL to 999 (or smthg)
# and then wouldn't have to re-code as numeric.
  mutate(year = as.numeric(str_sub(fyear, 1, 4))) %>% 
  select(fyear, year, everything())


population_estimates <- read_csv(here("tmp", "population_estimates.csv"), na = "NULL") %>% 
  mutate(age_band = case_when(
    str_detect(.$age_group, "^0|^1")      ~ "00to19",
    str_detect(.$age_group, "^2|^3|^40")  ~ "20to44",
    str_detect(.$age_group, "^45|^5|^60") ~ "45to64",
    str_detect(.$age_group, "^65|^7|^80") ~ "65to84",
    str_detect(.$age_group, "^85|^90")    ~ "85plus"
  )) %>% 
  filter(!age_group == "85plus") # problem will be fixed at root.


population_projections <- read_csv(here("tmp", "population_projections.csv"), na = "NULL") %>% 
  mutate(age_band = case_when(
    str_detect(.$age_group, "^0|^1")      ~ "00to19",
    str_detect(.$age_group, "^2|^3|^40")  ~ "20to44",
    str_detect(.$age_group, "^45|^5|^60") ~ "45to64",
    str_detect(.$age_group, "^65|^7|^80") ~ "65to84",
    str_detect(.$age_group, "^85|^90")    ~ "85plus"
  )) 

# *** ---------------------------------------------------------------------


# Wrangle -----------------------------------------------------------------

# group by age, group by gender
# Age groupings should be based on literature, but provisionally:
# 0-19 # 20-44 # 45-64 # 65-84 # 85+

rate_admissions <- left_join(admissions, population_estimates, by = c("year", "gender", "age_group")) %>% 
  mutate(fyear = as.factor(fyear))

rate_age_band <- rate_admissions %>%
  drop_na() %>% 
  group_by(fyear, age_band) %>%
  summarise(adm_rate_10k = (sum(admissions)/sum(population)*10000))

rate_gender_age <- rate_admissions %>%
  drop_na() %>% 
  group_by(fyear, age_band, gender) %>%
  summarise(adm_rate_10k = (sum(admissions)/sum(population)*10000))



tmp <- left_join(proximity_to_death, population_estimates, by = c("year", "gender", "age_group")) %>% 
  mutate(fyear = as.factor(fyear))
  
rate_lunney <- tmp %>% 
  group_by(fyear, age_band, lunney_group, population) %>% 
  summarise(#adm_rate_10k = (sum(admissions)/sum(population))*10000)
    admissions = sum(admissions)) %>% 
  ungroup() %>% 
  group_by(fyear, age_band, lunney_group) %>% 
  summarise(admissions = sum(admissions),
            population = sum(population),
            adm_rate_10k = sum(admissions)/sum(population)*10000)

# count_prox_basic <- tmp %>% 
#   group_by(fyear, proximity_death) %>%
#   summarise(admissions = sum(admissions, na.rm = T)) %>% 
#   drop_na()
# count_prox_age <- tmp %>% 
#   group_by(fyear, proximity_death, age_band, lunney_group) %>%
#   summarise(admissions = sum(admissions, na.rm = T)) %>% 
#   drop_na()

rate_prox_basic <- tmp %>% 
  group_by(fyear, age_band, proximity_death, population) %>%
  summarise(admissions = sum(admissions, na.rm = T)) %>%
  ungroup() %>% 
  group_by(fyear, age_band, proximity_death) %>% 
  summarise(admissions = sum(admissions),
            population = sum(population),
            adm_rate_10k = sum(admissions)/sum(population)*10000) %>% 
  drop_na()



rate_prox_lunney <- tmp %>% 
  group_by(fyear, age_band, proximity_death, lunney_group, population) %>%
  summarise(admissions = sum(admissions, na.rm = T)) %>%
  ungroup() %>% 
  group_by(fyear, age_band, proximity_death, lunney_group) %>% 
  summarise(admissions = sum(admissions),
            population = sum(population),
            adm_rate_10k = sum(admissions)/sum(population)*10000) %>% 
  drop_na()

# *** ---------------------------------------------------------------------


# Plots: -------------------------------------------------------------------

# Pop by year ------------------------------------------------------

ggplot(population_estimates %>% 
         mutate(year = as.factor(year)) %>% 
         group_by(age_band, year, gender) %>% 
         summarise(population = sum(population)),
       aes(year, population, colour = age_band))+
  geom_line(aes(group = interaction(age_band, gender))) # two groups.



# Pop projections -------------------------------------------------------------

ggplot(population_projections %>% 
         mutate(year = as.factor(year)) %>% 
         group_by(age_band, year, gender) %>% 
         summarise(population = sum(population)),
       aes(year, population, colour = age_band))+
  geom_line(aes(group = interaction(age_band, gender))) # two groups.




# Adm by age band, and age and gender -----------------------------------

ggplot(rate_age_band,
       aes(fyear, adm_rate_10k, colour = age_band))+
  geom_point()


# This plot needs attention: labels (!):
ggplot(rate_gender_age,
       aes(fyear, adm_rate_10k, colour = age_band))+
  geom_text(aes(label = gender), position = position_dodge(0.2), size = 2)+
  geom_point()



# Lunney Group -------------------------------------------------

ggplot(rate_lunney %>% drop_na,
       aes(fyear, adm_rate_10k, colour = age_band))+
  geom_point()+
  facet_wrap(~lunney_group)
# highlights the lunney group


# or :
"Will have to decide when all years data"

ggplot(rate_lunney %>% drop_na,
       aes(fyear, adm_rate_10k, colour = lunney_group))+
  geom_point()+
  facet_wrap(~age_band)
# highlights the affect of age 


 

# Proximity to death  -------------------------------------------

ggplot(rate_prox_basic, aes(proximity_death, admissions, colour = age_band))+
  geom_line()

# How to interpret admission rate by proximity to death?
# 800 admissions per 10k (over 85) population are due last month of life?
ggplot(rate_prox_basic, aes(proximity_death, adm_rate_10k, colour = age_band))+
  geom_line()

ggplot(rate_prox_lunney, aes(proximity_death, adm_rate_10k))+
  geom_line(aes(group = lunney_group, colour = lunney_group))+
  facet_wrap(~age_band, scales = "free")

