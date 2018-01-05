####################################
"DEMOGRAPHICS:"
"EXPERIMENTATION WITH MODELS"
####################################

library(apc)
library(here)
library(tidyverse)

tmp <- ip_base %>% 
  group_by(fyear, gender, age_band, population) %>%
  summarise(admissions = sum(admissions)) %>% 
  ungroup() %>% 
  group_by(fyear, gender, age_band) %>% 
  summarise(admissions = sum(admissions),
            population = sum(population),
            adm_rate_1k = sum(admissions)/sum(population)*1000)



mod_nb_basic <- MASS::glm.nb(admissions ~ age_band + gender + fyear + offset(log(population)),
                             data = tmp)



