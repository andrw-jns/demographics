####################################
"DEMOGRAPHICS:"
"EXPERIMENTATION WITH MODELS"
####################################

library(apc)
library(here)
library(tidyverse)

tmp <- ip_base %>% 
  group_by(year, gender, age_band, population) %>%
  summarise(admissions = sum(admissions)) %>% 
  ungroup() %>% 
  group_by(year, gender, age_band) %>% 
  summarise(admissions = sum(admissions),
            population = sum(population),
            adm_rate_1k = sum(admissions)/sum(population)*1000)



mod_nb_basic <- MASS::glm.nb(admissions ~ age_band + gender + year + offset(log(population)),
                             data = tmp)


crimp <- modelr::add_predictions(tmp, mod_nb_basic) %>% 
  mutate(pred = exp(pred))

ggplot(data = tmp, aes(year, admissions, colour= age_band))+
  geom_line(aes(group = interaction(age_band, gender))) +
  geom_point(data = crimp, aes(year, pred, colour = age_band, group = interaction(age_band, gender)))

