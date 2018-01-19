######################################
"DEMOGRAPHICS:"
"MODEL WITH TIME TO DEATH"
######################################

"NEED TO FIX A LONGER TIME PERIOD"
# adm ~ TTD + age + sex + year

# What kind of distribution?

options(na.action = na.warn)

library(here)
library(lme4)
library(modelr)
library(tidyverse)
# library(apc)
# library(ggeffects)

source(here("r", "aj_18-01-05_dm-load.R"))


ip_base %>% filter(is.na(proximity_death)) & proximity_death > 0)

ttd_base <- ip_base %>% 
  mutate(ttd = case_when(
    is.na(ip_base$proximity_death) ~ 0,   # No death
    ip_base$proximity_death %in% c(0:11) ~ 1, # Activity in last year of life
    ip_base$proximity_death %in% c(12:23) ~2  # Activity in penultimate year of life
  ))



df_ttd <- ttd_base %>% 
  group_by(year, gender, age_group, population, ttd) %>%
  summarise(admissions = sum(admissions)) %>% 
  ungroup() %>% 
  group_by(year, ttd) %>% 
  filter(!is.na(ttd)) %>% # ttd NAs are those who have activity after death ie. prox_death == 999
  mutate(id = row_number()) %>%
  arrange(id)
# 
# ggplot(df_ttd, aes(admissions))+
#   geom_histogram()

# Remove those who did not die:
df_ttd2 <- df_ttd %>% filter(ttd != 0)

ggplot(df_ttd2, aes(year, admissions, colour = ttd))+
  geom_line(aes(group = interaction(age_group, gender, ttd)))


ggplot(df_ttd2, aes(admissions))+
  geom_histogram()

# Geometric distribution, therefore a negative binomial with dispersion parameter set to 1.

# With Mixed effects for group:

mod_provis <- MASS::glm.nb(admissions ~ age_group + gender + ttd + offset(log(population)),
                           data = df_ttd2)
 

mod_provis2 <- MASS::glm.nb(admissions ~ age_group + gender + ttd + year + offset(log(population)),
                           data = df_ttd2)

mod_provisional <- glmer.nb(admissions ~ age_group + gender + ttd + (1 | id) + offset(log(population)),
                      data = df_ttd2 %>% drop_na())
"Ideally needs fine-tuning"

BIC(mod_provis)
BIC(mod_provis2)

prediction_reg <- add_predictions(df_ttd2, mod_provis) %>% 
  mutate(exp(pred))
