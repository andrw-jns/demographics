######################################
"DEMOGRAPHICS:"
"DESCRIPTIVE STATISTICS"
######################################

# Notes:
# proximity_to_death and admissions: Only 14/15
# population: all years.

"Notes:

Longer time series?

Remove HEALTHY BABIES from the analysis as in JW analysis

By REGION? Will we observe the London effect still?

If we are to use period effect in a model (according to Steven's outline)
we will need to make assumptions about future period effects. BUT we may
not be using period effect to predict, since we are only dealing with 
demand (population) factors?"


library(here)
library(stringr)
library(tidyverse)
library(extrafont) # for theme_strategy.
# library(hrbrthemes)

# devtools::install_github("thomasp85/patchwork")
library(patchwork)

# Turn off scientific notation:
options(scipen = 999)

# functions
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

# test <- read_rds(here("data", "ip_data.RDS")) %>% 
#   mutate(year = as.numeric(str_sub(fyear, 1, 4))) %>% 
#   select(fyear, year, everything()) %>% 
#   add_age_band(. , test$age_group)
# 
#   add_age_band(test, test$age_group)

# Load --------------------------------------------------------------------

# Age groupings could/should be based on literature, but provisionally:
# 0-19 # 20-44 # 45-64 # 65-84 # 85+


ip_data1 <- read_rds(here("data", "ip_data.RDS")) %>% 
  mutate(year = as.numeric(str_sub(fyear, 1, 4))) %>% 
  select(fyear, year, everything()) %>% 
  add_age_band(., .$age_group) %>% 
  as.tibble()
  # mutate(age_band = case_when(
  #   str_detect(.$age_group, "^0|^1")      ~ "00to19",
  #   str_detect(.$age_group, "^2|^3|^40")  ~ "20to44",
  #   str_detect(.$age_group, "^45|^5|^60") ~ "45to64",
  #   str_detect(.$age_group, "^65|^7|^80") ~ "65to84",
  #   str_detect(.$age_group, "^85|^90")    ~ "85plus"
  # ))

population_estimates <- read_rds(here("data", "AJ_2018-01-05_pop_est.RDS")) %>% 
  add_age_band(., .$age_group) %>%
  as.tibble()

population_projections <- read_rds(here("data", "AJ_2018-01-05_pop_proj.RDS")) %>% 
  add_age_band(., .$age_group) %>%
  as.tibble()
  # mutate(age_band = case_when(
  #   str_detect(.$age_group, "^0|^1")      ~ "00to19",
  #   str_detect(.$age_group, "^2|^3|^40")  ~ "20to44",
  #   str_detect(.$age_group, "^45|^5|^60") ~ "45to64",
  #   str_detect(.$age_group, "^65|^7|^80") ~ "65to84",
  #   str_detect(.$age_group, "^85|^90")    ~ "85plus"
  # )) 


#  ***----------------------------------------------------------------

# Wrangle: -----------------------------------------------------------------

# *** -------------------------------------------------------------------

ip_base <- left_join(ip_data1, population_estimates,
                     by = c("year", "gender", "age_group")) %>% 
  rename(age_band = age_band.x) # %>% 
  # mutate_at(vars(fyear, age_group, age_band, gender, lunney_group),
  #           funs(factor))
# How many NAs (ie. have no gender / age group)? See test modelling.

# Admissions ---------------------------------------------------------

admis <- ip_base %>% 
  group_by(fyear, age_band, gender, population) %>%
  summarise(admissions = sum(admissions)) %>% 
  ungroup() %>% 
  group_by(fyear, age_band, gender) %>% 
  summarise(admissions = sum(admissions),
            population = sum(population),
            adm_rate_1k = sum(admissions)/sum(population)*1000)


# Lunney Group -------------------------------------------------------
"Random assignment of 'Frailty' may need to be added to SQL"

rate_lunney <- ip_base %>% 
  group_by(fyear, age_band, lunney_group, population) %>% 
  summarise(admissions = sum(admissions, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(fyear, age_band, lunney_group) %>% 
  summarise(admissions = sum(admissions),
            population = sum(population),
            adm_rate_1k = sum(admissions)/sum(population)*1000)


# Proximity to death ------------------------------------------------------

rate_prox_basic <- ip_base %>% 
  group_by(fyear, age_band, proximity_death, population) %>%
  summarise(admissions = sum(admissions, na.rm = T)) %>%
  ungroup() %>% 
  group_by(fyear, age_band, proximity_death) %>% 
  summarise(admissions = sum(admissions),
            population = sum(population),
            adm_rate_1k = sum(admissions)/sum(population)*1000) %>% 
  drop_na()


rate_prox_lunney <- ip_base %>% 
  group_by(fyear, age_band, proximity_death, lunney_group, population) %>%
  summarise(admissions = sum(admissions, na.rm = T)) %>%
  ungroup() %>% 
  group_by(fyear, age_band, proximity_death, lunney_group) %>% 
  summarise(admissions = sum(admissions),
            population = sum(population),
            adm_rate_1k = sum(admissions)/sum(population)*1000) %>% 
  drop_na()


# *** ---------------------------------------------------------------------


# Plots: -------------------------------------------------------------------


# *** ---------------------------------------------------------------------


# Pop by year ------------------------------------------------------

grph_pop_est <- ggplot(population_estimates %>% 
         mutate(year = as.factor(year)) %>% 
         group_by(age_band, year, gender) %>% 
         summarise(population = sum(population)),
         aes(year, population, colour = age_band))+
  geom_line(aes(group = interaction(age_band, gender)))+ # two groups.
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")+
  ylim(0, 10e6)


# Pop projections -------------------------------------------------------------


grph_pop_proj <- ggplot(population_projections %>% 
         mutate(year = as.factor(year)) %>% 
         group_by(age_band, year, gender) %>% 
         summarise(population = sum(population)),
       aes(year, population, colour = age_band))+
  geom_line(aes(group = interaction(age_band, gender))) + # two groups.
  theme_minimal()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()
        )+
  ylim(0, 10e6) +
  ggrepel::geom_text_repel( data = population_projections %>%
                                     mutate(year = as.factor(year)) %>%
                                     filter(year == 2039) %>%
                                     group_by(age_band, year, gender) %>%
                                     summarise(population = sum(population)),
                              aes(label = gender, group = gender) #,
  # geom_text(aes(label = gender),
  #                          size = 2,
  #                          data = population_projections %>%
  #                            mutate(year = as.factor(year)) %>%
  #                            filter(year == 2017) %>%
  #                            group_by(age_band, year, gender) %>%
  #                            summarise(population = sum(population)))
  )

grph_pop_est + grph_pop_proj


# Adm by age band, and age and gender -----------------------------------

# Basic Age band by year:
ggplot(admis %>%
         group_by(fyear, age_band) %>%
         summarise(adm_rate_1k = sum(admissions, na.rm = T)/sum(population, na.rm = T)*1000) %>% 
         drop_na(),
       aes(fyear, adm_rate_1k, colour = age_band))+
  geom_point()+
  geom_line(aes(group = age_band))


# This plot needs attention: labels (!):
ggplot(admis %>% drop_na(),
       aes(fyear, adm_rate_1k, colour = age_band, group = interaction(age_band, gender)))+
  ggrepel::geom_text_repel(aes(label = gender),
                           nudge_x = 2,
                           size = 2,
                           #colour = "black",
                           data = admis %>%
                             drop_na() %>% filter(fyear == "201415" ))+
  # geom_point()+
  geom_line(aes(group = interaction(age_band, gender)))



# Lunney Group -------------------------------------------------

"When updated, frailty should be the biggest group - cross reference with lunney paper"

ggplot(rate_lunney %>% drop_na, # %>% filter(fyear == "201415"),
       aes(fyear, adm_rate_1k, colour = age_band, group = age_band))+
  geom_line()+
  facet_wrap(~lunney_group)
# highlights the lunney group


# or :
"Will have to decide when all years data"

ggplot(rate_lunney %>% drop_na,
       aes(fyear, adm_rate_1k, colour = lunney_group, group = lunney_group ))+
  geom_line()+
  facet_wrap(~age_band)
# highlights the affect of age 


# Proximity to death  -------------------------------------------
"Tweaks needed here"
# Counts:
ggplot(rate_prox_basic %>% filter(proximity_death <24) %>% ungroup,
       aes(proximity_death, admissions, colour = age_band))+
  geom_line()

# How to interpret admission rate by proximity to death?
# 800 admissions per 1k (over 85) population are due last month of life?
ggplot(rate_prox_basic  %>% filter(proximity_death <24),
       aes(proximity_death, adm_rate_1k, colour = age_band))+
  geom_line()

ggplot(rate_prox_lunney %>% ungroup %>% filter(proximity_death <24),
       aes(proximity_death, adm_rate_1k))+
  geom_line(aes(group = interaction(lunney_group, fyear), colour = lunney_group))+
  facet_wrap(~age_band, scales = "free")

investigation <- rate_prox_lunney %>% ungroup %>% filter(proximity_death <24)
# it's by year that needs to be broken down.


ggplot(rate_prox_lunney %>% ungroup %>% filter(proximity_death <24),
       aes(proximity_death, admissions))+
  geom_line(aes(group = interaction(lunney_group, fyear), colour = lunney_group))+
  facet_wrap(~age_band, scales = "free")
