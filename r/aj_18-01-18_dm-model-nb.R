#########################################
"DEMOGRAPHICS:"
"MODEL: NEGATIVE BINOMIAL EXPERIMENTATION"
#########################################

# The purpose of this script is to quickly explore/understand why certain models don't work,
# And how a future modelling process might best be structured.

options(na.action = na.warn)

library(here)
library(lme4)
library(modelr)
library(tidyverse)
# library(apc)
# library(ggeffects)

source(here("r", "aj_18-01-05_dm-load.R"))

# Functions:
predictify_log <- function(df, model_name){
  
  x <- add_predictions(df, model_name) %>% 
    mutate(pred = exp(pred))
  x
}


trend_plot <- function(prediction_df){
  plot_this <- ggplot(data = df_modelling %>% drop_na(), aes(year, admissions, colour= age_band))+
    geom_line(aes(group = interaction(age_band, gender))) +
    geom_point(data = prediction_df,
               aes(year, pred, colour = age_band, group = interaction(age_band, gender)))
  plot_this
}

# Wrangle ----------------------------------------------------------

df_modelling <- ip_base %>% 
  group_by(year, gender, age_band, population) %>%
  summarise(admissions = sum(admissions)) %>% 
  ungroup() %>% 
  group_by(year, gender, age_band) %>% 
  summarise(admissions = sum(admissions),
            population = sum(population),
            adm_rate_1k = sum(admissions)/sum(population)*1000) %>% 
  # for me groups:
  group_by(year) %>% 
  mutate(id = row_number()) %>%
  arrange(id)
  

# df_me_model <- df_modelling %>% 
#   group_by(year) %>% 
#   mutate(id = row_number()) %>% 
#   arrange(id)

# Models --------------------------------------------------------------


mod_nb <- MASS::glm.nb(admissions ~ age_band + gender + year + offset(log(population)),
                       data = df_modelling %>% drop_na)


mod_me_nb <- glmer.nb(admissions ~ age_band + gender + (year | id) + offset(log(population)),
                      data = df_modelling %>% drop_na() %>% ungroup)
"Ideally needs fine-tuning"


# Predictions and plots ----------------------------------------------

prediction_nb     <- predictify_log(df_modelling, mod_nb)
prediction_me_nb  <- predictify_log(df_modelling %>% drop_na(), mod_me_nb)


trend_plot(prediction_nb)
trend_plot(prediction_me_nb)

# Test ggeffects 
# plot(ggpredict(m6, c("year", "age_band", "gender")# ,
#                #type = "re"
#                # full.data = T
#                ))
"Would we get away with a linear mixed effects model"


ggplot(data = df_modelling %>% drop_na(), aes(year, admissions))+#, colour= age_band))+
    geom_point(alpha = 0.2) +
    geom_line(data = prediction_me_nb,
               aes(year, pred, colour = age_band, group = interaction(age_band, gender)))
 
# Forecasting --------------------------------------------------------

future <- expand.grid(
  year = as.numeric(seq(2017, 2039))
  , gender =  unique(df_modelling$gender) # c("M", "F") #
  , age_band = unique(df_modelling$age_band) # c("00to19", "20to44", "45to64", "65to84", "85plus")  
  , admissions = as.numeric(1)) %>% as.tibble() %>% ungroup %>% 
  mutate_at(vars(age_band, gender), funs(as.character))

# Note: can't use tidyr::crossing as it rermoves NA levels

pop_proj_short <- population_projections %>% 
  group_by(year, gender, age_band) %>% 
  summarise(population = sum(population)) %>% 
  ungroup %>% as.tibble()


futurey <- left_join(future, pop_proj_short, by = c("year", "age_band","gender")) %>% 
  mutate(adm_rate_1k = as.double(1)) %>% 
  arrange(gender, age_band) %>% 
  group_by(year) %>% 
  mutate(id = row_number()) %>% 
  arrange(id) %>% 
  ungroup() %>% 
  drop_na()


past_future <- bind_rows(df_modelling %>% drop_na(), futurey)

prediction_future <- predictify_log(past_future, mod_me_nb)


trend_plot(prediction_future)+ ylim(0,1.8e6)
# fit a function to each group based on past trends.
#Thus continuation of that function not necessarily helpful!


# Test if this takes population into account:
# It does, but year seems a strong influence (which is probably meaningless)
# (ie. admissions will still grow sharply from 2030 despite keeping population
# relatively steady # and we haven't given model enough to predict this) 

futurez <- futurey %>%
  mutate(population = population*sqrt(17/(year - 2000)))
past_futurez <- bind_rows(df_modelling %>% drop_na(), futurez)
prediction_futurez <- predictify_log(past_futurez, mod_me_nb)
trend_plot(prediction_futurez)+ ylim(0,1.8e6)

yah <- unique(futurey$year)
pop <- 200

pop/((yah - 2000)/17)^2


sqrt(17/(yah - 2000))
