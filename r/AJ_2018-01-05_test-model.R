####################################
"DEMOGRAPHICS:"
"MODEL EXPERIMENTATION"
####################################

# The purpose of this script is to quickly explore/understand why certain models don't work,
# And how a future modelling process might best be structured.

options(na.action = na.warn)

library(apc)
library(here)
library(modelr)
library(tidyverse)

"Try GAMs"
"Mixed models"
"Generalised Estimating Equations"
"apc package"
"GENERALLY HAVE BEEN LINEAR TRENDS"
"Probably better to ungroup age! Or at least disagregate by a level"
"Longer time series?"
# Questions ----------------------------------------------------------


# 1. How to handle NAs?
# 2. Interaction terms?
# 3. Issue with number of predictors similar to degrees freedom?
# 4. How much of an issue is over fitting in this forecasting game?
# 5. Is there an equivalent of training and testing for these types of forecasts?
" After this is done, how to include prox_death -> read paper highlighted in lit review"


# Preparation --------------------------------------------------------

tmp <- ip_base %>% 
  group_by(year, gender, age_band, population) %>%
  summarise(admissions = sum(admissions)) %>% 
  ungroup() %>% 
  group_by(year, gender, age_band) %>% 
  summarise(admissions = sum(admissions),
            population = sum(population),
            adm_rate_1k = sum(admissions)/sum(population)*1000)

# tmp <- tmp %>% drop_na()
# na_observations <- tmp %>% filter_all(any_vars(is.na(.)))

# But should still consider them as part of total admissions. Could 
# distribute them to ages according to gender distribution, or are one group
# more likely to have no age recorded. Could explore if necessary.
# 
# tmp %>% filter(year == 2014, gender == "M") %>% 
#   drop_na() %>% 
#   summarise(sum(admissions))

# There are 100 rows of complete cases, 80 including NAs.
# Mostly cases of genders missing an age. [Small Fraction of the Data
# 11k in 2.7M or 0.4% ].



# Distributions -------------------------------------------------------

"Which distribution?"

ggplot(tmp %>%  drop_na # %>% filter(year == 2013)
       , aes(adm_rate_1k))+
  # geom_dotplot(stat_bindot(bins = 500))
  geom_histogram(bins = 1000)
# Is more a nbinom, than the distribuion of counts


ggplot(tmp %>%  drop_na # %>% filter(year == 2013)
       , aes(adm_rate_1k))+
  # geom_dotplot(stat_bindot(bins = 500))
  geom_histogram(bins = 1000)
# Is more a nbinom, than the distr


ggplot(tmp %>%  drop_na %>% filter(year == 2013), aes(admissions))+
  geom_histogram(bins = 100)
# Is more a nbinom, than the distribuion of counts


ggplot(tmp %>%  drop_na, aes(admissions))+
  geom_histogram(bins = 50)
# Is more a nbinom, than the distribuion of counts


install.packages("fitdistrplus")
library(fitdistrplus)

plot(fitdist(tmp %>% drop_na %>% pull(admissions), "nbinom"))
plot(fitdist(tmp %>% drop_na %>% pull(admissions), "pois"))


# plot(fitdist(tmp %>% drop_na %>% pull(adm_rate_1k), "gamma"))
# plot(fitdist(tmp %>% drop_na %>% pull(adm_rate_1k), "weibull"))
plot(fitdist(tmp %>% drop_na %>% pull(adm_rate_1k), "lnorm"))

# Models -------------------------------------------------------------


"Is it reasonable to have year as a variable in this model? WHAT IF JUST
the interaction between AGE_BAND and gender. Then F 20to44 from 2009 would 
be grouped with  F 20to44 from 2014. This wouldn't be correct. 

YEAR takes into period factors (ie. gov policy) as well as (implicitly) 
changes in the population size. Remember year as function of population )
It's impossible to foresee how year may change.
Which is what APC method is getting at."


model_update_count <- MASS::glm.nb(admissions ~ age_band + gender + year + log(population),
                             data = tmp %>% drop_na)

mod_nb_count <- MASS::glm.nb(admissions ~ age_band + gender + year + offset(log(population)),
                             data = tmp %>% drop_na)

mod_nb_rate <- MASS::glm.nb(adm_rate_1k ~ age_band + gender + year, # + offset(log(population)),
                            data = tmp %>% drop_na)
# The rate model fits much better than the count, maybe due to distribution


mod_nb_rate_int <- MASS::glm.nb(adm_rate_1k ~ age_band*gender + year, # + offset(log(population)),
                            data = tmp %>% drop_na)

# Problem here is how does the model interpret future years (factors are a complete set)
# mod_nb_rate_int_catyear <- MASS::glm.nb(adm_rate_1k ~ age_band*gender + as.character(year), # + offset(log(population)),
#                                 data = tmp %>% drop_na)

# mod_nb_rate_3int <- MASS::glm.nb(adm_rate_1k ~ age_band*gender*year, # + offset(log(population)),
#                                  data = tmp %>% drop_na)


# How a formula would be interpreted:
# m_matrix <- modelr::model_matrix(tmp, adm_rate_1k ~ age_band*gender*year)

model_matrix(tmp, admissions ~ age_band+gender+year+log(population)) # 8 variables
model_matrix(tmp, admissions ~ age_band+gender+year+offset(log(population))) # 7 variables
model_matrix(tmp, adm_rate_1k ~ age_band+gender+year) # 7 variables
model_matrix(tmp, adm_rate_1k ~ age_band*gender+year) # 11 variables
model_matrix(tmp, adm_rate_1k ~ age_band*gender*year) # 20 variables

# Choose BIC:
BIC(mod_nb_rate)
BIC(mod_nb_rate_int)
BIC(mod_nb_rate_3int)
# As expected rate_int provides a good balance between fit and dof.

broom::glance(mod_nb_rate_int)

# Create model function:
nb_rate_int_model <- function(df) {
  MASS::glm.nb(adm_rate_1k ~ age_band*gender + year, data = df)
}

# Artificial use of nest, but it's useful for learning purposes.
tmp_nest <- tmp %>% 
  mutate(title_gender = gender, title_age = age_band) %>%
  drop_na %>%
  group_by(title_gender, title_age) %>%
  nest() %>% 
  mutate(mod_nb = map(data, nb_rate_int_model))

# The model obviously relies on the whole dataframe! There is no subsetting
# like in the R4DS method.

tmp_nest_2 <- tmp %>% ungroup %>% nest %>% 
  mutate(mod_nb = map(data, nb_rate_int_model))

# Can add more models and more predictions from this point.
  
# Add predictions ----------------------------------------------------
# 
# crimp <- modelr::add_predictions(tmp, mod_nb_count) %>% 
#   mutate(pred = exp(pred))
# 
# crimp2 <- modelr::add_predictions(tmp, mod_nb_rate) %>% 
#   mutate(pred = exp(pred))

crimp3 <- modelr::add_predictions(tmp, mod_nb_rate_int) %>% 
  mutate(pred = exp(pred))

# crimp31 <- modelr::add_predictions(tmp, mod_nb_rate_int_catyear) %>% 
#   mutate(pred = exp(pred))

# Issue with number of predictors similar to degrees freedom?
# crimp32 <-  modelr::add_predictions(tmp, mod_nb_rate_3int) %>% 
#   mutate(pred = exp(pred))


# Plots --------------------------------------------------------------

# ggplot(data = tmp %>% drop_na(), aes(year, admissions/population*1000, colour= age_band))+
#   geom_line(aes(group = interaction(age_band, gender))) +
#   geom_point(data = crimp %>% drop_na(),
#              aes(year, pred/population*1000, colour = age_band, group = interaction(age_band, gender)))
# 
# 
# ggplot(data = tmp %>% drop_na(), aes(year, adm_rate_1k, colour= age_band))+
#   geom_line(aes(group = interaction(age_band, gender))) +
#   geom_point(data = crimp2 %>% drop_na(),
#              aes(year, pred, colour = age_band, group = interaction(age_band, gender)))#+
#   # ylim(50, 90)

# The issue is that they all have the same gradients!

ggplot(data = tmp %>% drop_na(), aes(year, adm_rate_1k, colour= age_band))+
  geom_line(aes(group = interaction(age_band, gender))) +
  geom_point(data = crimp3 %>% drop_na(),
             aes(year, pred, colour = age_band, group = interaction(age_band, gender)))+
  ylim(50, 90)

# Factor? 

ggplot(data = tmp %>% drop_na(), aes(year, adm_rate_1k, colour= age_band))+
  geom_line(aes(group = interaction(age_band, gender))) +
  geom_point(data = crimp31 %>% drop_na(),
             aes(year, pred, colour = age_band, group = interaction(age_band, gender)))+
  ylim(50, 90)

# 3 way interactions: still linear (due to year being numeric, increasing by 1)
# but different gradients now

ggplot(data = tmp %>% drop_na(), aes(year, adm_rate_1k, colour= age_band))+
  geom_line(aes(group = interaction(age_band, gender))) +
  geom_point(data = crimp32 %>% drop_na(),
             aes(year, pred, colour = age_band, group = interaction(age_band, gender)))+
  ylim(50, 90)

"Should the modelling involve population projections - which we have data for"
"With this (rate) model, there is no need for the projections"
"Bulge in admission rates in 2008 and 2009 across all age groups"
"Is this what was found in CHSEO?"
"Yes: Conclusion: Austerity imposed 2010/11"
# Predictions --------------------------------------------------------

tail(tmp %>% ungroup %>% add_row(year = c(2015, 2016),
                   gender = rep("M", 2),
                   age_band = rep("20to44", 2) #, non-explicit
                   # admissions = rep(NA, 2),
                   # population  = rep(NA, 2),
                   # adm_rate_1k  =  rep(NA, 2),
                   # pred         =  rep(NA, 2)  
                     ))

# which means this (rate) model makes predictions independent of the future population
# whereas the count shouldn't (or can't).

names(crimp3)

population_estimates %>% filter(year == 2015)

future <- tmp %>%
  ungroup %>%
  add_row(year = c(2015, 2016, 2017),
          gender = rep("M", 3),
          age_band = rep("20to44", 3) #, non-explicit
          # admissions = rep(NA, 2),
          # population  = rep(NA, 2),
          # adm_rate_1k  =  rep(NA, 2)
          )

crimp4 <-  modelr::add_predictions(future, mod_nb_rate_3int) %>% 
  mutate(pred = exp(pred))
#  This will just be a linear extrapolation of trend


crimp5 <-  modelr::add_predictions(future, mod_nb_rate_int_catyear) %>% 
  mutate(pred = exp(pred))
# Obviously can't do forecasting when year is a factor

"Must use model with count of admissions, in order to utilise the pop projections
 from ONS"

ggplot(data = tmp %>% drop_na(), aes(year, adm_rate_1k, colour= age_band))+
  geom_line(aes(group = interaction(age_band, gender))) +
  geom_point(data = crimp4, # %>% drop_na(),
             aes(year, pred, colour = age_band, group = interaction(age_band, gender)))+
  ylim(50, 90)


