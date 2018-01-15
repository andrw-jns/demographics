####################################
"DEMOGRAPHICS:"
"MIXED EFFECTS MODEL EXPERIMENTATION"
####################################


library(modelr)

# At individual level
test1 <- ip_base %>% filter_at(vars(gender, age_band), all_vars(!is.na(.))) 
# %>% filter(proximity_death < 1)

test1 %>% 
  group_by(age_group) %>% 
  summarise(sum(admissions))

ggplot(test1, aes(proximity_death, admissions))+
  geom_point()

# Models ------------------------------------------------------------------


mod_nb_count <- MASS::glm.nb(admissions ~ age_band + gender + year + offset(log(population)),
                             data = tmp %>% drop_na)

mod_update_count <- MASS::glm.nb(admissions ~ age_band + gender + year + log(population),
                                 data = tmp %>% drop_na)

mod_nb_rate <- MASS::glm.nb(adm_rate_1k ~ age_band + gender + year, # + offset(log(population)),
                            data = tmp %>% drop_na)
# The rate model fits much better than the count, maybe due to distribution

mod_nb_rate_int <- MASS::glm.nb(adm_rate_1k ~ age_band*gender + year, # + offset(log(population)),
                                data = tmp %>% drop_na)


mod_log_n <-  glm(log(admissions) ~ age_band + gender + year + log(population),
                  data = tmp %>% drop_na)

mod_log_2 <-  glm(admissions ~ age_band + gender + year + log(population),
                  data = tmp %>% drop_na,
                  family = gaussian(link = "log"))


mod_poisson <- glm(admissions ~ age_band + gender + year + log(population),
                   data = tmp %>% drop_na, family = "poisson")


mod_glm1 <- glm(admissions ~ age_band + gender + year + population,
               data = tmp %>% drop_na)

mod_glm2 <- glm(admissions ~ age_band + gender + year + log(population),
                data = tmp %>% drop_na)

# Plots 1 ----------------------------------------------------------------

ggplot(tmp %>% drop_na %>% filter(year == "2014"), aes(log(population), admissions))+
  geom_point()

# BIC ---------------------------------------------------------------------

# only useful between models of the same spec (ie. for testing when switching covariates)
BIC(mod_nb_count)
BIC(mod_poisson)
BIC(mod_update_count)
BIC(mod_nb_rate)
BIC(mod_nb_rate_int)
BIC(mod_glm)
BIC(mod_glm2)
BIC(mod_log_n)
BIC(mod_log_2)



# Predictions -------------------------------------------------------------

dictify_log <- function(df, model_name){
  
  x <- add_predictions(df, model_name) %>% 
  mutate(pred = exp(pred))
  x
}


dictify <- function(df, model_name){
  
  x <- add_predictions(df, model_name)
  x
}

dict_count <- dictify_log(tmp, mod_nb_count)
dict_upda  <- dictify_log(tmp, mod_update_count)
dict_rate  <- dictify_log(tmp, mod_nb_rate)
dict_rtint <- dictify_log(tmp, mod_nb_rate_int)
dict_log   <- dictify_log(tmp, mod_log_n)
dict_log2  <- dictify_log(tmp, mod_log_2)
dict_pois  <- dictify_log(tmp, mod_log_2)


# Plot --------------------------------------------------------------------

ggplot(data = tmp %>% drop_na(), aes(year, admissions, colour= age_band))+
  geom_line(aes(group = interaction(age_band, gender))) +
  geom_point(data = dict_count,
             aes(year, pred, colour = age_band, group = interaction(age_band, gender)))

trend_plot <- function(dict_df){
  plot_this <- ggplot(data = tmp %>% drop_na(), aes(year, admissions, colour= age_band))+
    geom_line(aes(group = interaction(age_band, gender))) +
    geom_point(data = dict_df,
               aes(year, pred, colour = age_band, group = interaction(age_band, gender)))
  plot_this
}


trend_rate_plot <- function(dict_df){
  plot_this <- ggplot(data = tmp %>% drop_na(), aes(year, adm_rate_1k, colour= age_band))+
    geom_line(aes(group = interaction(age_band, gender))) +
    geom_point(data = dict_df,
               aes(year, pred, colour = age_band, group = interaction(age_band, gender)))
  plot_this
}

trend_rate_plot(dict_count)
trend_rate_plot(dict_upda) # less good
trend_rate_plot(dict_rate)+ ylim(50,90) # still not close on 20to44
trend_rate_plot(dict_rtint)+ ylim(50,90) # much closer but all same gradient still
trend_plot(dict_log) # LogNormal pretty good, though misses 20to44
trend_plot(dict_log2) # not terrible
trend_plot(dict_pois)


# Mixed effects nb --------------------------------------------------------
me_tmp <- tmp %>% 
  group_by(year) %>% 
  mutate(id = row_number()) %>% 
  arrange(id)

library(lme4)
m3 <- glmer.nb(admissions ~ age_band + gender + (year | id),
               data = me_tmp %>% drop_na())

# seems it may not like interaction terms.
# Need to rescale admissions if this way

m3 <- glmer.nb(adm_rate_1k ~ gender + age_band + (year | id),
               data = me_tmp %>% drop_na())


m4 <- glmer.nb(adm_rate_1k ~ gender + age_band + year + I(year^2)+(year | id),
               data = me_tmp %>% drop_na())

# If interactions and non-linear terms don't work, then try glmmTMB function
# library(glmmTMB)
# 
# # m1 <- glmmTMB(SiblingNegotiation ~ SexParent + ArrivalTime + (1 | Nest), data = Owls, family = nbinom1)
# m2 <- glmmTMB(SiblingNegotiation ~ SexParent + ArrivalTime + (1 | Nest), data = Owls, family = nbinom2)

# See Steven's model for help


dict_me  <- dictify_log(me_tmp %>% drop_na(), m3)
dict_me_year <- dictify_log(me_tmp %>% drop_na(), m4)

trend_rate_plot(dict_me)+ ylim(50, 90)
# Good job but still struggling with the 20to44 age band.
# Either because Male/Female reverse (due childbirth)
# OR larger group than others.

# Can see the model is not picking up the bump in activity before austerity.
# Which is fine since we can't be predicting that
# Needs some non-linear flexibility?

trend_rate_plot(dict_me)+ ylim(50, 90)

"IN ANY CASE, IT MUST TIE IN WITH "
