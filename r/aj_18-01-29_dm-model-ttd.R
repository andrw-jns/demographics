##########################
"DEMOGRAPHICS:"
"MODEL WITH TIME TO DEATH"
##########################

test <- ip_base %>% 
  filter(year == 2014)

test2 <- left_join(test, mort_groups, by = c("gender", "age_group"))

test3 <- test2 %>%
  mutate(heads = ifelse(
    proximity_death %in% 0:23,
    round(population * death_rate / 1000),
    round(population * (1 - death_rate / 1000))
  ))

# Here just the 2014 death rates (applied to population which may be from different years)

test4 <- test3 %>% 
  mutate(ttd = case_when(
    is.na(.$proximity_death)     ~ "n",   # No death
    .$proximity_death %in% 0:11  ~ "1", # Activity in last year of life
    .$proximity_death %in% 12:23 ~ "2"  # Activity in penultimate year of life
  ))

ttd <- test4 %>% 
  group_by(gender, age_group, heads, ttd) %>%
  summarise(admissions = sum(admissions)) 

ggplot(ttd %>% drop_na(), aes(ttd, admissions/heads, colour = age_group))+
  geom_point()+
  geom_line(aes(group = interaction(age_group, gender)))


ggplot(ttd %>% drop_na(), aes(admissions))+
  geom_histogram(bins = 100)


# Model -------------------------------------------------------------------

# Geometric distribution, therefore a negative binomial with dispersion parameter set to 1.


mod_simple <- MASS::glm.nb(admissions ~ age_group*ttd + gender + offset(log(heads)),
                           data = ttd %>% drop_na(), init.theta = 1)

summary(mod_simple)

look <- model.matrix(admissions ~ age_group*ttd + gender + offset(log(heads)), ttd)

broom::tidy(mod_simple)
broom::glance(mod_simple)
broom::augment(mod_simple)
broom::confint_tidy(mod_simple)


ttd %>% modelr::add_predictions(mod_simple) %>%
  mutate(pred = exp(pred)) %>% 
  mutate(closeness = pred/admissions)

# Not all that great at predicting admissions when in ttd == n

"CAREFUL: MODELLING THREE CAT VARIABLES"
# Mixed Effects:

ttd <- ttd %>%
  group_by(ttd) %>%
  drop_na() %>% 
  mutate(id = row_number())

library(lme4)
mod_me <- glmer.nb(admissions ~ age_group + gender + ttd + (ttd | id) + offset(log(heads)),
                            data = ttd %>% drop_na())

"SURELY OVERFITTED"
mod_overfit <- ttd %>%
  modelr::add_predictions(mod_me) %>%
  mutate(pred = exp(pred)) %>% 
  mutate(closeness = pred/admissions)


broom::tidy(mod_me)
broom::glance(mod_me)
broom::glance(mod_simple)
broom::augment(mod_me)
broom::confint_tidy(mod_me)
