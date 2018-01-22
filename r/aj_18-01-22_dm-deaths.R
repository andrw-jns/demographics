######################################
"DEMOGRAPHICS:"
"DEATH PROJECTIONS"
######################################

library(tidyverse)
library(janitor)

deaths_female <- read_csv("data/2014 SNPP Deaths females.csv") %>% 
  clean_names()

deaths_male <- read_csv("data/2014 SNPP Deaths males.csv") %>% 
  clean_names()

deaths_all <- bind_rows(deaths_female, deaths_male)

deaths_all <- deaths_all %>% filter(age_group != "All ages") %>%
  mutate(age_group = ifelse(age_group == "90 and over", "90", age_group)) %>% 
  # 90 holds all persons over 90
  mutate(age_group = as.numeric(age_group))

# quick purrr function:
round_sum <- compose(round, sum)

deaths_smry <- deaths_all %>%
  group_by(age_group, sex) %>% 
  summarise_at(vars(starts_with("x")), funs(round_sum)) %>% 
  arrange(sex, age_group)


