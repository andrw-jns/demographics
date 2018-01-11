######################################
"DEMOGRAPHICS:"
"TEST MIXED MODELS"
######################################

library(here)
library(stringr)
library(tidyverse)
library(extrafont) # for theme_strategy.
library(lme4)


names(speed_training )

ggplot(speed_training, aes(Days, Reaction, group = Subject))+
  geom_line(alpha = 0.4)
