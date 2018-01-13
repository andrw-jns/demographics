######################################
"DEMOGRAPHICS:"
"TEST MIXED MODELS"
######################################

" A regular lm is actually okay if you have very few clusters, and put the cluster id in the model as a fixed effect." # MC

library(here)
library(stringr)
library(tidyverse)
# library(extrafont) # for theme_strategy.
library(lme4)
install.packages("lme4")
install.packages("ggeffects")
names(speed_training )
library(ggeffects)
install.packages("purrr")
ggplot(speed_training, aes(Days, Reaction, group = Subject))+
  geom_line(alpha = 0.4)


glmer.nb()

# From M Clark tutorial ---------------------------------------------------

library(Matrix)
set.seed(8675309)
tp = 4 # number of timepoints
n = 2500 # number of individuals
sigmasq = 1 # residual variance
rho = .7 # rho of AR1 model
intercept = .2 # intercept
time_beta = .5 # time effect
treat_beta = -.5 # treatment effect
intsd = .5 # intercept standard devation
timesd = .25 # slope of time variable standard deviation

# variables:
time = rep(0:(tp-1), n) # time
id = rep(1:n, e=tp) # group id
treatment = gl(2, n/2, labels=c('control', 'treatment'))[id] # cluster level variable
re_int = rnorm(n, sd=intsd) # random intercepts
re_time = rnorm(n, sd=timesd)

##
ar1 = bandSparse(tp, tp, 0:(tp-1), list(rep(1 , tp),
                                        rep(rho , tp-1),
                                        rep(rho^2, tp-2),
                                        rep(rho^3, tp-3)), symmetric=T)
Sig = kronecker(diag(1, n), ar1)
Sig[1:10, 1:10] # inspect, note that dots are 0s

# e = MASS::mvrnorm(1, mu=rep(0, n*tp), Sigma=sigmasq*Sig) # residual error
e = c(replicate(n, MASS::mvrnorm(1, mu=rep(0, tp), Sigma=sigmasq*ar1))) # much faster


# Put it all together:
y = (intercept + re_int[id]) +
  (time_beta + re_time[id])*time +
  treat_beta*(treatment=='treatment') +
  e
d = data.frame(y, time, treatment, id)

d <- tibble::as.tibble(d)

# Plot

ggplot(d, aes(time, y, group = id))+
  geom_line(alpha = 0.04)

# Linear model (Ignore data dependency):
lm_mod = lm(y ~ time + treatment, data=d, x=T)

lm_mod$coefficients
y_test_control <- lm_mod$coefficients[1]+ 

tibble(
  id = rep(11111, 4)
  
       )



  
  