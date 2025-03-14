#'

library(tidyverse)
library(janitor)
library(brms)
library(lme4)

iris <- as_tibble(iris) %>% clean_names()


# 
mod_glm <- lmer(sepal_length ~ sepal_width + (1|species), data = iris)
mod_brm <- brm(
    sepal_length ~ sepal_width + (1|species), 
    data = iris,
    prior = prior(normal(0, 1))
)

summary(mod_glm)
pp_check(mod_brm)
conditional_effects(mod_brm)
