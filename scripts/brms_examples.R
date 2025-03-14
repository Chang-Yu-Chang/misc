#' This script goes through the examples in 
#' https://journal.r-project.org/archive/2018/RJ-2018-017/RJ-2018-017.pdf
#' and 
#' https://cran.r-project.org/web/packages/brms/vignettes/brms_distreg.html

library(tidyverse)
library(brms)
library(tidybayes) # integrate bayes output and tidyverse

zinb <- read_csv("http://stats.idre.ucla.edu/stat/data/fish.csv")
zinb$camper <- factor(zinb$camper, labels = c("no", "yes"))
zinb

# 1. Catching fish. Zero inflated ----
## 1.1 Fit a zero inflated poisson model
mod_zinb1 <- brm(
    count ~ persons + child + camper, 
    data = zinb,
    family = zero_inflated_poisson("log")
)

summary(mod_zinb1)
conditional_effects(mod_zinb1) # Graphic summary

## 1.2. Additionally predict the zero-inflation prob by the number of children
mod_zinb2 <- brm(
    bf(
        count ~ persons + child + camper,
        zi ~ child
    ),
    data = zinb,
    family = zero_inflated_poisson()
)

summary(mod_zinb2)
conditional_effects(mod_zinb2)

plot(conditional_effects(mod_zinb2), ask = FALSE)

## Compare model fit via leave-one-out cross validation
loo(mod_zinb1, mod_zinb2, moment_match = T)

# 2. unequal variance ----
set.seed(42)
group <- rep(c("treat", "placebo"), each = 30)
symptom_post <- c(rnorm(30, mean = 1, sd = 2), rnorm(30, mean = 0, sd = 1))
dat1 <- data.frame(group, symptom_post)
head(dat1)
dat1 %>% 
    ggplot() +
    geom_jitter(aes(x = group, y = symptom_post), width = .1) +
    coord_cartesian(clip = "off") +
    theme_bw() +
    theme() +
    guides() +
    labs()
    

mod1 <- brm(
    bf(
        symptom_post ~ group,
        sigma ~ group
    ),
    data = dat1,
    family = gaussian()
)

summary(mod1)
plot(mod1, nvariables = 2, ask = FALSE)
plot(conditional_effects(mod1), points = TRUE)

## Test hypothesis 
hyp <- hypothesis(mod1, c("exp(sigma_Intercept) = 0", "exp(sigma_Intercept + sigma_grouptreat) = 0"))
plot(hyp)
hyp <- hypothesis(mod1, c(" exp(sigma_Intercept + sigma_grouptreat) > exp(sigma_Intercept)"))
plot(hyp)

# get_variables(mod1)
# mod1 %>% 
#     spread_draws(b_grouptreat, b_Intercept, b_sigma_Intercept) %>% 
#     median_qi()


# 3. Example of multilevel selection ----
# This also uses spline to guess the relationship between a predictor and the response 
set.seed(42)
dat_smooth <- mgcv::gamSim(eg = 6, n = 200, scale = 2, verbose = FALSE) %>% as_tibble

mod_smooth <- brm(
    bf(
        y ~ s(x1) + s(x2) + (1|fac),
        sigma ~ s(x0) + (1|fac)
    ),
    data = dat_smooth,
    family = gaussian(),
    chains = 2, control = list(adapt_delta = 0.95)
)

summary(mod_smooth)
plot(conditional_effects(mod_smooth), points = T, ask = F)






