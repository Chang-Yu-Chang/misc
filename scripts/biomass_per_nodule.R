#' This tackles the problems of symbiotic benefits computed as 
#' the ratio of plant fitness and rhizobia nodule number

library(tidyverse)
set.seed(1)

mu = 1 # expected mean
sigma = 0.1 # expected sd

tb <- tibble(
    n_nodule = rpois(10000, lambda = 50),
    #biomass = n_nodule /50 + rgamma(10000, shape = mu^2/sigma^2, scale = sigma^2/mu),
    biomass = rgamma(10000, shape = mu^2/sigma^2, scale = sigma^2/mu),
    m_over_n = biomass/n_nodule
)

# Biomass and nodule number is not correlated
lm(biomass ~ n_nodule, data = tb) %>% broom::tidy()

tb %>% 
    filter(n_nodule != 0) %>% 
    ggplot() +
    geom_point(aes(x = n_nodule, y = m_over_n), position = position_dodge2(width = 0.1), shape = 21) +
    theme_bw() 

# Variance in the ration decreases as the number of nodule increases
tb %>% 
    group_by(n_nodule) %>% 
    summarize(var_mn = var(m_over_n))

tb %>% 
    filter(n_nodule != 0) %>% 
    ggplot() +
    geom_point(aes(x = n_nodule, y = biomass), position = position_dodge2(width = 0.1), shape = 21) +
    theme_bw() 


