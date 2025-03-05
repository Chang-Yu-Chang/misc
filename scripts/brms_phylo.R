#' This script goes through a tutorial of brms on a phylogenetic multilevel model
#' from here https://cran.r-project.org/web/packages/brms/vignettes/brms_phylogenetics.html#references

library(tidyverse)
library(ape)
library(brms)
library(tidybayes) # integrate bayes output and tidyverse


# Case 1. A simple model ----
phylo <- ape::read.nexus("https://paul-buerkner.github.io/data/phylo.nex")
data_simple <- read_delim(
    "https://paul-buerkner.github.io/data/data_simple.txt"
)
head(data_simple)

# variance covariance matrix
A <- ape::vcv.phylo(phylo)

# first phylogenetic multilevel model:
model_simple <- brm(
    phen ~ cofactor + (1|gr(phylo, cov = A)),
    data = data_simple,
    family = gaussian(),
    data2 = list(A = A),
    prior = c(
        prior(normal(0, 10), "b"),
        prior(normal(0, 50), "Intercept"),
        prior(student_t(3, 0, 20), "sd"),
        prior(student_t(3, 0, 20), "sigma")
    )
)

summary(model_simple)

# 
plot(model_simple, nvariables = 2, ask = FALSE)

plot(conditional_effects(model_simple), points = TRUE)

# Phylogenetic signals
hyp <- hypothesis(model_simple, "sd_phylo__Intercept^2 / (sd_phylo__Intercept^2 + sigma^2) = 0", class = NULL)
plot(hyp)
# Note that the phylogenetic signal is just a synonym of the intra-class correlation (ICC) used in the context phylogenetic analysis.


# Case 2. a phylogenetic model with repeated measurements ----
data_repeat <- read_delim("https://paul-buerkner.github.io/data/data_repeat.txt")
data_repeat$spec_mean_cf <- with(data_repeat, sapply(split(cofactor, phylo), mean)[phylo])
# Incorporate the variability in cofactor within species
data_repeat$within_spec_cf <- data_repeat$cofactor - data_repeat$spec_mean_cf
head(data_repeat)


model_repeat1 <- brm(
    phen ~ spec_mean_cf + (1|gr(phylo, cov = A)) + (1|species),
    data = data_repeat,
    family = gaussian(),
    data2 = list(A = A),
    prior = c(
        prior(normal(0,10), "b"),
        prior(normal(0,50), "Intercept"),
        prior(student_t(3,0,20), "sd"),
        prior(student_t(3,0,20), "sigma")
    ),
    sample_prior = TRUE, chains = 2, cores = 2,
    iter = 4000, warmup = 1000
)

summary(model_repeat1)
hyp <- hypothesis(model_repeat1, "sd_phylo__Intercept^2 / (sd_phylo__Intercept^2 + sd_species__Intercept^2 + sigma^2) = 0", class = NULL)
plot(hyp)


# Then fit it again using within_spec_cf as an additional predictor
model_repeat2 <- update(
    model_repeat1, formula = ~ . + within_spec_cf,
    newdata = data_repeat, chains = 2, cores = 2,
  iter = 4000, warmup = 1000
)
summary(model_repeat2)
# The result does not change much, meaning that no obvious relationship between intraspecific variance of cofactor and the phenotype
# So the PS remains mor or less the same
hyp <- hypothesis(model_repeat2, "sd_phylo__Intercept^2 / (sd_phylo__Intercept^2 + sd_species__Intercept^2 + sigma^2) = 0", class = NULL)
plot(hyp)

# Case 3. A phylogenetic count data model ----
data_pois <- read_delim("https://paul-buerkner.github.io/data/data_pois.txt")
data_pois$obs <- 1:nrow(data_pois)
head(data_pois)

# 3.1 Fit a poisson model
model_pois <- brm(
    phen_pois ~ cofactor + (1|gr(phylo, cov = A)) + (1|obs),
    data = data_pois, family = poisson("log"),
    data2 = list(A = A),
    chains = 2, cores = 2, iter = 4000,
    control = list(adapt_delta = 0.95)
)

plot(conditional_effects(model_pois), points = T) 

# Posterior prediction check
pp_check(model_pois)

# 3.2 Fit a norm model intead
model_norm <- brm(
    phen_pois ~ cofactor + (1|gr(phylo, cov = A)),
    data = data_pois, family = gaussian(),
    data2 = list(A = A),
    chains = 2, cores = 2, iter = 4000,
    control = list(adapt_delta = 0.95)
)

# The distribution of phenotype predicted by normal dist fails to resember the original dist of the phenotype 
pp_check(model_norm)
 
# 3.3 fit a negative binomial model
model_nb <- brm(
    phen_pois ~ cofactor + (1|gr(phylo, cov = A)),
    data = data_pois, family = negbinomial("log"),
    data2 = list(A = A),
    chains = 2, cores = 2, iter = 4000,
    control = list(adapt_delta = 0.95)
)
pp_check(model_nb)

# 3.4 Compare the three models
loo(model_pois, model_norm, model_nb)



