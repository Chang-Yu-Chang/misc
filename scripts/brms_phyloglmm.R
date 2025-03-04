#' This script goes through a tutorial of brms on a phylogenetic multilevel model
#' from here https://cran.r-project.org/web/packages/brms/vignettes/brms_phylogenetics.html#references

library(tidyverse)
library(ape)
library(brms)
library(tidybayes) # integrate bayes output and tidyverse


# Case 1. A simple model ----
phylo <- ape::read.nexus("https://paul-buerkner.github.io/data/phylo.nex")
data_simple <- read.table(
    "https://paul-buerkner.github.io/data/data_simple.txt",
    header = TRUE
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
hyp <- "sd_phylo__Intercept^2 / (sd_phylo__Intercept^2 + sigma^2) = 0"
(hyp <- hypothesis(model_simple, hyp, class = NULL))
plot(hyp)
# Note that the phylogenetic signal is just a synonym of the intra-class correlation (ICC) used in the context phylogenetic analysis.


# Case 2. a phylogenetic model with repeated measurements ----
data_repeat <- read.table(
    "https://paul-buerkner.github.io/data/data_repeat.txt",
    header = TRUE
)
data_repeat$spec_mean_cf <- with(data_repeat, sapply(split(cofactor, phylo), mean)[phylo])
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

hyp <- paste(
    "sd_phylo__Intercept^2 /",
    "(sd_phylo__Intercept^2 + sd_species__Intercept^2 + sigma^2) = 0"
)
(hyp <- hypothesis(model_repeat1, hyp, class = NULL))
plot(hyp)

# Incorporate the variability in cofactor within species
data_repeat$within_spec_cf <- data_repeat$cofactor - data_repeat$spec_mean_cf

# Then fit it again using within_spec_cf as an additional predictor
model_repeat2 <- update(
    model_repeat1, formula = ~ . + within_spec_cf,
    newdata = data_repeat, chains = 2, cores = 2,
  iter = 4000, warmup = 1000
)

summary(model_repeat2)
# The result does not change much, meaning that no obvious relationship between intraspecific variance of cofactor and the phenotype
# So the PS remains mor or less the same
hyp <- paste(
    "sd_phylo__Intercept^2 /",
    "(sd_phylo__Intercept^2 + sd_species__Intercept^2 + sigma^2) = 0"
)
(hyp <- hypothesis(model_repeat2, hyp, class = NULL))







