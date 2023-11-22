#'

library(tidyverse)
library(cowplot)
library(janitor)
#source(here::here("analysis/00-metadata.R"))

# 1. simulate 100000 pools with a obvservation of 30% voted to candidate 1 ----
n_polls <- 10^5
n_votes <- 1000
p_poll <- 0.3

list_polls <- rep(list(NA), n_polls)
set.seed(1)

for (i in 1:n_polls) list_polls[[i]] <- runif(n_votes) < p_poll
polls <- tibble(poll_id = 1:n_polls, poll_result = list_polls %>% sapply(sum)) %>%
    mutate(poll_result = poll_result / n_votes)

# 
q_polls <- quantile(polls$poll_result, probs = c(0.05, 0.95))
diff(q_polls)

# 
polls %>% 
    ggplot() +
    geom_histogram(aes(x = poll_result), color = "black", fill = "white", binwidth = 0.005) +
    geom_vline(xintercept = q_polls, color = "maroon", linetype = 2) +
    #geom_vline(xintercept = p_poll, color = "black") +
    #annotate("text", x = p_poll, y = Inf, label = paste0("observed poll = ", p_poll), vjust = 2, hjust = 2) +
    scale_x_continuous(breaks = seq(0.2, 0.4, 0.02), minor_breaks = seq(0.2, 0.4, 0.005)) +
    scale_y_continuous(breaks = seq(0, 20000, 2000), minor_breaks = seq(0, 20000, 1000)) +
    theme_bw() +
    theme(
    ) +
    guides() +
    labs(x = "percentage of voted to candidate 1", y = "count")



# 2. ----

x


































