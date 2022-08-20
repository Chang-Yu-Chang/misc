library(tidyverse)
library()
library(rbenchmark)

n <- 1000

benchmark(
    "paste" = {
        x <- data.frame(a = 1, b = 1)
        for (i in 1:n) {
            x <- bind_rows(x, data.frame(a = i, b = i))
        }
    },
    "overwrite" = {
        x <- data.frame(a = rep(NA, n), b = rep(NA, n))
        for (i in 1:n) {
            x$a[i] <- i
            x$b[i] <- i
        }
    }
)

x <- data.frame(a = 1, b = 1)
        for (i in 1:n) {
            x <- bind_rows(x, data.frame(a = i, b = i))
        }

x <- data.frame(a = rep(NA, n), b = rep(NA, n))
for (i in 1:n) {
    x$a[i] <- i
    x$b[i] <- i
}

x
