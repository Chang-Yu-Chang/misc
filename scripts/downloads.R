# Checl how many times a package has been downloaded 

#devtools::install_github("metacran/cranlogs")
library(cranlogs)
cran_downloads(when = "last-week", packages = c("ggplot2", "httr"))

cran_downloads(when = "last-week", packages = c("glmulti", "metafor", "leaps", "tidypredict"))