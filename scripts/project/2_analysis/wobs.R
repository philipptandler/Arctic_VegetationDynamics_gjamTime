library(here)
# Set working directory only if not already set
setwd(here::here())

source("config/config_local.R")
source("scripts/core/2_analysis/wobs_geospatial.R")

arg <- "probe1_highorder2"
times_out = c(T,F,F,F,F,F)

## call fixpt_geospatial()
wobs <- wobs_geospatial(argument = arg, times_out = times_out)
