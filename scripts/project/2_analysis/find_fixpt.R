library(here)
# Set working directory only if not already set
setwd(here::here())

source("config/config_local.R")
source("scripts/core/2_analysis/fixpt_geospatial.R")

arg <- "probe1_highorder2"
times_out = c(T,F,F,T,F,T)

## call fixpt_geospatial()
wstar <- fixpt_geospatial(argument = arg, times_out = times_out, data_type = "INT2S")
