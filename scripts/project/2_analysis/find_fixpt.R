library(here)
# Set working directory only if not already set
setwd(here::here())

source("config/config_local.R")
source("scripts/core/2_analysis/fixpt_geospatial.R")

arg <- "data/gjamTime/out/somefolder/"

## call fixpt_geospatial()
wstar <- fixpt_geospatial(arg)
