library(here)
# Set working directory only if not already set
setwd(here::here())

source("config/config_local.R")
source("scripts/core/2_analysis/wobs_geospatial.R")

arg <- "test_higherorder1_alphaNeg1"

## call fixpt_geospatial()
wobs <- wobs_geospatial(argument = arg)
