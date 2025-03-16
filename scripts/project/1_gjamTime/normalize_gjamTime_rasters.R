library(here)
# Set working directory only if not already set
setwd(here::here())

source("config/config_local.R")
source("scripts/core/2_analysis/analysis_normalization_raster.R")

# set calling script here
call_scrpt <- "scripts/project/1_gjamTime/call_gjamTime_test1.R"

## call normalize_predictor_rasters()
normalize_predictor_rasters(call_scrpt)