library(here)
# Set working directory only if not already set
setwd(here::here())

source("config/config_local.R")
source("scripts/1_gjamTime/normalization_predictors.R")



## read system Arguments and set call
sysArgs <- commandArgs(trailingOnly = TRUE)

if(length(sysArgs) > 0){
  call_scrpt <- sysArgs[1]
}
if(!exists("call_scrpt")){
  stop("Missing Argument: calling script")
}

## call normalize_predictor_rasters()
normalize_predictor_rasters(call_scrpt)