library(here)
source("scripts/1_gjamTime/normalization_predictors.R")

# Set working directory only if not already set
setwd(here::here())

# set calling script here
call_scrpt <- "scripts/project/1_gjamTime/call_gjamTime_test1.R"

## call normalize_predictor_parameters()
normalize_predictor_parameters()