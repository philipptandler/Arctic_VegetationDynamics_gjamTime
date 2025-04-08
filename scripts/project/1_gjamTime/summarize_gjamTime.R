library(here)
# Set working directory only if not already set
setwd(here::here())

source("config/config_local.R")
source("scripts/core/1_gjamTime/gjamTime_summary.R")

# set calling script here
# call_scrpt <- "scripts/project/1_gjamTime/call_gjamTime_test1.R"
# call_scrpt <- "data/gjamTime/out/test_higherorder1_alphaNeg1/copy_call_gjamTime_test1.R"
argument <- "probe1_base_regular-1"

## call gjamTime_goespatial()
output_summary <- gjamTime_summary(argument=argument, cp_to_repo=T, summarize_call=T)

