library(here)
source("scripts/1_gjamTime/gjamTime_geospatial.R")

# Set working directory only if not already set
setwd(here::here())

# set calling script here
call_scrpt <- "scripts/project/1_gjamTime/call_gjamTime_test1.R"

## call gjamTime_goespatial()
output <- gjamTime_geospatial(call_scrpt, .default_call()$task_id)