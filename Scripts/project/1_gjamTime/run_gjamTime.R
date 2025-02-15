library(here)
# Set working directory only if not already set
setwd(here::here())

source("config/config_local.R")
source("scripts/core/1_gjamTime/gjamTime_geospatial.R")

# set calling script here
call_scrpt <- "scripts/project/1_gjamTime/call_gjamTime_test1.R"

## call gjamTime_goespatial()
output <- gjamTime_geospatial(call_scrpt,saveOutput=F,
                              savePlots=F,
                              showPlots=T)

