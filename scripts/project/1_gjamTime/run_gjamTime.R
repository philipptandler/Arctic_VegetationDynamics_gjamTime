library(here)
# Set working directory only if not already set
setwd(here::here())

source("config/config_local.R")
source("scripts/core/1_gjamTime/gjamTime_geospatial.R")

# set calling script here
call_scrpt <- "scripts/project/1_gjamTime/call_probe1_base_exploreVMSc.R"


## call gjamTime_goespatial()
output <- gjamTime_geospatial(call_scrpt,
                              saveOutput = TRUE,
                              task_id = 1,
                              savePlots = TRUE,
                              showPlots = FALSE)

