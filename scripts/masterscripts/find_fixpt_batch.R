library(here)
# Set working directory only if not already set
setwd(here::here())

source("config/config_local.R")
source("scripts/core/2_analysis/fixpt_geospatial.R")

## read system Arguments and set call
sysArgs <- commandArgs(trailingOnly = TRUE)

if(length(sysArgs) > 0){
  arg <- sysArgs[1]
  if(length(sysArgs) > 1){
    times_out = sysArgs[2]
  } else {times_out <- NULL}
}
if(!exists("arg")){
  stop("Missing Argument: arg")
}

## call fixpt_geospatial()
# first argument is arg, second times_out
wstar <- fixpt_geospatial(argument = arg, times_out = times_out, data_type = "INT2S",
                          output_mask = file.path(path_masks, "wildfire_mask_1978-2014.tif"))
