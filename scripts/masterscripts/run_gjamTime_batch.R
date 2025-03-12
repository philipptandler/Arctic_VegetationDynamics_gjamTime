#' This Script fits gjamTime to a timeseries of variables given as spatial
#' raster datasets
#' !!!! it is not advised to run the gjamTime model in an IDE unless raster
#' sizes are small enough !!!!
#' the variable 'call' holds the Script that specifies the input for gjamTime

library(here)

# Set working directory only if not already set
if (getwd() != here::here()) {
  setwd(here::here())
}

source("config/config_local.R")
source("scripts/core/1_gjamTime/gjamTime_geospatial.R")

## read system Arguments and set call
sysArgs <- commandArgs(trailingOnly = TRUE)

if(length(sysArgs) > 0){
  call_scrpt <- sysArgs[1]
  if(length(sysArgs) > 1){
    task_id <- as.integer(sysArgs[2])
  }
}
if(!exists("call_scrpt")){
  stop("Missing Argument: calling script")
}
if(!exists("task_id")){
  task_id <- .default_call()$task_id
}

## call gjamTime_goespatial()
output <- gjamTime_geospatial(call_scrpt, task_id)
