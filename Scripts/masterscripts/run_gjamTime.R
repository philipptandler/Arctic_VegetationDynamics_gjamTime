#' This Script fits gjamTime to a timeseries of variables given as spatial
#' raster datasets
#' !!!! it is not advised to run the gjamTime model in an IDE unless raster
#' sizes are small enough !!!!
#' the variable 'call' holds the Script that specifies the input for gjamTime

library(here)
source("scripts/1_gjamTime/gjamTime_geospatial.R")

# Set working directory only if not already set
if (getwd() != here::here()) {
  setwd(here::here())
}

# set calling script here
call_this_script <- "scripts/1_gjamTime/call_gjamTime_test1.R"


## read system Arguments and set call
sysArgs <- commandArgs(trailingOnly = TRUE)

if(length(sysArgs) > 0){
  call_scrpt <- sysArgs[1]
  if(length(sysArgs) > 1){
    task_id <- as.integer(sysArgs[2])
  }
}
if(!exists("call_scrpt")){
  call_scrpt <- call_this_script
}
if(!exists("task_id")){
  task_id <- .default_call()$task_id
}


## call gjamTime_goespatial()


output <- gjamTime_geospatial(call_scrpt, task_id)
