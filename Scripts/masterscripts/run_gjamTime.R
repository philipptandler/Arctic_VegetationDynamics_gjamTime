#' This Script fits gjamTime to a timeseries of variables given as spatial
#' raster datasets
#' !!!! it is not advised to run the gjamTime model in an IDE unless raster
#' sizes are small enough !!!!
#' the variable 'call' holds the Script that specifies the input for gjamTime

library(here)
## sourcing general scripts ####
source("config/config_local.R")
source("scripts/1_gjamTime/.gjamTime_Hfunctions.R")

# set calling script here
call_this_script <- "scripts/1_gjamTime/call_gjamTime_test1.R"

# Set working directory only if not already set
if (getwd() != here::here()) {
  setwd(here::here())
}

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

## initialize and validate call
call <- .initialize_and_validate_call(call_scrpt, task_id)

## prepare geospatial rasters for model

## 