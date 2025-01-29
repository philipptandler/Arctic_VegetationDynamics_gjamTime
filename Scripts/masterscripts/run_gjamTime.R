#' This Script fits gjamTime to a timeseries of variables given as spatial
#' raster datasets
#' !!!! it is not advised to run the gjamTime model in an IDE unless raster
#' sizes are small enough !!!!
#' the variable 'call' holds the Script that specifies the input for gjamTime

## sourcing general scripts ####
source("config/config_local.R")
source("scripts/1_gjamTime/.gjamTime_Hfunctions.R")

# set calling script here
call_scrpt_local <- "some/call_script.R"



# set wd
library(here)
# Set working directory only if not already set
if (getwd() != here::here()) {
  setwd(here::here())
}

## read system Arguments and set call
sysArgs <- commandArgs(trailingOnly = TRUE)

if(length(sysArgs) > 0){
  call_scrpt <- sysArgs[1]
}
if(!exists("call_scrpt")){
  call_scrpt <- call_scrpt_local
}

## initialize and validate call
call <- .initialize_and_validate_call(call_scrpt)

## prepare geospatial rasters for model

## 