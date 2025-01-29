#' This Script fits mulitple models of gjamTime to a timeseries of variables
#' given as spatial raster datasets
#' !!!! it is not advised to run the gjamTime model in an IDE unless rasters 
#' sizes are small enough !!!!

# set wd
library(here)
# Set working directory only if not already set
if (getwd() != here::here()) {
  setwd(here::here())
}

#' the variable 'call' holds the Script that specifies the input for gjamTime
# set call script here
call <- "some/call_gjamTime_script.R"

# set replicates for multiple simulations
n_replicates <- 200

call_name <- NULL
# runs gjamTime independent
for(run in 1:n_replicates){
  task_id <- run
  call_scrpt <- call
  source("scripts/run_gjamTime.R")
  call_name #TODO save call name and nreps in call object
}

#TODO summarize output
