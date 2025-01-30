#' This Script fits mulitple models of gjamTime to a timeseries of variables
#' given as spatial raster datasets
#' !!!! it is not advised to run the gjamTime model in an IDE unless rasters 
#' sizes are small enough !!!!

library(here)
# Set working directory only if not already set
if (getwd() != here::here()) {
  setwd(here::here())
}

#' the variable 'call' holds the Script that specifies the input for gjamTime
# set call script here
call_this_script <- "some/call_gjamTime_script.R"

# set replicates for multiple simulations here
n_replicates <- 10

# runs gjamTime independently
call_name <- NULL
for(run in 1:n_replicates){
  task_id <- run
  call_scrpt <- call_this_script
  source("scripts/run_gjamTime.R")
  call_name #TODO save call name and nreps in call object
}

#TODO summarize output
.summarize_gjamTime_output(script = call_this_script)
# or 
.summarize_gjamTime_output(folder = "some/outfolder")
source("some/script/that/summarizes.R")