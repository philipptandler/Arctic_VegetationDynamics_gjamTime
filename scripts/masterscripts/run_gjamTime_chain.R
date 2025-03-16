#' This Script fits mulitple models of gjamTime to a timeseries of variables
#' given as spatial raster datasets
#' !!!! it is not advised to run the gjamTime model in an IDE unless rasters 
#' sizes are small enough !!!!

library(here)
# Set working directory only if not already set
setwd(here::here())

#' the variable 'call' holds the Script that specifies the input for gjamTime
# set call script here
call_this_script <- "scripts/project/1_gjamTime/call_gjamTime_test1.R"

# set replicates for multiple simulations here
n_replicates <- 10

# runs gjamTime independently
for(run in 1:n_replicates){
  cat("run_gjamTime_batch run", run, "out of", n_replicates, "\n")
  task_id <- run
  call_scrpt <- call_this_script
  source("scripts/masterscripts/run_gjamTime_batch.R")
}

# summarize output
arg <- call_this_script
source("scripts/masterscripts/summarize_gjamTime_batch.R")

# and calculate fixed point
source("scripts/masterscripts/find_fixpt_batch.R")
