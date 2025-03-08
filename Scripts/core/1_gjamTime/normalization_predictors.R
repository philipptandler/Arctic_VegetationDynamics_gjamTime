#' This script normalizes finds normalization parameters for all predictors
#' incl. interactions and normalizes raster

## sourcing general scripts ####
source("scripts/core/1_gjamTime/.gjamTime_Hfunctions.R")

# recieves a call script and writes in a list
normalize_predictor_parameters <- function(call_scrpt, reset = F, overwrite = F){
  
  # get call
  call <- .initialize_and_validate_call(call_scrpt)
  
  # normalize for given predictors
  # for each variable write mean and sd in list
  .check_and_write_norm_param(call, reset, overwrite)
  
}

normalize_predictor_rasters <- function(call_scrpt){
  
  # get call
  call <- .initialize_and_validate_call(call_scrpt)
  
  # normalize for given predictors
  # for each variable calculate normalized raster thorough mean and sd
  
}