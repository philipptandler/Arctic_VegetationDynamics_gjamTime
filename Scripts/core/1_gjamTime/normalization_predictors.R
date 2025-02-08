#' This script normalizes finds normalization parameters for all predictors
#' incl. interactions and normalizes raster

## sourcing general scripts ####
source("scripts/core/1_gjamTime/.gjamTime_Hfunctions.R")

# revieves a callscript and writes 
normalize_predictor_parameters <- function(call_scrpt){
  
  # get call
  call <- .initialize_and_validate_call(call_scrpt)
  
  # normalize for given predictors
  # for each variable write mean and sd in list
  
}
normalize_predictor_rasters <- function(call_scrpt){
  
  # get call
  call <- .initialize_and_validate_call(call_scrpt)
  
  # normalize for given predictors
  # for each variable calculate normalized raster thorugh mean and sd
  
}