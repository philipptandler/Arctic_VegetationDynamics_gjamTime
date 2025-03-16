#' This script normalizes raster

source("scripts/core/2_analysis/.analysis_Hfunctions.R")

#' argument is directory, folder that is subdir of 'where', or gjam Time calling
#' script. times is a logical vector or character vector that holds the respective
#' time labels according to gjamTime_validVariables
#' output_mask
normalize_predictor_rasters <- function(argument, times = NULL, output_mask = NULL){
  
  call <- .get_argument(argument, "call.rds", where = path_gjamTime_out)
  .normalize_predictor_rasters(call, times, output_mask)
  
}