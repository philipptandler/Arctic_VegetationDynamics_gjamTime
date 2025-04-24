## sourcing general scripts ####
source("scripts/core/2_analysis/.analysis_Hfunctions.R")

wrate_geospatial <- function(files, ...){
  
  wrate_list <- .wrate_geospatial(files, ...)
  
  wrate_list
}