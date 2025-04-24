## sourcing general scripts ####
source("scripts/core/2_analysis/.analysis_Hfunctions.R")

wobs_geospatial <- function(argument, ...){
  
  wobs_list <- .wobs_geospatial(argument, ...)
  
  wobs_list
}