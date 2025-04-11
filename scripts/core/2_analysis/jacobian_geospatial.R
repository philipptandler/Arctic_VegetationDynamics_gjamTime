## sourcing general scripts ####
source("scripts/core/2_analysis/.analysis_Hfunctions.R")

jacobian_geospatial <- function(argument, ...){
  
  jacobian_list <- .jacobian_geospatial(argument, ...)
  
  jacobian_list
}