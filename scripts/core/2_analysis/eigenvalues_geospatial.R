## sourcing general scripts ####
source("scripts/core/2_analysis/.analysis_Hfunctions.R")

eigenvalues_geospatial <- function(argument, ...){
  
  eigenvals_list <- .eigenvalues_geospatial(argument, ...)
  
  eigenvals_list
}