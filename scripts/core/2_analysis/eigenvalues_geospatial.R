## sourcing general scripts ####
source("scripts/core/2_analysis/.analysis_Hfunctions.R")

eigenvalues_geospatial <- function(jacobian_list, ...){
  
  eigenvals_list <- .eigenvalues_geospatial(jacobian_list, ...)
  
  eigenvals_list
}