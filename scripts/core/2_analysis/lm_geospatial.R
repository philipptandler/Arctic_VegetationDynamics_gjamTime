## sourcing general scripts ####
source("scripts/core/2_analysis/.analysis_Hfunctions.R")

lm_geospatial <- function(r_list, p_list, ...){
  
  lm <- .lm_geospatial(r_list=r_list, p_list=p_list, ...)
  
  lm
}