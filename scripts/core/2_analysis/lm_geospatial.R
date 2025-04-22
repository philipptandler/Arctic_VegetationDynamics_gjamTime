## sourcing general scripts ####
source("scripts/core/2_analysis/.analysis_Hfunctions.R")

lm_geospatial <- function(y, x, ...){
  
  lm <- .lm_geospatial(y=y, x=x, ...)
  
  lm
}