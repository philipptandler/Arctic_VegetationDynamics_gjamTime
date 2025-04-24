## sourcing general scripts ####
source("scripts/core/2_analysis/.analysis_Hfunctions.R")

raster_regression <- function(y, x, ...){
  
  lm <- .raster_regression(y=y, x=x, ...)
  
  lm
}