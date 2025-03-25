## sourcing general scripts ####
source("scripts/core/2_analysis/.analysis_Hfunctions.R")
fixpt_geospatial <- function(arg, out_folder = NULL, output_mask = NULL){
  
  wstar_list <- .fixpt_geospatial(arg, out_folder, output_mask)
  
  wstar_list
}