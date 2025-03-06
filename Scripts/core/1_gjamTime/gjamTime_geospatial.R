## sourcing general scripts ####
source("scripts/core/1_gjamTime/.gjamTime_Hfunctions.R")

## main function to fit gjamTime to geospatial data ####
gjamTime_geospatial <- function(call_scrpt,
                                task_id=NULL,
                                saveOutput=TRUE,
                                savePlots=TRUE,
                                showPlots=FALSE){
 
  output <- .gjamTime_geospatial(call_scrpt,
                                 task_id,
                                 saveOutput,
                                 savePlots,
                                 showPlots)

  return(output)
}

