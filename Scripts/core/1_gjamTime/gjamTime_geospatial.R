## sourcing general scripts ####
source("scripts/core/1_gjamTime/.gjamTime_Hfunctions.R")

## main function to fit gjamTime to geospatial data ####
gjamTime_geospatial <- function(call_scrpt,
                                task_id=NULL,
                                saveOutput=TRUE,
                                savePlots=TRUE,
                                showPlots=FALSE){
  if(FALSE){
    # for debugging
    task_id=NULL
    saveOutput=TRUE
    savePlots=TRUE
    showPlots=FALSE
    fixWarning=T
  
  }
  
  ## initialize and validate call
  call <- .initialize_and_validate_call(call_scrpt, task_id)
  
  ## prepare outfolder 
  if(saveOutput || savePlots){
    call <- .prepare_gjamTime_outfolder(call, call_scrpt,
                                        create.if.notFound = TRUE) 
  }

  ## prepare geospatial rasters for model
  call <- .prepare_geodata(call)
  
  ## load predictors (xdata) as dataframe
  call$xdata <- .load_predictors(call)
  
  ## load response (ydata) as dataframe
  call$ydata <- .load_response(call)
  
  ## fit gjamTime
  output <- .fit_gjamTime(call, saveOutput, savePlots, showPlots, fixWarning = T)
  
  ## done
  return(output)
}

