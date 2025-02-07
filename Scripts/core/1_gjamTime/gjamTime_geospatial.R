## sourcing general scripts ####
source("scripts/1_gjamTime/.gjamTime_Hfunctions.R")

## main function to fit gjamTime to geospatial data ####
gjamTime_geospatial <- function(call_scrpt, task_id = NULL){
  
  ## initialize and validate call
  call <- .initialize_and_validate_call(call_scrpt, task_id)
  
  ## prepare outfolder 
  call <- .prepare_gjamTime_outfolder(call, call_scrpt) # TODO mayne move to end
  
  ## prepare geospatial rasters for model
  call <- .prepare_geodata(call)
  
  ## load predictors (xdata) as dataframe
  call$xdata <- .load_predictors(call)
  
  ## load response (ydata) as dataframe
  call$ydata <- .load_response(call)
  
  ## fit gjamTime
  output <- .fit_gjamTime(call)
  
  ## done
  return(output)
}

