## sourcing general scripts ####
source("config/config_local.R")
source("scripts/1_gjamTime/.gjamTime_Hfunctions.R")

## main function to fit gjamTime to geospatial data ####
gjamTime_geospatial <- function(call_scrpt, task_id){
  ## initialize and validate call
  call <- .initialize_and_validate_call(call_scrpt, task_id)
  
  ## prepare geospatial rasters for model
  call <- .prepare_geodata(call)
  
  ## load predictors (xdata) as dataframe
  call <- .load_predictors(call)
  
  ## load response (ydata) as dataframe
  call <- .load_response(call)
  
  ## fit gjamTime
  call <- .fit_gjamTime()
  
  ## done
  return(call)
}

