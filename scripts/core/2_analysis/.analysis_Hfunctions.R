
################################################################################
## retrieve parameters ####
################################################################################




################################################################################
## retrieve predictor rasters ####
################################################################################





################################################################################
## calculate fixed point ####
################################################################################

# returns call or output from gjamTime run
.load_gjamTime_out <- function(arg, file){
  if(dir.exists(arg)){
    return(file.path(arg, file))
  } else if(file.exists(file.path("scripts/project/.parameters/", paste(arg, file, sep="_"))
                        )){
    return(file.path("scripts/project/.parameters/", paste(arg, file, sep="_")))
  } else {
    stop("Input not found:", arg)
  }
}

# loads function in local env
.load_output_Rdata <- function(output_path){
  load(output_path)
  if(exists("output_save")){
    return(output_save)
  }
  if(exists("output_summary")){
    return(output_summary)
  }
}

.fixpt_geospatial <- function(arg, out_folder, output_mask){
  call_path <- .load_gjamTime_out(arg, "call.rds")
  call <- readRDS(call_path)
  output_path <- .load_gjamTime_out(arg, "output.Rdata")
  output <- .load_output_Rdata(output_path) #loads output_summary or output_save
  
  # load_predictor_raster(call)
  
  ## 
  # calc_fixpt(alpha, rho, x)
  
  
  
}