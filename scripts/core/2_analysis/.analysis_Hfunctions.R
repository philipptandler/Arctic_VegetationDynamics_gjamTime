
################################################################################
## retrieve parameters ####
################################################################################

# arg can be calling script, output or name in scripts/project/.parameters/(name)_output.rdata 
.get_argument <- function(arg, type, where = path_analysis){
  if(type != "call.rds" && type != "output.rdata" && type != "dir"){
    stop("invalid argument type:", type)
  }
  if(file.exists(arg)){
    helper_env <- new.env()
    source("scripts/core/1_gjamTime/.gjamTime_Hfunctions.R", local = helper_env)
    call <- helper_env$.initialize_and_validate_call(arg)
    call <- helper_env$.prepare_gjamTime_outfolder(call, arg, create.if.notFound = F)
    if(type == "call.rds") return(call)
    if(type == "output.rdata") return(.load_output_Rdata(call$outfolderBase))
    if(type == "dir") return(call$outfolderBase)
  } else if (dir.exists(arg)){
    if(type == "call.rds") return(readRDS(file.path(arg,
                                                    type)))
    if(type == "output.rdata")return(.load_output_Rdata(arg))
    if(type == "dir") return(arg)
  } else if (dir.exists(file.path(where, arg))){
    if(type == "call.rds") return(readRDS(file.path(where, arg, type)))
    if(type == "output.rdata") return(.load_output_Rdata(file.path(where, arg)))
    if(type == "dir") return(file.path(where, arg))
  } else if (file.exists(file.path("scripts/project/.parameters/",
                                   paste(arg, type, sep="_")
                                   ))){
    if(type == "call.rds") return(readRDS(file.path("scripts/project/.parameters/",
                                                    paste(arg, type, sep="_"))))
    if(type == "output.rdata") return(.load_output_Rdata("scripts/project/.parameters/"))
    if(type == "dir") stop("argument", arg, "requires type=='call.rds' or 'output.rdata'")
    } else {
    stop("Not found:", arg)
  }
}

# loads function in local env
.load_output_Rdata <- function(output_path){
  output_name <- file.path(output_path, "output.rdata")
  load(output_name)
  if(exists("output_save")){
    return(output_save)
  }
  if(exists("output_summary")){
    return(output_summary)
  }
}



################################################################################
## create and retrieve predictor rasters ####
################################################################################

# checks if raster exists in library and creates if not exists
.check_and_write_norm_rasters <- function(call, times, output_mask){
  #TODO
  #
}
# loads rasters from library
.load_predictor_rasters <- function(call, times, output_mask){
  #TODO
}

#' in path_analysis_
.normalize_predictor_rasters <- function(call, times, output_mask){
  .check_and_write_norm_rasters(call, times, output_mask)
  xlist <- .load_predictor_rasters(call, times, output_mask)
  xlist
}



################################################################################
## calculate fixed point ####
################################################################################
.fixpt <- function(beta, alpha, rho, x){
  #TODO
}


# argument can be calling_scrpt, outfolder or name (in scripts/project/.parameters/(name)_call.rds, (name)_output.rds)
.fixpt_geospatial <- function(argument,
                              out_folder,
                              output_mask,
                              times = NULL){
  
  # currently only for beta = FALSE, rho = TRUE, alpha = TRUE implemented
  cat("calling .fixpt_geospatial():
      Currently only implemented for beta=FALSE, rho=TRUE, alpha=TRUE, x_raster (predictors)")
  
  call <- .get_argument(argument, "call.rds", where = path_gjamTime_out)
  output <- .get_argument(argument, "output.rdata", where = path_gjamTime_out)
  
  # assign parameters
  beta = output$betaMu
  alpha = output$alphaMu
  rho = output$rhoMu
  
  # load_predictor_raster(call)
  x_list <- .normalize_predictor_rasters(call, times, output_mask) #TODO

  # calc_fixpt(alpha, rho, x)
  
  for ()
  
}