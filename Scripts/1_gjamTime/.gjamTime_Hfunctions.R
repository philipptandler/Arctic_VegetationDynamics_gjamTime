source("scripts/__helper/.get_outfolder.R")
source("config/config_local.R")

.validate_call <- function(call_script){
  source(call_script)
  # validate name
  if(!exists("name") | length(name) == 0){name <- "gjamTime_model"}
  name <- .get_outfolder(path_gjamTime_out, name)
  
  # validate yvars
  if(!exists("yvars")){stop(paste("No yvars provided in", call_script))}
  
  #validate xvars
  if(!exists("xvars")){xvars <- NULL}
  
  # validate subset
  if(!exists("subset")){subset <- NULL}
  
  # validate subsample
  if(!exists("subsample")){
    warning("Not subsampling the spatial rasters might overflow the available memory")
    subsample <- NULL
  }
  
  # validate model
  if(!exists("model")){
    model <- list(
      termB <- FALSE,
      termR <- TRUE,
      termA <- TRUE)
  }
  
  # validate priors
  if(!exists("priors")){}
  
  # validate model_runntime
  
  
}

.initialize_and_validate_call <- function(call_script){
  
  ## initialize call
  
}