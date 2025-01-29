library(dplyr)

source("scripts/__helper/.get_outfolder.R")
source("scripts/1_gjamTime/.gjamTime_defaultSettings.R") 


# .is_proper_list <- function(obj) {
#   # Check if the object is a list
#   is.list(obj) &&
#     # Check if all elements have names
#     !is.null(names(obj)) &&
#     # Check if none of the names are empty
#     all(names(obj) != "")
# }

.is_equivalent_to_default <- function(entry){
  return(is.null(entry) || is.na(entry) || isFALSE(entry) ||
           length(entry) == 0 || entry == 0 || entry == "")
}


.check_unique_variables <- function(interactionVec, uniqueVec) {
  # Extract base variables
  extracted_vars <- unique(unlist(lapply(interactionVec, function(x) {
    # Split by ":" or extract base of power notation
    unique(gsub("\\d+$", "", unlist(strsplit(x, ":"))))  
  })))
  
  if(!all(extracted_vars %in% uniqueVec)){
    stop("Interaction variables must exist as single variables: ", 
         paste(setdiff(extracted_vars, uniqueVec), collapse = ", "))
  }
}


#### old down: >>>>>>>>




.validate_variables <- function(provided_list, master_list){
  if(!all(names(provided_list) %in% names(master_list))){
    stop("Invalid entries in variable list (yvars or xvars)")
  }
  default_vars <- .default_variables()
  valid_variable <- FALSE

  
  
  interaction_variables <- c()
  for(name in names(provided_list)){
    # for Lat/Lon and interactions
    if(name == "x" || name == "y" || name == "interaction"){
      if(name == "x" &
         (length(provided_list$x) > 1 || !provided_list$x )){
        
      }
      
    } else { # for all other variables with var_list as reference
      
      # valid group of variables 
      if(!(name %in% names(var_list))){
        stop("Invalid variable group: ", name)
      }
      # valid variables
      if(length(provided_list[[name]]) != 0 &
         !all(provided_list[[name]] %in% var_list[[name]]$varnames)){
        stop("Invalid variable in: ", name)
      }
    } # end else
  } # end for loop
}
  

.initialize_and_validate_call <- function(call_script){
  # set default list
  call_default <- .default_call()
  # this script sets the call
  call_given <- .
  source(call_script)
  # this script sets all possible calls
  source(path_gjamTime_mastervariables)
  valid_variables_list$x <- FALSE
  valid_variables_list$y <- FALSE
  valid_variables_list$interaction <- NULL
  
  # validate name
  if(!exists("name") || is.na(call_list$name) ||
     !is.character(call_list$name) || nchar(name) == 0){
    name <- default$name
  }
  # check uniqueness of name
  name <- .get_outfolder(path_gjamTime_out, name)
  
  # validate yvars
  if(!exists("yvars") | length(yvars) == 0 | !is_proper_list(yvars)){
    stop(paste("No valid yvars provided in", call_script))
  } else{
    .validate_variables(yvars, var_list)
  }
  
  #validate xvars
  if(!exists("xvars") | length(xvars) == 0 | !is_proper_list(xvars)){
    xvars <- default$xvars
  } else {
    .validate_variables(yvars, var_list)
  }
  
  # validate subset
  if(!exists("subset")){subset <- default$subset}
  
  # validate subsample
  if(!exists("subsample")){
    warning("Not subsampling the spatial rasters might overflow the available memory")
    subsample <- default$subsample
  }
  
  # validate model
  if(!exists("model")){
    model <- default$model
  }
  
  # validate priors
  if(!exists("priors")){}
  
  # validate model_runntime
  
  
}


#### old up: <<<<<<<<<<<<<













# returns a list of the recieved called from call_scrpt
.receive_call <- function(call_scrpt){
  # TODO source script and return as list
}

# returns a list of the valid variables in path_gjamTime_mastervariables
.receive_validVariables <- function(){
  source(path_gjamTime_validVariables)
  valid_vars <- list(
    periods = valid_periods,
    period_const = valid_period_const,
    variables = valid_variables_list,
    versions = valid_versions
  )
  return(valid_vars)
}


## initializes call from calling script
.initialize_call <- function(call_scrpt){
  call_received <- .receive_call(call_scrpt)
  call_default <- .default_call()
  call_build <- list()
  # for each entry in default list
  for(entry in names(call_default)){
    # if available, fill entry with received call
    if(exists(entry, where = call_received) &&
       !.is_equivalent_to_default(call_received[[entry]])){
      call_build[[entry]] <- call_received[[entry]]
    # if not available, fill default entry
    } else {
      call_build[[entry]] <- call_default[[entry]]
    }
  }
  return(call_build)
}

## validates variables in yvars, xvars

.validate_variables <- function(provided_vars, valid_vars, which){
  
  vars_default <- .default_vars()
  vars_default_names <- names(vars_default)

  if(! all(names(provided_vars) %in% vars_default_names)){
    stop("Invalid entry in ", which)
  }
  # vector storing all unique variables
  varVecUnique <- c()
  for(entry in vars_default_names){
    # if an entry exists
    if(exists(entry, where = provided_vars)){
      # validate if its a true variable
      if(exists(entry, where = valid_vars)){
        if(!all(provided_vars[[entry]] %in% valid_vars[[entry]]$varnames)){
          stop("Invalid variable name in ", which, " : ", entry)
        }
        varVecUnique <- append(varVecUnique, provided_vars[[entry]])
      # if x,y
      } else {
        if(entry == "x" || entry == "y"){
          if(isTRUE(provided_vars[[entry]])){provided_vars[[entry]] <- TRUE}
          else {provided_vars[[entry]] <- FALSE}
        }
      }
    # set default  
    } else {
      provided_vars[[entry]] <- vars_default[[entry]]
    }
  }
  
  # ## add uniqueVars # TODO maybe include?
  # provided_vars$varVecUnique <- varVecUnique
  
  ## assert all interaction variables are in varvec unique
  if(!isFALSE(provided_vars$interaction)){
    .check_unique_variables(provided_vars$interaction, varVecUnique)
  }
  ## assert no interactions for response variable
  if(which == "yvars"){
    provided_vars$interaction <- FALSE
  }
}




#' each and only the entry from default in .gjamTime_defaultSettings exists in
#' object 'call'
# asserts call is valid
.validate_call <- function(call){
  
  valid_vars <- .receive_validVariables()
  
  ## validate name
  call$name <- .get_outfolder(path_gjamTime_out, call$name)
  
  ## validate periods
  # if FALSE, needs to be specified
  if(isFALSE(call$periods)){stop("Please specify periods")}
  if(!all(call$periods %in% valid_vars$periods)){
    stop("Invalid periods specified")
  }
  
  ## validate version
  # if FALSE, initialize as first element in valid_versions from 
  if(isFALSE(call$version)){
    call$version <- valid_vars$versions[1]
  } else{
    if(!(all(call$version %in% valid_vars$versions) &&
         length(call$version) == 1)){
      stop("Invalid version specified")
    }
  }
  
  ## validate yvars
  call$yvars <- .validate_variables(call$yvars, valid_vars$variables, "yvars")
  call$yvars$periods = call$periods
  call$yvars$version = call$version
  
  ## validate xvars
  call$xvars <- .validate_variables(call$xvars, valid_vars$variables, "xvars")
  call$xvars$periods = call$periods
  call$xvars$version = call$version
  
  ## validate subset
  if(!isFALSE(call$subset)){
    if(!file.exists(file.path(path_masks,call$subset$mask))){
      stop("Mask for subset not found: ", file.path(path_masks,call$subset$mask))
    } else {
      call$subset$doSubset <- TRUE
    }
  }
  
  ## validate subsample
  if(!isFALSE(call$subsample)){
    
  }
  
  ## validate model
  
  ## validate priorList
  
  ## validate modelRunntime
  
}



.initialize_and_validate_call <- function(call_script){
  
  ## initialize call
  call <- .initialize_call(call_script)
  
  ## validate call
  call <- .validate_call(call)
  
  
}