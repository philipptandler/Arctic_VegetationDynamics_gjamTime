library(dplyr)
library(digest)

source("scripts/__helper/.get_outfolder.R")
source("scripts/1_gjamTime/.gjamTime_defaultSettings.R") 


.is_equivalent_to_default <- function(entry){
  return(identical(entry, NULL) || identical(entry, NA) || isFALSE(entry) ||
           length(entry) == 0 || identical(entry,0) || identical(entry,""))
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

# returns a list of the recieved called from call_scrpt
.receive_call <- function(call_scrpt){
  if(!file.exists(call_scrpt)){
    stop("Call Script not found")
  }
  call_env <- new.env()
  # Source the script inside this environment
  source(call_scrpt, local = call_env)
  as.list(call_env)
}

## initializes call from calling script
.initialize_call <- function(call_scrpt, task_id){
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
  call_build$task_id <- task_id
  call_build
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
  valid_vars
}

## for initializing folder
.get_hash_id <- function(script_path){
  script_content <- readLines(script_path, encoding = "UTF-8")
  script_content <- gsub("\r\n", "\n", script_content)  # Normalize line endings
  script_hash <- substr(digest(paste(script_content, collapse = ""), algo = "sha1"), 1, 12)
  return(script_hash)
}

.copy_callingscript <- function(call_scrpt, outfolder){
  filename_callscrpt <- basename(call_scrpt)
  file.copy(call_scrpt, 
            file.path(outfolder,paste0("copy_", basename(call_scrpt))))
}

.write_hash_id <- function(scrpt, outfolder){
  hash_id <- .get_hash_id(scrpt)
  writeLines(hash_id, file.path(outfolder, ".hash_id.txt"))
}

.find_continuing_outfolder <- function(base_path, base_name, hash_id){
  # List all subdirectories
  subdirs <- list.dirs(base_path, recursive = FALSE, full.names = TRUE)
  
  # Regex pattern to match directories like "somename/" or "somename(n)/"
  pattern <- paste0("^", base_path, "/", base_name, "(\\(\\d+\\))?$")
  
  # Filter directories that match the pattern
  matching_dirs <- grep(pattern, subdirs, value = TRUE)
  for (subdir in rev(matching_dirs)) {
    # Path to .hash_id.txt in each subdirectory
    hash_file <- file.path(subdir, ".hash_id.txt")

    if (file.exists(hash_file)) {
      # Read the stored hash from the file
      stored_hash <- trimws(readLines(hash_file, warn = FALSE, n = 1))
      
      # Compare with the given hash_id
      if (stored_hash == hash_id) {
        return(subdir)  # Return the matching directory
      }
    }
  }
  return(FALSE)
}

.update_continuing_task_id <- function(outfolder){
  
  # List all immediate subdirectories (not files)
  subdirs <- list.dirs(outfolder, recursive = FALSE, full.names = TRUE)
  
  # Count the number of subdirectories
  num_subdirs <- length(subdirs)
  return(num_subdirs+.default_call()$task_id)
}

.initialize_new_outfolder <- function(path, name, scrpt){
  outfolder <- .get_outfolder(path, name)
  dir.create(outfolder)
  .copy_callingscript(scrpt, outfolder) #TODO #write out/somename(1)/.copy_(callscript.R)
  .write_hash_id(scrpt, outfolder) # TODO writes in out/somename(1)/.hash_id.txt
  return(outfolder)
}

## Prepare outfolder:
#' find outfolder for output, 
#' copying calling script,
#' sets hash_id so multiple subsamples go in same outfolder
#' ..
.prepare_outfolder <- function(call, call_script){
  task_id <- call$task_id
  basename <- call$name
  outfolder <- NULL
  if(isFALSE(call$continue)){
    if(task_id == .default_call()$task_id){
      outfolder <- .initialize_new_outfolder(path_gjamTime_out, basename, call_script)
    } else {
      hash_id <- .get_hash_id(call_script) # unique string calling script (length 12)
      outfolder <- .find_continuing_outfolder(path_gjamTime_out, 
                                              name = basename, 
                                              hash = hash_id)
      if(isFALSE(outfolder)){
        outfolder <- .initialize_new_outfolder(path_gjamTime_out, basename, call_script)
        call$task_id <- .default_call()$task_id
      }
    }
  } else {
    if(dir.exists(file.path(path_gjamTime_out, call$continue))){
      outfolder <- file.path(path_gjamTime_out, call$continue)
    } else {
      outfolder <- .find_continuing_outfolder(path_gjamTime_out, 
                                              name = basename, 
                                              hash = call$continue)
      if(isFALSE(outfolder)){
        stop("Continuation of model: continue = ", call$continue, " not found.")
      }
    }
    call$task_id <- .update_continuing_task_id(outfolder)
  }
  call$outfolder <- outfolder
  call
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
  provided_vars
}

.validate_subsample <- function(call){
  if(isFALSE(call$subsample)){
    warning("No subsampling might require more memory than available")
  } 
  # build subsample from default and overwrite existing entries
  build_subsample <- .default_subsample()
  for(entry in names(build_subsample)){
    if(exists(entry, where = call$subsample) &&
       !.is_equivalent_to_default(build_subsample[[entry]])){
      build_subsample[[entry]] <- call$subsample[[entry]]
    }
  }
  call$subsample <- build_subsample
  # assertions
  call$subsample$doSubsample <- TRUE
  call$subsample$size <- as.integer(call$subsample$size)
  call$subsample$seed <- as.integer(call$subsample$seed)
  if(call$subsample$size <= 1){stop("Invalid parameter in subsample")}
  if(call$subsample$mode != "regular" && call$subsample$mode != "random"){
    stop("Invalid mode in subsample$mode")
  }
  # build subsampling
  if(call$subsample$mode == "regular"){
    max_samples <- call$subsample$size**2
    set.seed(call$subsample$seed)
    random_numbers <- sample(0:(max_samples-1))
    new_seed <- call$task_id %% max_samples
    call$subsample$seed <- random_numbers[new_seed + 1]
    set.seed(call$subsample$seed)
  }
  if(call$subsample$mode == "random"){
    call$subsample$seed <- call$subsample$seed + call$task_id
    set.seed(call$subsample$seed)
  }
  call
}

.validate_model <- function(given_model){
  default_model <- .default_call()$model
  model_build <- list()
  for(entry in names(default_model)){
    if(exists(entry, where = given_model) &&
       (isTRUE(given_model[[entry]]) || isFALSE(given_model[[entry]]))){
      model_build[[entry]] <- given_model[[entry]]
    } else {
      model_build[[entry]] <- default_model[[entry]]
    }
  }
  model_build
}


.validate_iteratively_numeric <- function(given_list, default_list) {
  # Iterate through each element in the default_list
  for (name in names(default_list)) {
    # If there is a corresponding entry in given_list
    if (!is.null(given_list[[name]])) {
      # If the value in given_list is a number (and not a vector)
      if (is.numeric(given_list[[name]]) && length(given_list[[name]]) == 1) {
        default_list[[name]] <- given_list[[name]]  # Replace with given value
      } else if (is.list(default_list[[name]]) && is.list(given_list[[name]])) {
        # If both entries are lists, call the function recursively
        default_list[[name]] <- .replace_iteratively_numeric(given_list[[name]], default_list[[name]])
      }
    }
    # If no entry in given_list, keep the default value
  }
  return(default_list)
}




#' each and only the entry from default in .gjamTime_defaultSettings exists in
#' object 'call'
# asserts call is valid
.validate_call <- function(call, call_scrpt){
  
  ## reference for validation  
  valid_vars <- .receive_validVariables()
  
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
  } else {
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
  call <- .validate_subsample(call)
  
  ## validate model
  call$model <- .validate_model(call$model)
  
  ## validate priorSettings
  call$priorSettings <- .validate_iteratively_numeric(call$priorSettings,
                                                      .default_call()$priorSettings)
  
  ## validate modelRunntime
  call$modelRunntime <- .validate_iteratively_numeric(call$modelRunntime,
                                                      .default_call()$modelRunntime)
  
  ## validate name and outfolder
  call <- .prepare_outfolder(call, call_scrpt)
  
  ## return
  call
}



.initialize_and_validate_call <- function(call_scrpt, task_id){
  
  ## initialize call
  call <- .initialize_call(call_scrpt, task_id)
  
  ## validate call
  call <- .validate_call(call, call_scrpt)
  
  ## return
  call
}