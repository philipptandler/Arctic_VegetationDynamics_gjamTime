library(dplyr)
library(withr)
library(digest)
library(terra)
library(orthopolynom)
library(devtools)
library(gjam)

source("scripts/core/1_gjamTime/.gjamTime_defaultSettings.R") 
source("scripts/core/1_gjamTime/.gjamTime_officialFunctions.R")



################################################################################
## initialize and validate call ####
################################################################################
# existance of a specific entry in list
.nested_entry_exists <- function(lst, keys) {
  for (key in keys) {
    if (!is.list(lst) || is.null(lst[[key]])) {
      return(FALSE)
    }
    lst <- lst[[key]]
  }
  return(TRUE)
}


.is_equivalent_to_default <- function(entry){
  return(identical(entry, NULL) || identical(entry, NA) || isFALSE(entry) ||
           length(entry) == 0 || identical(entry,0) || identical(entry,""))
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
  if(is.null(task_id)){
    task_id = call_default$task_id
  }
  # set.seed(task_id) for 
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
  source(path_gjamTime_validVariables, local = TRUE)
  valid_vars <- list(
    times = valid_times,
    time_const = valid_time_const,
    variables = valid_variables_list,
    versions = valid_versions
  )
  valid_vars
}

##############################
## outfolder organisation ####

## get path for saving folder in increments if path exists
.get_outfolder <- function(path, name, full = TRUE){
  dir_path <- file.path(path, name)
  
  # Check if the directory already exists
  if (dir.exists(dir_path)) {
    # If it exists, find a new directory name
    i <- 1
    while (dir.exists(dir_path)) {
      dir_path <- file.path(path, paste0(name, "-", i))
      i <- i + 1
    }
  }
  # return full path or foldername
  if(full){return(dir_path)}
  else{return(basename(dir_path))}
}

.out_pattern <- function(b_name){
  paste0("^", b_name, "[^/]*$")
}

## returns dir
.get_gjamTime_list <- function(argument, b_name){
  if(dir.exists(argument)){
    ptrn <- .out_pattern(b_name)
    l <- list(outfolder = argument, pattern = ptrn)
    return(l)
  } 
  if(!file.exists(argument)){stop("Invalid Argument: ", argument)}
  call <- .initialize_and_validate_call(argument, task_id=NULL)
  call <- .prepare_gjamTime_outfolder(call, argument, create.if.notFound = F)
  
  namestring <-c()
  if(isFALSE(call$subset)){
    subfolder_name <- append(namestring,"allData")
  } else {
    namestring <- append(namestring,
                         tools::file_path_sans_ext(basename(
                           .toLowerCamelCase(call$subset$mask)))
                         )
  }
  namestring <- append(namestring, call$version)
  namestring <- paste(namestring, collapse = "[^/]*")
  
  
  ptrn <- .out_pattern(namestring)
  l <- list(outfolder = call$outfolderBase, pattern = ptrn)
  return(l)
}

## for initializing folder
.get_hash_id <- function(script_path){
  script_content <- readLines(script_path, encoding = "UTF-8")
  script_content <- gsub("\r\n", "\n", script_content)  # Normalize line endings
  script_hash <- substr(digest(paste(script_content, collapse = ""), algo = "sha1"), 1, 16)
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

# returns directory if name (/name-1/ => name) and hash id matches
# else FALSE
.find_continuing_outfolder <- function(base_path, name, hash_id){
  # List all subdirectories
  subdirs <- list.dirs(base_path, recursive = FALSE, full.names = TRUE)
  
  # Regex pattern to match directories like "somename/" or "somename(n)/"
  pattern <- paste0("^", base_path, "/", name, "(-\\d+)?$")
  
  # Filter directories that match the pattern
  matching_dirs <- grep(pattern, subdirs, value = TRUE)
  for (subdir in rev(matching_dirs)) { #rev() so it chooses the latest in case multiple directories exist
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

.initialize_new_outfolder <- function(path, name, scrpt){
  outfolder <- .get_outfolder(path, name)
  dir.create(outfolder)
  .copy_callingscript(scrpt, outfolder)
  .write_hash_id(scrpt, outfolder) 
  return(outfolder)
}

# lowerCamelCase:
.toLowerCamelCase <- function(str) {
  str <- tolower(str) # Convert the whole string to lowercase
  str <- gsub("(_|\\.|-)(\\w)", "\\U\\2", str, perl = TRUE) # Capitalize letter/digit after "_", ".", or "-"
  return(str)
}


## Prepare outfolder:
.prepare_gjamTime_outfolder <- function(call, call_script,
                                        create.if.notFound = FALSE){
  # find base folder
  basename <- call$name
  outfolder <- NULL
  if(isFALSE(call$continue)){
    hash_id <- .get_hash_id(call_script) # unique string calling script
    outfolder <- .find_continuing_outfolder(path_gjamTime_out, 
                                            name = basename, 
                                            hash_id = hash_id)
    if(isFALSE(outfolder)){
      if(create.if.notFound){
        outfolder <- .initialize_new_outfolder(path_gjamTime_out, basename,
                                               call_script)
      } else {
        stop("output folder not found for script: ", call_script)
      }
    }
  } else {
    if(dir.exists(call$continue)){
      outfolder <- call$continue
    } else if(dir.exists(file.path(path_gjamTime_out, call$continue))){
      outfolder <- file.path(path_gjamTime_out, call$continue)
    } else {
      outfolder <- .find_continuing_outfolder(path_gjamTime_out, 
                                              name = basename, 
                                              hash_id = call$continue)
      if(isFALSE(outfolder)){
        if(create.if.notFound){
          outfolder <- .initialize_new_outfolder(path_gjamTime_out, basename,
                                                 call_script)
          warning("Continuation of model: continue = ", call$continue, " not found.\n
                  Output in ", call$continue)
        } else {
          stop("Continuation of model: continue = ", call$continue, " not found.\n
                  Output in ", call$continue)
        }
      }
    }
  }
  call$outfolderBase <- outfolder
  # find subfolder
  subfolder_name <-c()
  if(isFALSE(call$subset)){
    subfolder_name <- append(subfolder_name,"allData")
  } else {
    subfolder_name <- append(subfolder_name, 
                             tools::file_path_sans_ext(basename(
                               .toLowerCamelCase(call$subset$mask)))
                             )
  }
  if(!isFALSE(call$subsample)){
    subfolder_name <- append(subfolder_name, paste0("seed",call$subsample$seed))
  }
  subfolder_name <- append(subfolder_name, call$version)
  subfolder_name <- paste(subfolder_name, collapse = "_")
  if(dir.exists(file.path(call$outfolderBase, subfolder_name)) &&
     create.if.notFound){
    warning("Check if model already exists in: \n",
         file.path(call$outfolderBase, subfolder_name))
  }
  call$outfolderSub <- subfolder_name
  call
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

.extract_base_power <- function(var) {
  # If the var ends with a number, we extract the base and the power
  if (grepl("\\d+$", var)) {
    base <- sub("\\d+$", "", var)  # Extract the base variable (e.g., "A" from "A2")
    power <- as.numeric(sub("^\\D+", "", var))  # Extract the exponent (e.g., 2 from "A2")
    return(c(base, power))
  }
  return(c(var, 1))  # If no number, assume power 1
}

.expand_powers <- function(name, order, base=T){
  if(order < 1){return()}
  if(order == 1 && !base){return()}
  order_nr <- c(1:order)
  order_nr <- as.character(order_nr)
  if(base){order_nr[1] <- ""}
  else {order_nr <- order_nr[-1]}
  expanded <- paste0(name, order_nr)
  return(expanded)
}


.simplify_interactions <- function(interactionVec){
  parsed_terms <- strsplit(interactionVec, ":")
  # each var and the highest power
  power_list <- list()
  # highest order interaction
  interaction_list <- list()
  for(term in parsed_terms){
    # vars_powers is list that holds var and power for each term
    vars_powers <- list()
    for(entry in term){
      vp <- .extract_base_power(entry)
      var <- vp[1]
      power <- as.integer(vp[2])
      if(is.null(vars_powers[[var]])){
        vars_powers[[var]] <- power
      } else {
        vars_powers[[var]] <- vars_powers[[var]] + power
      }
    }
    
    # write power
    for(var in names(vars_powers)){
      if(is.null(power_list[[var]])){
        power_list[[var]] <- vars_powers[[var]]
      } else {
        power_list[[var]] <- max(power_list[[var]], vars_powers[[var]])
      }
    }
    
    # write interaction
    if(length(vars_powers) > 1){
      min_val <- max(min(unlist(vars_powers)), 1) # Find the lowest value in the list
      var_int_base <- .sort_variable(paste0(names(vars_powers), collapse = ":"))
      if(is.null(interaction_list[[var_int_base]])){
        interaction_list[[var_int_base]] <- min_val
      } else {
        interaction_list[[var_int_base]] <- max(interaction_list[[var_int_base]], min_val)
      }
    }
  }
  # power_list holds vars and highest power
  # interaction_list holds base interaction (A:C) and highest order e.g. 1
  powers_vec <- c()
  for(var in names(power_list)){
    all_powers_var <- .expand_powers(var, power_list[[var]], base=F)
    powers_vec <- append(powers_vec, all_powers_var)
  }
  
  interactions_vec <- c()
  for(interaction in names(interaction_list)){
    order <- interaction_list[[interaction]]
    expanded_interactions <- c()
    vars <- strsplit(interaction, ":")[[1]]
    for(var in vars){
      other_vars <- setdiff(vars, var)
      other_vars <- paste0(other_vars, collapse = ":")
      expanded_othervars <- c()
      for(ovar in other_vars){
        tmp <- .expand_powers(ovar, order, base=T)
        if(length(expanded_othervars) == 0){
          expanded_othervars <- tmp
        } else {
          expanded_othervars <- as.vector(outer(expanded_othervars,
                                                tmp,
                                                function(x,y) paste(x, y, sep = ":")))
        }
      }
      expanded_var <- .expand_powers(var, power_list[[var]], base=T)
      tmp <- as.vector(outer(expanded_othervars,
                             expanded_var,
                             function(x,y) paste(x, y, sep = ":")))
      expanded_interactions <- append(expanded_interactions, tmp)
    }
    for(i in 1:length(expanded_interactions)){
      expanded_interactions[i] <- .sort_variable(expanded_interactions[i])
    }
    expanded_interactions <- unique(expanded_interactions)
    interactions_vec <- append(interactions_vec, expanded_interactions)
  }
  int_list <- list(
    powers = powers_vec,
    interactions = interactions_vec
  )
  for(entry in names(int_list)){
    if(is.null(int_list[[entry]])){int_list[[entry]] <- FALSE}
  }
  int_list
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
    if(exists(entry, where = provided_vars) && !isFALSE(provided_vars[[entry]])){
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
    # if no entry, then set default  
    } else {
      provided_vars[[entry]] <- vars_default[[entry]]
    }
  }
  
  # ## add uniqueVars # TODO maybe include?
  # provided_vars$varVecUnique <- varVecUnique
  
  ## assert all interaction variables are in varvec unique
  if(!isFALSE(provided_vars$interaction)){
    .check_unique_variables(provided_vars$interaction, varVecUnique)
    int_list <- .simplify_interactions(provided_vars$interaction)
    provided_vars$interaction <- int_list$interactions
    provided_vars$powers <- int_list$powers
  }
  ## assert no interactions for response variable
  if(which == "yvars"){
    provided_vars$interaction <- FALSE
    provided_vars$powers <- FALSE
    provided_vars$x <- FALSE
    provided_vars$y <- FALSE
  }
  provided_vars
}

.validate_subsample <- function(call){
  if(isFALSE(call$subsample)){
    warning("No subsampling might require more memory than available")
    return(call)
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
    # for all reps the same random array
    set.seed(call$subsample$seed)
    random_numbers <- sample(0:(max_samples-1))
    # select from same array dependent on 
    select <- call$task_id %% max_samples
    call$subsample$seed <- random_numbers[select + 1]
    # random within regular grid
    set.seed(call$subsample$seed)
  }
  if(call$subsample$mode == "random"){
    call$subsample$seed <- call$subsample$seed + call$task_id
    # incremental 
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

# iteratively modifies default list with provided entries in given list
.validate_iteratively_numeric <- function(given_list, default_list, vec=F) {
  # Iterate through each element in the default_list
  for (name in names(default_list)) {
    # If there is a corresponding entry in given_list
    if (!is.null(given_list[[name]])) {
      # If the value in given_list is a number (and not a vector)
      if (!vec && is.numeric(given_list[[name]]) && length(given_list[[name]]) == 1) {
        default_list[[name]] <- given_list[[name]]  # Replace with given value
      } else if (vec && is.numeric(given_list[[name]])) {
        default_list[[name]] <- given_list[[name]]  # Replace with given value
      } else if(is.list(default_list[[name]]) && is.list(given_list[[name]])) {
        # If both entries are lists, call the function recursively
        default_list[[name]] <- .validate_iteratively_numeric(given_list[[name]],
                                                              default_list[[name]],
                                                              vec)
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
  
  ## validate times
  # if FALSE, needs to be specified
  if(isFALSE(call$times)){stop("Please specify times")}
  if(!all(call$times %in% valid_vars$times)){
    stop("Invalid times specified")
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
  
  if(!isFALSE(call$yvars)){
    ## validate yvars
    call$yvars <- .validate_variables(call$yvars, valid_vars$variables, "yvars")
    call$yvars$times = call$times
    call$yvars$version = call$version
  } else {stop("No yvars (response) specified")}
  
  if(!isFALSE(call$xvars)){
    ## validate xvars
    call$xvars <- .validate_variables(call$xvars, valid_vars$variables, "xvars")
    call$xvars$times = call$times
    call$xvars$version = call$version
  } # xvars can be FALSE

  ## validate subset
  if(!isFALSE(call$subset)){
    if(file.exists(file.path(path_masks,call$subset$mask))){
      call$subset$doSubset <- TRUE
    } else if (file.exists(call$subset$mask)){
      call$subset$doSubset <- TRUE
      call$subset$mask <- basename(call$subset$mask)
    } else {
      stop("Mask for subset not found: ", file.path(path_masks,call$subset$mask))
    }
  }
  
  ## validate subsample
  call <- .validate_subsample(call)
  
  ## validate model
  call$model <- .validate_model(call$model)
  
  ## validate priorSettings
  call$priorSettings <- .validate_iteratively_numeric(call$priorSettings,
                                                      .default_call()$priorSettings, vec=T)
  
  ## validate modelRunntime
  call$modelRunntime <- .validate_iteratively_numeric(call$modelRunntime,
                                                      .default_call()$modelRunntime, vec=F)
  ## return
  call
}

.initialize_and_validate_call <- function(call_scrpt, task_id = NULL){
  
  ## initialize call
  call <- .initialize_call(call_scrpt, task_id)
  
  ## validate call
  call <- .validate_call(call, call_scrpt)

  ## return
  call
}

################################################################################
## prepare Geodata ####
################################################################################


# make sample mask
.make_samplemask <- function(call){
  
  ## load master_mask 
  mastermask <- rast(file.path(path_masks, name_master_mask))
  
  ## subsample
  if(!isFALSE(call$subset)){
    subsetmask <- rast(file.path(path_masks,call$subset$mask))
    if(ext(subsetmask) != ext(mastermask)){
      mastermask <- crop(mastermask, subsetmask)
    }
    mastermask <- mastermask & subsetmask
  }
  ## subset
  if(!isFALSE(call$subsample)){
    sample_size <- call$subsample$size
    samplemask <- mastermask
    samplemask[] <- FALSE
    if(call$subsample$mode == "random"){
      sample_size <- min(sample_size, ncell(samplemask))
      # init sample
      init_sample_ind <- spatSample(mastermask, size = sample_size, 
                                    method = "random", cells = TRUE)
      ratio <- sum(init_sample_ind[,2] == 1)/sample_size
      if(sum(init_sample_ind[,2] == 1) == 0){ ratio <- 1/sample_size}
      # true sample corrected for NA
      true_sample_size <- min(as.integer(sample_size/ratio), ncell(mastermask))
      true_sample_ind <- spatSample(mastermask, size = true_sample_size, 
                                    method = "random", cells = TRUE)
      samplemask[true_sample_ind[,1]] <- TRUE
    } 
    if(call$subsample$mode == "regular"){
      seedI <- as.integer(floor(call$subsample$seed/sample_size))
      seedJ <- as.integer(call$subsample$seed %% sample_size)
      fact <- sample_size
      
      # Get the dimensions and seed of the raster
      dims <- dim(samplemask)
      
      # Create indices for the reduced sampling
      reduce_i <- seq(seedI+1, dims[1], by = fact)
      reduce_j <- seq(seedJ+1, dims[2], by = fact)
      
      # Create a matrix of indices to set to TRUE
      indices <- expand.grid(reduce_i, reduce_j)
      samplemask[cbind(indices$Var1, indices$Var2)] <- TRUE
    }
    mastermask <- mastermask & samplemask
  }
  return(mastermask)
}

.get_filenames <- function(time, var_list){
  vers <- var_list$version
  file_vec <- c()
  var_vec <- c()
  valid_variables <- .receive_validVariables()
  # for all potential variables
  for(entry in names(valid_variables$variables)){
    # for the non FALSE variable groups
    if(!isFALSE(var_list[[entry]])){
      groupname <- valid_variables$variables[[entry]]$groupname
      
      if(valid_variables$variables[[entry]]$isDynamic){time_name <- time}
      else {time_name <- valid_variables$time_const}
      
      for(var in var_list[[entry]]){
        filename <- paste(groupname, time_name, var, vers, sep = "_")
        filename <- paste0(filename, ".tif")
        file_vec <- append(file_vec, filename)
        var_vec <- append(var_vec, var)
      }
    } #end if
  }
  f_list <- list(
    files = file_vec,
    variables = var_vec
    )
  return(f_list)
}

.sample_geodata <- function(var_list, which, samplemask){
  n_time <- length(var_list$times)
  for(tm in 1:n_time){
    
    # get files
    file_var_list <- .get_filenames(var_list$times[tm], var_list)
    files_this_time <- file_var_list$files
    vars_this_time <- file_var_list$variables
    
    #file paths
    file_paths_in <- file.path(path_gjamTime_in, files_this_time)
    file_paths_tmp <- file.path(path_gjamTime_tmp, files_this_time)
    # load, mask and save each independently
    for(i in 1:length(files_this_time)){
      if(!file.exists(file_paths_tmp[i])){
        # load
        raster <- rast(file_paths_in[i])
        names(raster) <- vars_this_time[i]
        # crop if necessary
        if(ext(raster) != ext(samplemask)){
          raster <- crop(raster, samplemask)
        }
        # mask
        raster <- mask(x=raster,
                       mask = samplemask,
                       maskvalues=0, updatevalue=NA)
        # save
        writeRaster(raster,
                    file_paths_tmp[i])
      } # if not exists (excludes constant variables)
    } # for loop variables
  } # for loop time
}

# prepare datasets
.prepare_geodata <- function(call){
  # if no subset or subsample
  if(isFALSE(call$subset) && isFALSE(call$subsample)){
    call$gjamTime_data_in <- path_gjamTime_in
    return(call)
  }
  # clean path_gjamTime_tmp directory
  filesRM <- list.files(path_gjamTime_tmp, full.names = TRUE)
  file.remove(filesRM)
  
  #make samplemask
  samplemask <- .make_samplemask(call)
  writeRaster(samplemask, file.path(path_gjamTime_tmp, "samplemask.tif"))
  
  # subset and subsample
  # writes subsetted and subsampled data in path_gjamTime_tmp
  if(!isFALSE(call$xvars)){
    .sample_geodata(var_list = call$xvars, which = "xdata", samplemask = samplemask)
  }
  .sample_geodata(var_list = call$yvars, which = "ydata", samplemask = samplemask)
  
  # adjust path for subsetted, subsampled vairables and return
  call$gjamTime_data_in <- path_gjamTime_tmp
  return(call)
}



################################################################################
## load geospatial rasters as dataframes ####
################################################################################

# Function to return the elements of each interaction. 
.split_interaction <- function(x) {
  if (grepl("^[A-Za-z]+\\d+$", x)) {
    # Handles cases like "A2", "A3"
    var <- gsub("\\d+", "", x)                # Extract "A"
    count <- as.numeric(gsub("\\D+", "", x))  # Extract "2" or "3"
    rep(var, count)                           # Repeat "A" accordingly
  } else {
    # Handles cases like "A:B" or "A:C:E"
    unlist(strsplit(x, ":"))                  # Split by ":"
  }
}

.load_geodata_as_dataframe <- function(var_list,
                                       path_files,
                                       dropgroup = TRUE,
                                       droptime = TRUE){
  
  # list for dataframes
  data_list <- list()
  n_timesteps <- length(var_list$times)
  
  # for all times, load the raster as dataframes
  for(tm in 1:n_timesteps){
    file_var_list <- .get_filenames(var_list$times[tm], var_list)
    # get files
    files_this_time <- file_var_list$files
    file_paths_full <- file.path(path_files, files_this_time)
    # make a raster with all variables for this time
    raster_this_time <- rast(file_paths_full)
    names(raster_this_time) <- file_var_list$variables
    # make dataframe
    getxy <- (var_list$x | var_list$y) #whether to include latitude or longitude
    df <- as.data.frame(raster_this_time, xy = getxy,
                        cells = TRUE, na.rm = NA)
    # Rename x and y to lat and lon
    colnames(df)[colnames(df) == "x"] <- "lon"
    colnames(df)[colnames(df) == "y"] <- "lat"
    if(getxy){
      if(!var_list$x){df <- df %>% dplyr::select(-lon)}
      if(!var_list$y){df <- df %>% dplyr::select(-lat)}
    }
    # add time
    df$time <- tm
    data_list[[tm]] <- df
  }
  # combine the dataframes
  combined_df <- do.call(rbind, data_list)
  # order
  ordered_df <- combined_df %>%
    dplyr::arrange(cell, time)
  # Get the current column names
  cols <- colnames(ordered_df)
  # Define the new order of columns
  new_order <- c("cell", "time", cols[!cols %in% c("cell", "time")])
  # Reorder the columns
  ordered_df <- ordered_df[, new_order]
  # Set row names starting from 1
  row.names(ordered_df) <- NULL
  
  # Optionally drop the 'cell' and 'period' columns if specified
  if (dropgroup) {
    ordered_df <- subset(ordered_df, select = -c(cell))
  }
  if (droptime) {
    ordered_df <- subset(ordered_df, select = -c(time))
  }
  
  ordered_df
}

.uncorr_predictors <- function(df, call){
  var_list <- call$xvars
  # normalize
  baseVars <- .load_variables(var_list, "base")
  df <- .normalize_gjamTime_predictors(call, baseVars, df)
  # add interactions
  if(!isFALSE(var_list$powers)){
    for(entry in var_list$powers){
      vp <- .extract_base_power(entry)
      var <- vp[1]
      power <- as.integer(vp[2])
      df[[entry]] <- .lengedre_poly(df[[var]], power)
    }
  }
  if(!isFALSE(var_list$interaction)){
    for(entry in var_list$interaction){
      vars <- .split_interaction(entry)
      df[[entry]] <- Reduce(`*`, lapply(vars, function(v) df[[v]]))
    } 
  }
  df
}

.load_predictors <- function(call){
  if(isFALSE(call$xvars))return(NULL)
  df <- .load_geodata_as_dataframe(call$xvars, call$gjamTime_data_in,
                                   dropgroup = FALSE, droptime = FALSE)
  df <- .uncorr_predictors(df, call)
  df
}

.load_response <- function(call){
  df <- .load_geodata_as_dataframe(call$yvars, call$gjamTime_data_in,
                                   dropgroup = TRUE, droptime = TRUE)
  df
}


################################################################################
## data normalisation ####
################################################################################

# Lengendre polynomial, x is vector, raster, ..., n is order of highest power
.lengedre_poly <- function(x, n) {
  leg_poly_func <- as.function(legendre.polynomials(n, normalized=F)[[n+1]])
  xn <- leg_poly_func(x) 
  return(xn)
}

# returns specified variables in call as vector
.load_variables <- function(var_list, method = "all"){
  vars_default <- .default_vars()
  valid_vars <- .receive_validVariables()$variables
  
  varVec <- c()
  for(entry in names(valid_vars)){
    if(!isFALSE(var_list[[entry]])){
      if(method == "all" || method == "base"){
        varVec <- append(varVec, var_list[[entry]])
      } else if (method == "const" && !valid_vars[[entry]]$isDynamic){
        varVec <- append(varVec, var_list[[entry]])
      }
    }
  }
  if(var_list$x){varVec <- append(varVec, "lon")}
  if(var_list$y){varVec <- append(varVec, "lat")}
  if(method == "base"){return(varVec)}
  # interactions and powers
  if(!isFALSE(var_list$interaction) || !isFALSE(var_list$powers)){
    interactions <- c()
    if(!isFALSE(var_list$powers)){interactions <- append(interactions, var_list$powers)}
    if(!isFALSE(var_list$interaction)){interactions <- append(interactions, var_list$interaction)}
    for(interaction in interactions){
      if(method == "all"){
        varVec <- append(varVec, interaction)
      }
      else if (method == "const"){
        extracted_vars <- unique(unlist(lapply(interaction, function(x) {
          # Split by ":" or extract base of power notation
          unique(gsub("\\d+$", "", unlist(strsplit(x, ":"))))  
        })))
        if(all(extracted_vars %in% varVec)){
          varVec <- append(varVec, interaction)
        }
      }
    }
  }
  return(varVec)
}

## normalization
.binTimeCode <- function(time_vec){
  ref_vec <-.receive_validVariables()$times
  bin_vec <- ref_vec %in% time_vec
  return(paste0(as.integer(bin_vec), collapse = ""))
}

.sort_variable <- function(interaction_var) {
  vars <- unlist(strsplit(interaction_var, ":", fixed = TRUE))
  sorted_vars <- sort(vars)
  return(paste(sorted_vars, collapse = ":"))
}


# finds the mu and sd
.calc_normalization_var <- function(version, subset, times, var){
  
  ## set seed env 
  withr::with_seed(1234, {
    
    ## load raster generally (works for simple vars as well as var interactions)
    ref_times <- times
    ref_times <- append(ref_times, .receive_validVariables()$time_const)
    reftimePattern <- paste(ref_times, collapse = "|")
    rstack <- list()
    
    if(var == "lat" || var == "lon"){
      mastermask <- rast(file.path(path_masks, name_master_mask))
      r <- mastermask
      if(var == "lat"){
        values(r) <- yFromCell(mastermask, 1:ncell(mastermask))
      } 
      if(var == "lon"){
        values(r) <- xFromCell(mastermask, 1:ncell(mastermask))
      }
      r <- mask(r, mastermask, maskvalues=0, updatevalue=NA)
    }else{
      pattern <- paste0(".*_(", reftimePattern, ")_", var, "_", version,"\\.tif$")
      file_path <- list.files(path_gjamTime_in, pattern = pattern, full.names = TRUE)
      r <- rast(file_path)
    }
    ## subset
    if(!isFALSE(subset)){
      mask_subset <- rast(file.path(path_masks,subset$mask))
      if(ext(r) != ext(mask_subset)){r <- crop(r, mask_subset)}
      r <- mask(r, mask_subset, maskvalues=0, updatevalue=NA)
    }
    ## random sample
    ncells <- ncell(r)
    if(ncells <= 1e5){
      samplesize <- ncells
    } else {
      samplesize <- 1e5
    }
    # init sample
    init_sample_ind <- spatSample(r, size = samplesize, 
                                  method = "random", cells = TRUE)
    ratio <- sum(is.na(init_sample_ind[,2]))/samplesize
    if(sum(is.na(init_sample_ind[,2])) == 0){ ratio <- 1/samplesize}
    # true sample corrected for NA
    true_sample_size <- min(as.integer(samplesize/ratio), ncells)
    true_sample_ind <- spatSample(r, size = true_sample_size, 
                                  method = "random", cells = TRUE)
    vals <- c()
    for(index in 2:ncol(true_sample_ind)){
      vals <- append(vals, true_sample_ind[,index])
    }
    
    ## calc mu and sd
    mu <- mean(vals, na.rm =T)
    sd <- sd(vals, na.rm =T)
    ## return
    norm_vec <- c(mu, sd)
    norm_vec
  }) 
}

.check_and_write_norm_param <- function(call, reset = F, overwrite=F){
  varVec <- .load_variables(call$xvars, "base")
  version <- call$version
  subset <- call$subset
  subset_name <- "alldata"
  if(!isFALSE(subset)){
    subset_name <- tools::file_path_sans_ext(basename(subset$mask))
  }
  time_code <- .binTimeCode(call$times)
  
  if(!reset && file.exists("scripts/project/.normalization/.norm_param.rds")){
    ref_list <- readRDS("scripts/project/.normalization/.norm_param.rds")
  } else {
    ref_list <- list()
  }
  changed_rds <- FALSE
  for (var in varVec){
    varname <- .sort_variable(var)
    if(overwrite ||
       !.nested_entry_exists(ref_list, c(version, subset_name, varname, time_code))
    ){
      # write entry as vector c(Mu, Sd)
      ref_list[[version]][[subset_name]][[time_code]][[varname]] <- 
        .calc_normalization_var(version,
                                subset,
                                call$times,
                                varname)
      changed_rds <- TRUE
    }
  }
  # save list
  if(changed_rds){
    saveRDS(ref_list, "scripts/project/.normalization/.norm_param.rds")
  }
}



# returns normalized predictors
.normalize_gjamTime_predictors <- function(call, varVec, df){
  ## check if normalization parameter exist, calc if required
  .check_and_write_norm_param(call, reset=F)
  ## normalize input with normalisation parameters
  ref_list <- readRDS("scripts/project/.normalization/.norm_param.rds")
  version <- call$version
  subset <- call$subset
  subset_name <- "alldata"
  if(!isFALSE(subset)){
    subset_name <- tools::file_path_sans_ext(basename(subset$mask))
  }
  time_code <- .binTimeCode(call$times)
  for(col in varVec){
    col_ref <- .sort_variable(col)
    mu <- ref_list[[version]][[subset_name]][[time_code]][[col_ref]][1]
    sd <- ref_list[[version]][[subset_name]][[time_code]][[col_ref]][2]
    df[[col]] <- (df[[col]]-mu)/(3*sd)
  }
  return(df)
}



.normalize_predictor_rasters <- function(argument){
  
  # path path_gjamTime_out
  
  
}


################################################################################
## fit gjamTime ####
################################################################################

.redirect_gjam <- function(){
  source("scripts/core/1_gjamTime/.gjamTime_adjustments.R")
  environment(.rhoPriorMod) <- asNamespace('gjam')
  assignInNamespace(".rhoPrior", .rhoPriorMod, ns = "gjam")
}
.stop_redirect_gjam <- function(){
  # reload package
  detach("package:gjam", unload=TRUE)
  library(gjam)
}

## fill means in fields with NA
.fillmeans_df <- function(df, vars) {
  groupVec <- df$group
  
  for (col in vars) {
    colVec <- df[[col]]
    # Calculate means for each group, ignoring NA
    groupMeans <- tapply(colVec, groupVec, mean, na.rm = TRUE)
    
    # Calculate the overall mean of the column, ignoring NA
    overallMean <- mean(colVec, na.rm = TRUE)
    
    for (grp in unique(groupVec)) {
      # Identify the indices of the group
      groupIndices <- which(groupVec == grp)
      
      # Check if all values in this group for the column are NA
      if (all(is.na(colVec[groupIndices]))) {
        # Replace NA with the overall column mean
        df[[col]][groupIndices] <- overallMean
      } else {
        # Replace NA with the group's mean
        df[[col]][groupIndices][is.na(colVec[groupIndices])] <- groupMeans[as.character(grp)]
      }
    }
  }
  
  return(df)
}

## assert the dataframes are not empty
.check_dataframe <- function(df){
  n_NA <- sum(is.na(df))
  if(n_NA > 0){
    warning(n_NA, " NA values encountered")
    totCol <- nrow(df)
    for (col in names(df)) {
      column_vector <- df[[col]]
      n_NA_col <- sum(is.na(column_vector))
      warning("   ",col, ": ", n_NA_col, " NA values out of ", totCol)
    }
  }
}

## fill priorLists
.fill_priorList <- function(all_vars, priorlist, lo, hi){
  loValues <- rep(lo, length(all_vars))
  hiValues <- rep(hi, length(all_vars))
  # Add variables and their values dynamically
  for (i in seq_along(all_vars)) {
    priorlist$lo[[all_vars[i]]] <- loValues[i]
    priorlist$hi[[all_vars[i]]] <- hiValues[i]
  }
  return(priorlist)
}


.remove_large_entries <- function(lst, max_size = 1e4) {
  if (is.list(lst)) {
    # Recursively process each element of the list
    lst <- lapply(lst, .remove_large_entries, max_size = max_size)
    return(lst)
  } else {
    # Check if the element is too large
    if (is.matrix(lst) || is.data.frame(lst)) {
      if (prod(dim(lst)) > max_size) return(NA)
    } else if (length(lst) > max_size) {
      return(NA)
    }
  }
  return(lst)
}

.select_gjamOutput<- function(output){
  
  DIC <- output$fit$DIC
  rmspeAll <- output$fit$rmspeAll
  rmspeBySpec <- output$fit$rmspeBySpec
  xscore <- output$fit$xscore
  yscore <- output$fit$yscore
  
  alphaMu <- output$parameters$alphaMu
  alphaSe <- output$parameters$alphaSe
  
  corMu <- output$parameters$corMu
  corSe <- output$parameters$corSe
  
  rhoMu <- output$parameters$rhoMu
  rhoSe <- output$parameters$rhoSe
  
  rhoStandXmu <- output$parameters$rhoStandXmu
  rhoStandXse <- output$parameters$rhoStandXse
  
  sensAlpha <- output$parameters$sensAlpha
  sensRho <- output$parameters$sensRho
  
  # if Mu and Se, the ending of entry must be "entryMu", "entrySe"
  outlist <- list(
    DIC = DIC,
    rmspeAll = rmspeAll,
    rmspeBySpec = rmspeBySpec,
    xscore = xscore,
    yscore = yscore,
    alphaMu = alphaMu,
    alphaSe = alphaSe,
    corMu = corMu,
    corSe = corSe,
    rhoMu = rhoMu,
    rhoSe = rhoSe,
    rhoStandXMu = rhoStandXmu,
    rhoStandXSe = rhoStandXse,
    sensAlpha = sensAlpha,
    sensRho = sensRho
  )
  return(outlist)
}

## main model fit
.fit_gjamTime <- function(call,
                          saveOutput,
                          savePlots,
                          showPlots,
                          fixWarning=T){
  # general version to fix bug
  if(fixWarning){.redirect_gjam()}
  
  # setting up environment
  xdata <- as.data.frame(call$xdata)
  ydata <- as.matrix(call$ydata)
  rownames(ydata) <- NULL
  edata <- matrix(1, nrow = nrow(ydata), ncol = ncol(ydata))
  colnames(edata) <- colnames(ydata)

  ## Missing values in time series data
  # prepare input
  timeCol   <- "time" # Column that stores time
  groupCol  <- "cell" # column that stores the group ID
  
  # constVars are time invariant
  constVars <- .load_variables(call$xvars, "const")
  allVars <- .load_variables(call$xvars, "all")
  
  # fit missing values
  missingEffort <- 0.1
  # groupVars are the time invariant columns, which includes the group ID 'cell'
  groupVars <- c("cell", constVars)
  tmp <- gjamFillMissingTimes(xdata, ydata, edata, groupCol, timeCol,
                              FILLMEANS = T, groupVars = groupVars,
                              typeNames = 'DA', missingEffort = missingEffort)
  xdata  <- tmp$xdata
  ydata  <- tmp$ydata
  edata  <- tmp$edata
  tlist  <- tmp$timeList
  s_names <- colnames(ydata)
  n_spec <- ncol(ydata)
  effort <- list(columns = 1:n_spec, values = edata)
  
  # fill means of not group varibles manually
  xdata  <- .fillmeans_df(xdata, allVars)
  
  # check if there are missing values
  .check_dataframe(ydata)
  .check_dataframe(xdata)
  
  # defining the formula
  formula <- as.formula(paste("~ ", paste(allVars, collapse = " + ")))
  
  ##  set priorlist
  priorList <- list()
  if(call$model$termB){
    b_set <- call$priorSettings$beta
    # set priordistribution (intercept = (-Inf, Inf) and all other vars = (-100, 100))
    betaPrior  <- list(lo = list(intercept = b_set$intercept$lo),
                       hi = list(intercept = b_set$intercept$hi))
    betaPrior <- .fill_priorList(allVars, betaPrior,
                                 b_set$variables$lo,
                                 b_set$variables$hi)
    formulaBeta <- formula
    priorList$formulaBeta = formulaBeta
    priorList$betaPrior = betaPrior
  }
  if(call$model$termR){
    r_set <- call$priorSettings$rho
    # set priordistribution (intercept = (-1,1), all other vars = (-100, 100))
    rhoPrior  <- list(lo = list(intercept = r_set$intercept$lo),
                      hi = list(intercept = r_set$intercept$hi))
    rhoPrior <- .fill_priorList(allVars, rhoPrior,
                                r_set$variables$lo,
                                r_set$variables$hi)
    formulaRho <- formula
    priorList$formulaRho= formulaRho
    priorList$rhoPrior = rhoPrior
  }
  if(call$model$termA){
    a_set <- call$priorSettings$alpha
    alphaSign  <- matrix(a_set, n_spec, n_spec)
    colnames(alphaSign) <- rownames(alphaSign) <- s_names
    priorList$alphaSign = alphaSign
  }
  tmp <- gjamTimePrior(xdata, ydata, edata, priorList)
  timeList <- mergeList(tlist, tmp)
  
  ## fit gjam
  modelList <- list(typeNames = 'DA',
                    ng = call$modelRunntime$ng, 
                    burnin = call$modelRunntime$burnin,  
                    timeList = timeList, effort = effort)
  output <- gjam(formula, xdata=xdata, ydata=ydata, modelList=modelList)
  
  ## save or show outputs
  if(saveOutput || savePlots){
    outFolder <- file.path(call$outfolderBase, call$outfolderSub)
    if(dir.exists(outFolder)){
      newname <- .get_outfolder(call$outfolderBase, call$outfolderSub, full = F)
      call$outfolderSub <- newname
      outFoldernew <- file.path(call$outfolderBase, newname)
      warning(outFolder, " already exists, output in: ", outFoldernew)
      outFolder <- outFoldernew
    }
    dir.create(outFolder, recursive = T)
    
    # plots
    if(savePlots){
      plotPars  <- list(PLOTALLY=T,
                        SAVEPLOTS = savePlots, outFolder = outFolder)
      gjamPlot(output, plotPars)
    }
    # output
    if(saveOutput){
      # call
      call_saverds <- .remove_large_entries(call, .default_output_size()$saveCallRDS)
      call_saverds$xdata <- NULL
      call_saverds$ydata <- NULL
      saveRDS(call_saverds, file = file.path(outFolder, "call.rds"))
      call_savetxt <- .remove_large_entries(call, .default_output_size()$saveCalltxt)
      sink(file.path(outFolder, "call.txt"))
      print(call_savetxt)
      sink()
      # output
      mx <- .default_output_size()$saveOutputRData
      #output_save <- rapply(output, .rm_large_entries, how = "replace", max_size = mx)
      output_save <- .select_gjamOutput(output)

      # output_save <- .remove_large_entries(output, .default_output_size()$saveOutputRData)
      save(output_save, file = file.path(outFolder, "output.rdata"))
    }
  }
  if(showPlots){
    plotPars  <- list(PLOTALLY=T,
                      SAVEPLOTS = F)
    gjamPlot(output, plotPars)
  }
  
  if(fixWarning){.stop_redirect_gjam()}
  return(output)
}

# gjamTime geospatial main
.gjamTime_geospatial <- function(call_scrpt,
                                 task_id,
                                 saveOutput,
                                 savePlots,
                                 showPlots){
  
  cat("run .gjamTime_geospatial()\n")

  ## initialize and validate call
  cat("initialize and validate call\n")
  call <- .initialize_and_validate_call(call_scrpt, task_id)
  
  ## prepare outfolder 
  if(saveOutput || savePlots){
    cat("prepare outfolder\n")
    call <- .prepare_gjamTime_outfolder(call, call_scrpt,
                                        create.if.notFound = TRUE) 
  }
  
  ## prepare geospatial rasters for model
  cat("prepare geodata\n")
  call <- .prepare_geodata(call)
  
  ## load predictors (xdata) as dataframe
  cat("load xdata\n")
  call$xdata <- .load_predictors(call)
  
  ## load response (ydata) as dataframe
  cat("load ydata\n")
  call$ydata <- .load_response(call)
  
  ## fit gjamTime
  cat("fit gjamTime\n")
  output <- .fit_gjamTime(call, saveOutput, savePlots, showPlots, fixWarning = T)
  
  ## done
  cat("\ndone\n\n\n")
  return(output)
}


.separate_gjamTime_MuSe <- function(vec) {
  # Initialize empty vectors for Mu and MuSe
  Mu <- character(0)
  MuSe <- character(0)
  
  # Loop through each element of vec
  for (v in vec) {
    # If it contains 'mu' or 'se', process it for MuSe
    if (grepl("mu$|se$", v, ignore.case = TRUE)) {
      # Extract the base name (before "mu" or "se")
      base_name <- sub("(mu|se)$", "", v, ignore.case = TRUE)
      if (!base_name %in% Mu) {
        MuSe <- c(MuSe, base_name)
      }
    } else {
      # Otherwise, add to Mu
      Mu <- c(Mu, v)
    }
  }
  
  # Return the list of Mu and MuSe vectors
  return(list(simple = Mu, MuSe = unique(MuSe)))
}

.average_entry <- function(lst){
  nreps <- length(lst)
  if(nreps == 0) return(NULL)
  if(is.vector(lst[[1]])){
    vec <- rep(0, times = length(lst[[1]]))
    for(i in 1:nreps){
      vec <- vec + lst[[i]]
    }
    return(vec/nreps)
  }
  if(is.matrix(lst[[1]])){
    mat <- matrix(0, nrow = nrow(lst[[1]]), ncol = ncol(lst[[1]]))
    for(i in 1:nreps){
      mat <- mat + lst[[i]]
    }
    return(mat/nreps)
  } 
  return(FALSE)
}

.average_muse <- function(mu_list, sd_list){
  if(length(mu_list) == 0 || length(sd_list) == 0) return(NULL)
  n_row <- nrow(mu_list[[1]])
  n_col <- ncol(mu_list[[1]])
  
  # Initialize matrices to store the numerator and denominator
  weighted_mean <- matrix(0, nrow=n_row, ncol=n_col)
  inverse_variance <- matrix(0, nrow=n_row, ncol=n_col)
  # Calculate the numerator and denominator for each element in the matrices
  for (i in 1:length(mu_list)) {
    weighted_mean <- weighted_mean + mu_list[[i]] / (sd_list[[i]]^2)
    inverse_variance <- inverse_variance + 1 / (sd_list[[i]]^2)
  }
  
  # Calculate the weighted mean matrix
  mu_matrix <- weighted_mean / inverse_variance
  
  # Calculate the standard error matrix
  sd_matrix <- sqrt(1 / inverse_variance)
  
  paramater_list <- list(
    mean = mu_matrix,
    sd = sd_matrix
  )
  return(paramater_list)
}

# argument is either calling script or folder. If folder, base_name is an identifier of the sub directories. Can be NULL.
# cp_to_repo copies output in scripts/project/1_gjamTime/.parameters with outname_output.Rdata
.gjamTime_summary <- function(argument, base_name=NULL, cp_to_repo=F, outname=NULL){
  #find subdirs
  outlist <- .get_gjamTime_list(argument, base_name)
  dirs <- list.dirs(outlist$outfolder,
                    full.names = T, recursive = F)
  subdirs <- dirs[grepl(outlist$pattern, basename(dirs))]
  
  output_collection <- list()
  # write all outputs in output_collection[[entry]][[i]]=value
  for(subdir in subdirs){
    rdata_path <- file.path(subdir, "output.Rdata")
    if (file.exists(rdata_path)) {
      # Load the Rdata file
      load(rdata_path) #loads output_save as list
      for(entry in names(output_save)){
        output_collection[[entry]][[length(output_collection[[entry]])+1]] <- output_save[[entry]]
      }
    }
  }
  # treat simple vs. Mu/Se entries different
  sep_list <- .separate_gjamTime_MuSe(names(output_collection))
  entry_simple <- sep_list$simple
  entry_muse <- sep_list$MuSe
  output_summary <- list()
  for(entry in entry_simple){
    output_summary[[entry]] <- .average_entry(output_collection[[entry]])
    if(isFALSE(output_summary[[entry]])){
      warning("couldnt summarize", entry)
    }
  }
  for(entry in entry_muse){
    entryMu <- paste0(entry, "Mu")
    entrySe <- paste0(entry, "Se")
    avg_mu_se <- .average_muse(output_collection[[entryMu]],
                               output_collection[[entrySe]])
    output_summary[[entryMu]] <- avg_mu_se$mean
    output_summary[[entrySe]] <- avg_mu_se$sd
  }
  # write summarized output
  save(output_summary, file = file.path(outlist$outfolder, "output.rdata"))
  # copy call.rds
  call_save <- readRDS(file.path(subdirs[1], "call.rds"))
  saveRDS(call_save, file = file.path(outlist$outfolder, "call.rds"))
  if(cp_to_repo){
    if(is.null(outname)){outname <- basename(outlist$outfolder)}
    save(output_summary, file = file.path("scripts/project/.parameters",
                                          paste0(outname, "_output.rdata")))
    saveRDS(call_save, file = file.path("scripts/project/.parameters",
                                        paste0(outname, "_call.rds")))
  }
  output_summary
}