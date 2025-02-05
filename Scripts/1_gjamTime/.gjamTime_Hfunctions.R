library(dplyr)
library(digest)
library(terra)
library(devtools)
library(gjam)

source("scripts/__helper/.get_outfolder.R")
source("scripts/1_gjamTime/.gjamTime_defaultSettings.R") 
source("scripts/1_gjamTime/.gjamTime_officialFunctions.R")


################################################################################
## initialize and validate call ####
################################################################################


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
    times = valid_times,
    time_const = valid_time_const,
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
  .copy_callingscript(scrpt, outfolder)
  .write_hash_id(scrpt, outfolder) 
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
      outfolder <- .initialize_new_outfolder(path_gjamTime_out, basename,
                                             call_script)
    } else {
      hash_id <- .get_hash_id(call_script) # unique string calling script
      outfolder <- .find_continuing_outfolder(path_gjamTime_out, 
                                              name = basename, 
                                              hash = hash_id)
      if(isFALSE(outfolder)){
        outfolder <- .initialize_new_outfolder(path_gjamTime_out, basename,
                                               call_script)
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
  }
  ## assert no interactions for response variable
  if(which == "yvars"){
    provided_vars$interaction <- FALSE
    provided_vars$x <- FALSE
    provided_vars$y <- FALSE
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
        default_list[[name]] <- .validate_iteratively_numeric(given_list[[name]],
                                                              default_list[[name]])
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
  
  ## validate yvars
  call$yvars <- .validate_variables(call$yvars, valid_vars$variables, "yvars")
  call$yvars$times = call$times
  call$yvars$version = call$version
  
  ## validate xvars
  call$xvars <- .validate_variables(call$xvars, valid_vars$variables, "xvars")
  call$xvars$times = call$times
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
      # init sample
      init_sample_ind <- spatSample(mastermask, size = sample_size, 
                                    method = "random", cells = TRUE)
      ratio <- sum(init_sample_ind[,2] == 1)/sample_size
      if(sum(init_sample_ind[,2] == 1) == 0){ ratio <- 1/sample_size}
      # true sample corrected for NA
      true_sample_ind <- spatSample(mastermask, size = as.integer(sample_size/ratio), 
                                    method = "random", cells = TRUE)
      samplemask[true_sample_ind] <- TRUE
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
  .sample_geodata(var_list = call$xvars, which = "xdata", samplemask = samplemask)
  .sample_geodata(var_list = call$yvars, which = "ydata", samplemask = samplemask)
  
  # adjust path for subsetted, subsampled vairables and return
  call$gjamTime_data_in <- path_gjamTime_tmp
  return(call)
}



################################################################################
## load geospatial rasters as dataframes ####
################################################################################


# Function to return the elements of each interaction. 
.process_interaction <- function(x) {
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
                                       which,
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
    # add interactions
    if(!isFALSE(var_list$interaction)){
      for(entry in var_list$interaction){
        vars <- .process_interaction(entry)
        df[[entry]] <- Reduce(`*`, lapply(vars, function(v) df[[v]]))
      }
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


.load_predictors <- function(call){
  df <- .load_geodata_as_dataframe(call$xvars,
                                   call$gjamTime_data_in, which = "xdata",
                                   dropgroup = FALSE, droptime = FALSE)
  df
}

.load_response <- function(call){
  df <- .load_geodata_as_dataframe(call$yvars, 
                                   call$gjamTime_data_in, which = "ydata",
                                   dropgroup = TRUE, droptime = TRUE)
  df
}


################################################################################
## fit gjamTime ####
################################################################################

.redirect_gjam <- function(){
  source("scripts/1_gjamTime/.gjamTime_adjustments.R")
  environment(.rhoPriorMod) <- asNamespace('gjam')
  assignInNamespace(".rhoPrior", .rhoPriorMod, ns = "gjam")
}
.stop_redirect_gjam <- function(){
  # reload package
  detach("package:gjam", unload=TRUE)
  library(gjam)
}



.is_const_interaction <- function(interaction){
  vars <- .process_interaction(interaction)
  const <- c()
}

# returns specified variables in call as vector
.load_variables <- function(var_list, method = "all"){
  vars_default <- .default_vars()
  valid_vars <- .receive_validVariables()$variables
  
  varVec <- c()
  for(entry in names(valid_vars)){
    if(!isFALSE(var_list[[entry]])){
      if(method == "all"){
        varVec <- append(varVec, var_list[[entry]])
      } else if (method == "const" && !valid_vars[[entry]]$isDynamic){
        varVec <- append(varVec, var_list[[entry]])
      }
    }
  }
  if(var_list$x){varVec <- append(varVec, "lon")}
  if(var_list$y){varVec <- append(varVec, "lat")}
  if(!isFALSE(var_list$interaction)){
    for(interaction in var_list$interaction){
      if(method == "all"){
        varVec <- append(varVec, interaction)
      } 
      # TODO find constant interactions
    }
  }
  return(varVec)
}

c(setup,
termB = FALSE,
termR = TRUE,
termA = TRUE,
normalize = TRUE,
# normalize = "ref" for normalizing to reference fulldata, 1984-2020
saveOutput = TRUE,
showPlot = TRUE)


.fit_gjamTime <- function(call,
                          fixwarning = TRUE){
  # general version to fix bug
  if(fixWarning){.redirect_gjam()}
  
  # setting up environment
  xdata <- as.data.frame(call$xdata)
  ydata <- as.matrix(call$ydata)
  rownames(ydata) <- NULL
  edata <- matrix(1, nrow = nrow(ydata), ncol = ncol(ydata))
  colnames(edata) <- colnames(ydata)
  
  xvars_list <- call$xvars
  yvars_list <- call$yvars
  name <- call$name
  
  ## Missing values in time series data
  # prepare input
  timeCol   <- "time" # Column that stores time
  groupCol  <- "cell" # column that stores the group ID
  
  # constVars are time invariant
  constVars <- .load_variables(call$xvars, "const")
  allVars < .load_variables(call$xvars, "all")

  ## TODO continue here normalization
  
  ## >>>>>>>>>>>>>> (old)
  # general Vesions
  if(fixWarning){redirect_gjam()}
  if(tracking){start_track_gjam()}
  
  # setting up environment
  xdata <- as.data.frame(setup$xdata)
  ydata <- as.matrix(setup$ydata)
  rownames(ydata) <- NULL
  edata <- matrix(1, nrow = nrow(ydata), ncol = ncol(ydata))
  colnames(edata) <- colnames(ydata)
  
  xvars_list <- setup$xvars
  yvars_list <- setup$yvars
  name <- setup$name
  
  ## Missing values in time series data
  
  # prepare input
  timeCol   <- "period" # Column that stores time
  groupCol  <- "cell" # column that stores the group ID
  
  # constVars are time invariant
  constVars <- c(xvars_list$topography,
                 xvars_list$soil) #TODO wildfire when fitting
  if(xvars_list$x){constVars <- c(constVars, "lon")}
  if(xvars_list$y){constVars <- c(constVars, "lat")}
  
  # allVars are all predictor Variables
  allVars <- c(constVars,
               xvars_list$climate,
               xvars_list$wildfire,
               xvars_list$interaction)
  
  # data normalization
  if(normalize == "ref"){
    xdata <- normalize_gjamInput_ref(xdata, allVars)
  }else if(is.logical(normalize) && normalize){
    xdata <- normalize_gjamInput_this(xdata, allVars)
  }else{stop("no valid normalization method")}
  
  # fit missing values
  missingEffort <- 0.1 # set this
  # groupVars are the time invariant columns, which includes the group ID 'cell'
  groupVars <- c("cell", constVars)
  cat("    initialize xdata and ydata \n")
  tmp <- gjamFillMissingTimes(xdata, ydata, edata, groupCol, timeCol,
                              FILLMEANS = T, groupVars = groupVars,
                              typeNames = 'DA', missingEffort = 0.1)
  xdata  <- tmp$xdata
  ydata  <- tmp$ydata
  edata  <- tmp$edata
  tlist  <- tmp$timeList
  s_names <- colnames(ydata)
  n_spec <- ncol(ydata)
  effort <- list(columns = 1:n_spec, values = edata)
  
  # fill means of not group varibles manually
  xdata  <- fillmeans(xdata, allVars)
  
  # check if there are missing values
  check_dataframe(ydata)
  check_dataframe(xdata)
  
  # defining the formula
  formula <- as.formula(paste("~ ", paste(allVars, collapse = " + ")))
  
  ##  set priorlist
  cat("    setting priors \n")
  priorList <- list()
  if(termB){
    # set priordistribution (intercept = (-Inf, Inf) and all other vars = (-100, 100))
    betaPrior  <- list(lo = list(intercept = -10),
                       hi = list(intercept = 10) )
    betaPrior <- fill_priorList(allVars, betaPrior)
    formulaBeta <- formula
    priorList$formulaBeta = formulaBeta
    priorList$betaPrior = betaPrior
  }
  if(termR){
    # set priordistribution (intercept = (-1,1), all other vars = (-100, 100))
    rhoPrior  <- list(lo = list(intercept = -2),
                      hi = list(intercept = 2) )
    rhoPrior <- fill_priorList(allVars, rhoPrior)
    formulaRho <- formula
    priorList$formulaRho= formulaRho
    priorList$rhoPrior = rhoPrior
  }
  if(termA){
    alphaSign  <- matrix(-1, n_spec, n_spec)
    colnames(alphaSign) <- rownames(alphaSign) <- s_names
    priorList$alphaSign = alphaSign
  }
  tmp <- gjamTimePrior(xdata, ydata, edata, priorList)
  timeList <- mergeList(tlist, tmp)
  
  ## fit gjam
  modelList <- list(typeNames = 'DA', ng = 2500, burnin = 1500,  
                    timeList = timeList, effort = effort)
  cat("    running gjam \n")
  output <- gjam(formula, xdata=xdata, ydata=ydata, modelList=modelList)
  
  ## save and plot
  outFolder <- get_outfolder(name)
  
  if(showPlot){
    plotPars  <- list(PLOTALLY=T,
                      SAVEPLOTS = saveOutput, outFolder = outFolder)
    gjamPlot(output, plotPars)
  }
  if(saveOutput){
    call <- setup
    call$xdata <- NULL
    call$ydata <- NULL
    call$edata <- NULL
    saveRDS(call, file = file.path(outFolder, "call.rds"))
    output_short <- select_output(output) #to not blow up output file
    save(output_short, file = paste0(outFolder, "/output.rdata"))
    print_call(setup, output_file = paste0(outFolder, "/call.txt"))
    cat("\n    output available in", outFolder, "\n")
  }
  # gerneral reset
  if(tracking || fixWarning){stop_redirect_gjam()}
  ## return fitted gjam
  return(output)
  
  ## <<<<<<<<<<<< (old)
}
