# This Script harbours functions that return ydata and xdata based on the 
# selected variables in gjamTime.R

## set up environment ####

# packages
library(dplyr)
library(terra)
library(devtools)
library(gjam)
source("Scripts/gjamTime/load_gjamTimeFunctions.R")

## timing ####
start_time <- function(){
  start_time <- Sys.time()
  return(start_time)
}
end_time <- function(time){
  delta <- format(round(Sys.time() - time, 2))
  return(delta)
}

## For version ####
updateArgs <- function(vlist, sysArgs){
  if (!exists("vers", where = vlist) || length(vlist$vers) == 0){
    vlist$vers <- "full"
  }
  if (!exists("subset", where = vlist) || !is.logical(vlist$subset)){
    vlist$subset <- TRUE
  }
  if (!exists("subFact", where = vlist) || length(vlist$subFact) == 0){
    vlist$subFact <- 100
  }
  if (!exists("subSeed", where = vlist) || length(vlist$subSeed) == 0){
    vlist$subSeed <- 0
  }
  # update version from sysArgs
  if (length(sysArgs) > 0) {
    vlist$vers <- sysArgs[1]
  }
  
  # if we subset data
  if(vlist$subset || (length(sysArgs) > 1)){
    vlist$subset <- TRUE
    
    # get sysArgs
    if(length(sysArgs) > 1){
      vlist$subFact <- sysArgs[2]
      if(length(sysArgs) > 2){
        set.seed(1234)
        random_numbers <- sample(2764:8652, 160, replace = FALSE)
        index <- as.integer(sysArgs[3]) #87-160
        if(index < 1 || index > 160 || is.na(index)){stop("invalid index (sysArgs[3]) provided")}
        vlist$subSeed <- random_numbers[index]
      }
    }
    
    # check subset
    vlist$subFact <- as.integer(vlist$subFact)
    vlist$subSeed <- as.integer(vlist$subSeed)
    if(vlist$subFact < 1 || vlist$subFact > 10000){stop("invalid subFact")}
    vlist$subSeed <- as.integer(vlist$subSeed%%((vlist$subFact)**2))
    vlist$name <- "subs"
    
  }else{ #if not subset data
    vlist$subset <- FALSE
    vlist$subFact <- 1
    vlist$subSeed <- 0
    vlist$name <- vlist$vers
  }
  return(vlist)
}


## Assertion Function ####
getvars <- function(varlist){
  vars <- c()
  for(name in names(varlist)){
    var <- c()
    if (name != "x" && name != "y" &&
        name != "periods" && name != "version"){var <- varlist[[name]]}
    if(name == "x" && varlist[[name]]){var <- c("lon")}
    if(name == "y" && varlist[[name]]){var <- c("lat")}
    vars <- c(vars, var)
  }
  return(vars)
}

print_call <- function(call, output_file = NULL){
  if (!is.null(output_file)) {
    # Redirect output to the specified file
    sink(output_file)
  }
  insert <- "    "
  cat(" \n")
  cat("call: \n")
  cat(insert, "name: ", call$name, "\n")
  cat(insert, "version: ", call$version, "\n")
  cat(insert, "subset: ", call$subset, "\n")
  cat(insert, "subSeed: ", call$subSeed, "\n")
  cat(insert, "subFact: ", call$subFact, "\n")
  cat(insert, "xvariables: \n")
  cat(insert, insert, "topography: ", call$xvars$topography, "\n")
  cat(insert, insert, "climate: ", call$xvars$climate, "\n")
  cat(insert, insert, "soil: ", call$xvars$soil, "\n")
  cat(insert, insert, "wildfire: ", call$xvars$wildfire, "\n")
  cat(insert, insert, "interaction: ", call$xvars$interaction, "\n")
  cat(insert, "yvariables: \n")
  cat(insert, insert, "vegetation:", call$yvars$vegetation, "\n")
  cat(insert, "periods: ", call$periods, "\n")
  
  if (!is.null(output_file)) {
    # Stop redirecting output
    sink()
  }
}

assert_geodata <- function(var_list){
  # valid names
  if(!all(names(var_list) %in% names(masterlist_variables))){
    stop("Invalid key encountered in variable-list")
  }
  # existing names
  if (!exists("x", where = var_list)){var_list$x = FALSE}
  if (!exists("y", where = var_list)){var_list$y = FALSE}

  # length of variables
  if(length(var_list$vegetation) == 0 &
     length(var_list$topography) == 0 &
     var_list$x == FALSE &
     var_list$y == FALSE &
     length(var_list$climate) == 0 &
     length(var_list$wildfire) == 0 &
     length(var_list$soil) == 0){
    if(var_list$x || var_list$y){
      stop("additional predictors including to lat/lon required")
    }else{
      stop("No variables specified")
    }
  }
  # entries
  if(length(var_list$vegetation) != 0 &
    !all(var_list$vegetation %in% masterlist_variables$vegetation)){
    stop("Invalid vegetation variables")
  }
  if(length(var_list$topography) != 0 &
     !all(var_list$topography %in% masterlist_variables$topography)){
    stop("Invalid topography variables")
  }
  if(length(var_list$x) != 0 &
     !all(var_list$x %in% masterlist_variables$x)){
    stop("Invalid x variable")
  }
  if(length(var_list$y) != 0 &
     !all(var_list$y %in% masterlist_variables$y)){
    stop("Invalid y variable")
  }
  if(length(var_list$climate) != 0 &
     !all(var_list$climate %in% masterlist_variables$climate)){
    stop("Invalid climate variables")
  }
  if(length(var_list$wildfire) != 0 &
     !all(var_list$wildfire %in% masterlist_variables$wildfire)){
    stop("Invalid wildfire variables")
  }
  if(length(var_list$soil) != 0 &
     !all(var_list$soil %in% masterlist_variables$soil)){
    stop("Invalid soil variables")
  }
  return(var_list)
}

assert_gjamCall <- function(vlist, xvars, yvars, periods, callName){
  #initialize
  call_list <- list(
    name = paste(callName,
                 vlist$vers,
                 paste0(vlist$name, vlist$subFact),
                 sprintf(paste0("%04d"), vlist$subSeed),
                 sep = "_"),
    version = vlist$vers,
    subset = vlist$subset,
    subSeed = vlist$subSeed,
    subFact = vlist$subFact,
    periods = periods,
    xvars = xvars,
    yvars = yvars
  )
  
  
  # valid keys
  if(!all(names(call_list) %in% mastervector_call)){
    stop("Invalid key encountered in call-list")
  }
  # existing keys
  if (!exists("name", where = call_list) || length(call_list$name) == 0){stop("No name specified")}
  if (!exists("periods", where = call_list) || length(call_list$periods) == 0){stop("No periods specified")}
  if (!exists("version", where = call_list) || length(call_list$version) == 0){stop("No version specified")}
  if (!exists("xvars", where = call_list) || length(call_list$xvars) == 0){stop("No xvars list specified")}
  if (!exists("yvars", where = call_list) || length(call_list$yvars) == 0){stop("No yvars list specified")}
  

  if(!is.character(call_list$name) ||
     is.na(call_list$name) ||
     nchar(call_list$name) == 0){
    stop("Invalid name specified")
  }
  if(length(call_list$periods) != 0 &
     !all(call_list$periods %in% masterlist_variables$periods)){
    stop("Invalid periods variables")
  }
  if(length(call_list$version) != 0 &
     !all(call_list$version %in% masterlist_variables$version)){
    stop("Invalid version specified")
  }
  if(length(call_list$version) > 1){
    stop("Too many versions specified")
  }
  call_list$xvars$periods = call_list$periods
  call_list$yvars$periods = call_list$periods
  call_list$xvars$version = call_list$version
  call_list$yvars$version = call_list$version
  
  call_list$xvars <- assert_geodata(call_list$xvars)
  call_list$yvars <- assert_geodata(call_list$yvars)
  
  print_call(call_list)
  
  return(call_list)
}


## Prepare the geotifs (from main.R) to load in further step (in gjamTime.R)
#' this function writes for each variable and period (each geotiff in 
#' data/gjamTime_data) a subset in data/gjamTime_tmp with the current seed.
prepare_geodata <- function(call){
 
  # Only run if we actually take a subset of "full" or "crop"
  if(!call$subset){return(NULL)}
  
  # clean data/gjamTime_tmpSubset directory
  filesRM <- list.files(path_tmp, full.names = TRUE)
  file.remove(filesRM)
  
  #make samplemask
  samplemask <- make_samplemask(call$subSeed, call$subFact)
  writeRaster(samplemask, file.path(path_tmp, "samplemask.tif"))
  
  sample_geodata(var_list = call$xvars, which = "xdata", samplemask = samplemask)
  sample_geodata(var_list = call$yvars, which = "ydata", samplemask = samplemask)
}

sample_geodata <- function(var_list, which, samplemask){
  cat("    sampling", which, "\n")
  # for all periods
  n_time <- length(var_list$periods)
  includeInteraction <- (length(var_list$interaction > 0))
  for(per in 1:n_time){
    cat("        loading period", var_list$periods[per], "\n")
    # get files
    file_var_list <- get_filenames(var_list$periods[per], var_list, interaction = FALSE)
    files_this_period <- file_var_list$files
    vars_this_period <- file_var_list$variables
    #file paths
    file_paths_in <- file.path(path_vars, files_this_period)
    file_paths_out <- file.path(path_tmp, files_this_period)
    # load, mask and save each independently
    for(i in 1:length(files_this_period)){
      #load
      raster <- rast(file_paths_in[i])
      names(raster) <- vars_this_period[i]
      #mask
      raster <- mask(x=raster,
                     mask = samplemask,
                     maskvalues=0, updatevalue=NA)
      #save
      writeRaster(raster,
                  file_paths_out[i],
                  overwrite=TRUE) #overwrite important
    }
    if(includeInteraction){
      for(i in 1:length(var_list$interaction)){
        thisVar <- var_list$interaction[i]
        vars <- unlist(strsplit(thisVar, ":"))
        name_var1 <- files_this_period[grep(paste0("*", vars[1], "*"), files_this_period)]
        name_var2 <- files_this_period[grep(paste0("*", vars[2], "*"), files_this_period)]
        var1 <- rast(file.path(path_tmp, name_var1))
        var2 <- rast(file.path(path_tmp, name_var2))
        combined <- var1*var2
        #save
        nameshort <- paste("inter",var_list$periods[per],vars[1],vars[2],var_list$version, sep = "_")
        writeRaster(combined,
                    file.path(path_tmp,
                              paste0(nameshort, ".tif")),
                    overwrite=TRUE) #overwrite important
      }
    }
  }
}

#' makes a regular samplemask from dimensions of mastermask, based on density 
#' (fact) and sample ID (seed, which specifies what regular grid to sample)
make_samplemask <- function(subSeed, subFact){
  
  seedI <- as.integer(floor(subSeed/subFact))
  seedJ <- as.integer(subSeed %% subFact)
  fact <- subFact
  
  #create sample mask
  mastermask <- rast(file.path(path_masks, "master_mask.tif"))
  samplemask <- mastermask
  samplemask[] <- FALSE
  
  # Get the dimensions and seed of the raster
  dims <- dim(samplemask)
  
  # Create indices for the reduced sampling
  reduce_i <- seq(seedI+1, dims[1], by = fact)
  reduce_j <- seq(seedJ+1, dims[2], by = fact)
  
  # Create a matrix of indices to set to TRUE
  indices <- expand.grid(reduce_i, reduce_j)
  samplemask[cbind(indices$Var1, indices$Var2)] <- TRUE
  
  # get samplemask
  samplemask <- mastermask & samplemask
  
  return(samplemask)
}


## helper function get files ####

# returns a list of $files with filenames in data/gjamTime_data/ and $variables
get_filenames <- function(period, var_list, interaction=FALSE){
  vers <- var_list$version
  file_vec <- c()
  var_vec <- c()
  for (var in var_list$vegetation){
    filename <- paste("veg", period, var, vers, sep = "_")
    filename <- paste0(filename, ".tif")
    file_vec <- append(file_vec, filename)
    var_vec <- append(var_vec, var)
  }
  for (var in var_list$topography){
    filename <- paste("topo", "const", var, vers, sep = "_")
    filename <- paste0(filename, ".tif")
    file_vec <- append(file_vec, filename)
    var_vec <- append(var_vec, var)
  }
  for (var in var_list$climate){
    filename <- paste("clim", period, var, vers, sep = "_")
    filename <- paste0(filename, ".tif")
    file_vec <- append(file_vec, filename)
    var_vec <- append(var_vec, var)
  }
  for (var in var_list$wildfire){
    filename <- paste("fire", period, var, vers, sep = "_")
    filename <- paste0(filename, ".tif")
    file_vec <- append(file_vec, filename)
    var_vec <- append(var_vec, var)
  }
  for (var in var_list$soil){
    filename <- paste("soil", "const", var, vers, sep = "_")
    filename <- paste0(filename, ".tif")
    file_vec <- append(file_vec, filename)
    var_vec <- append(var_vec, var)
  }
  if(interaction){
    for (var in var_list$interaction){
      vars <- unlist(strsplit(var, ":"))
      filename <- paste("inter", period, vars[1], vars[2], vers, sep = "_")
      filename <- paste0(filename, ".tif")
      file_vec <- append(file_vec, filename)
      var_vec <- append(var_vec, var)
    }
  }
  list <- list("files" = file_vec,
               "variables" = var_vec)
  return(list)
}


## generalized getdata function ####

# takes list of variables and returns the dataframe
# by default sorted after group, within group after time
get_geodata <- function(call,
                        which,
                        dropgroup = TRUE,
                        dropperiod = TRUE){
  
  var_list <- NULL
  if(which == "xdata" || which == "ydata"){
    if(which == "xdata"){var_list = call$xvars}
    if(which == "ydata"){var_list = call$yvars}
  }else{stop("invalid argument: which, should be either 'xdata' or 'ydata' \n")}
  
  # initialize list of dataframes
  data_list <- list()
  path_files <- path_vars
  if(call$subset){path_files = path_tmp}
  # iterate over all periods
  n_time <- length(var_list$periods)
  for(per in 1:n_time){
    cat("    loading period", var_list$periods[per])
    # get files and variables in same order
    file_var_list <- get_filenames(var_list$periods[per], var_list, interaction=TRUE)
    # get files
    files_this_period <- file_var_list$files
    # make raster
    file_paths_full <- file.path(path_files, files_this_period)
    raster_this_period <- rast(file_paths_full)
    names(raster_this_period) <- file_var_list$variables
    
    # make dataframe
    getxy <- (var_list$x | var_list$y)
    cat(", converting to dataframe...")
    df <- as.data.frame(raster_this_period, xy = getxy,
                        cells = TRUE, na.rm = NA)
    cat(" done. \n")
    # Rename x and y to lat and lon
    colnames(df)[colnames(df) == "x"] <- "lon"
    colnames(df)[colnames(df) == "y"] <- "lat"
    if(getxy){
      if(!var_list$x){df <- df %>% dplyr::select(-lon)}
      if(!var_list$y){df <- df %>% dplyr::select(-lat)}
    }
    
    # add time
    df$period <- per
    data_list[[per]] <- df
  }
  #conbine the dataframes
  combined_df <- do.call(rbind, data_list)
  # order
  ordered_df <- combined_df %>%
    dplyr::arrange(cell, period)
  
  # Get the current column names
  cols <- colnames(ordered_df)
  
  # Define the new order of columns
  new_order <- c("cell", "period", cols[!cols %in% c("cell", "period")])
  
  # Reorder the columns
  ordered_df <- ordered_df[, new_order]
  
  # Set row names starting from 1
  row.names(ordered_df) <- NULL
  
  # Optionally drop the 'cell' and 'period' columns if specified
  if (dropgroup) {
    ordered_df <- subset(ordered_df, select = -c(cell))
  }
  if (dropperiod) {
    ordered_df <- subset(ordered_df, select = -c(period))
  }
  cat("    returning dataframe of nrows =", nrow(ordered_df), ", ncols =", ncol(ordered_df), "\n")
  
  # returning dataframe
  return(ordered_df)
}


## helper functions other for fittin gjamTime ####

## stop execution if NA
execute_code <- function() {
  # Prompt user to proceed
  confirmation <- readline(prompt = "Proceed? [y] / n: ")

  # Check user input
  if (tolower(confirmation) == "y"|| tolower(confirmation) == "") {
    cat("Proceeding with execution...\n")
  } else if (tolower(confirmation) == "n") {
    stop("Execution aborted by user.")  # Stop execution at this point
  } else {
    message("Invalid input. Please enter 'y' to proceed or 'n' to abort.")
    execute_code()  # Prompt user again if input is invalid
  }
}

## assert the dataframes are not empty
check_dataframe <- function(df){
  n_NA <- sum(is.na(df))
  if(n_NA > 0){
    warning(n_NA, " NA values encountered")
    totCol <- nrow(df)
    for (col in names(df)) {
      column_vector <- df[[col]]
      n_NA_col <- sum(is.na(column_vector))
      warning("   ",col, ": ", n_NA_col, " NA values out of ", totCol)
    }
    # Prompt user to proceed
    execute_code()
  } else {}
}


## get path for saving folder in increments if path exists
get_outfolder <- function(name){
  dir_path <- paste0(path_save, name)

  # Check if the directory already exists
  if (dir.exists(dir_path)) {
    # If it exists, find a new directory name
    i <- 1
    while (dir.exists(dir_path)) {
      dir_path <- file.path(path_save, paste0(name, "-", i))
      i <- i + 1
    }
  }
  return(dir_path)
}

## fill priorLists
fill_priorList <- function(all_vars, priorlist){
  
  # # Values to add corresponding to lo and hi
  # loValues <- rep(-Inf, length(all_vars))
  # hiValues <- rep(Inf, length(all_vars))
  loValues <- rep(-100, length(all_vars))
  hiValues <- rep(100, length(all_vars))
  # Add variables and their values dynamically
  for (i in seq_along(all_vars)) {
    priorlist$lo[[all_vars[i]]] <- loValues[i]
    priorlist$hi[[all_vars[i]]] <- hiValues[i]
  }
  return(priorlist)
}

## normalize xdata
normalize_gjamInput_ref <- function(xdata, vars){
  #each column must be normalized with mean and sd
  path_ref_list_fullname <- paste0(path_ref_list, ref_list_name)
  ref_list <- readRDS(path_ref_list_fullname)
  for(col in vars){
    xdata[[col]] <- (xdata[[col]]-ref_list[[col]]$mean)/ref_list[[col]]$sd
  }
  return(xdata)
}
normalize_gjamInput_this <- function(xdata, vars){
  #each column must be normalized with mean and sd
  for(col in vars){
    mu <- mean(xdata[[col]], na.rm = TRUE)
    sd <- sd(xdata[[col]], na.rm = TRUE)
    if(sd == 0){(xdata[[col]]-mu)}
    else{xdata[[col]] <- (xdata[[col]]-mu)/sd}
  }
  return(xdata)
}

## tracking and redirecting
redirect_gjam <- function(){
  source("Scripts/gjamTime/adjustments_gjamTime.R")
  environment(.rhoPriorMod) <- asNamespace('gjam')
  assignInNamespace(".rhoPrior", .rhoPriorMod, ns = "gjam")
}
start_track_gjam <- function(){
  source("Scripts/gjamTime/tracking_gjamTime.R")
  environment(.gjamMod) <- asNamespace('gjam')
  assignInNamespace(".gjam", .gjamMod, ns = "gjam")
  environment(.rhoPriorMod) <- asNamespace('gjam')
  assignInNamespace(".rhoPrior", .rhoPriorMod, ns = "gjam")
  environment(.getTimeIndexMod) <- asNamespace('gjam')
  assignInNamespace(".getTimeIndex", .getTimeIndexMod, ns = "gjam")
  environment(.betaWrapperMod) <- asNamespace('gjam')
  assignInNamespace(".betaWrapper", .betaWrapperMod, ns = "gjam")
  environment(.tnormMVNmatrixMod) <- asNamespace('gjam')
  assignInNamespace(".tnormMVNmatrix", .tnormMVNmatrixMod, ns = "gjam")
}
stop_redirect_gjam <- function(){
  # reload package
  detach("package:gjam", unload=TRUE)
  library(gjam)
}

## fill means in fields with NA

fillmeans <- function(df, vars) {
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


## selcet output entries ####
select_output<- function(output){
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
    rhoStandXmu = rhoStandXmu,
    rhoStandXse = rhoStandXse,
    sensAlpha = sensAlpha,
    sensRho =sensRho
  )
  return(outlist)
}

## fit gjam function ####

fit_gjamTime <- function(setup,
                         termB = FALSE,
                         termR = TRUE,
                         termA = TRUE,
                         normalize = TRUE,
                         # normalize = "ref" for normalizing to reference fulldata, 1984-2020
                         saveOutput = TRUE,
                         showPlot = TRUE){  # TODO if showplot FALSE it crashes
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
  
  # fill means of not group varaibles manually
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
  modelList <- list(typeNames = 'DA', ng = 25, burnin = 15,  
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
    output_short <- select_output(output) #to not blow up output file
    save(output_short, file = paste0(outFolder, "/output.rdata"))
    print_call(setup, output_file = paste0(outFolder, "/call.txt"))
    cat("\n    output available in", outFolder, "\n")
  }
  # gerneral reset
  if(tracking || fixWarning){stop_redirect_gjam()}
  ## return fitted gjam
  return(output)
}


## definitions ####

# bash calls
sysArgs <- commandArgs(trailingOnly = TRUE)

# Script version
fixWarning <- TRUE
tracking <- FALSE

# fitting general
termB <- FALSE    # include immigration/emigration term XB
termR <- TRUE     # include DI population growth term VL
termA <- TRUE    # include DD spp interaction term UA

# paths
path_vars <- "data/gjamTime_data/"
path_tmp <- "data/gjamTime_tmpSubset/"
path_masks <- "data/Masks/"
path_save <- "data/gjamTime_outputs/"
ref_list_name <- "normalization.rds"
path_ref_list <- "Scripts/gjamTime/"

# masterlist of all variables
masterlist_variables <- list(
  vegetation = c("sh", "cf", "hb", "lc", "wt", "br"),
  topography = c("elev", "slope", "aspect", "cosasp", "tpi"),
  x = c(TRUE, FALSE),
  y = c(TRUE, FALSE),
  climate = c("tas", "tasw", "tass", "pr", "prw", "prs"),
  wildfire = c(),
  soil = c("wvol", "wvol05", "wvol15", "wvol30", "wvol60", "scwd"),
  periods = c("1984-1990",
              "1991-1996",
              "1997-2002",
              "2003-2008",
              "2009-2014",
              "2015-2020"),
  version = c("full", "crop"),
  interaction = c()
)

# masterlist of gjam call
mastervector_call <- c("name", "version", "periods", "xvars", "yvars",
                       "subset", "subSeed", "subFact")
