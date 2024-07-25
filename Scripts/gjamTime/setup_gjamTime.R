# This Script harbours functions that return ydata and xdata based on the 
# selected variables in gjamTime.R

## set up environment ####

# packages
library(dplyr)
library(terra)
library(devtools)
library(gjam)

path_gjam <- "https://github.com/jimclarkatduke/gjam/blob/master/gjamTimeFunctions.R?raw=True"
#source_url(path_gjam)

## Assertion Function ####

assert_geodata <- function(var_list){
  # valid names
  if(!all(names(var_list) %in% names(masterlist_variables))){
    stop("Invalid key encountered in variable-list")
  }
  # existing names
  # if (!exists("vegetation", where = var_list)){var_list$vegetation = c()}
  # if (!exists("topography", where = var_list)){var_list$topography = c()}
  if (!exists("x", where = var_list)){var_list$x = FALSE}
  if (!exists("y", where = var_list)){var_list$y = FALSE}
  # if (!exists("climate", where = var_list)){var_list$climate = c()}
  # if (!exists("wildfire", where = var_list)){var_list$wildfire = c()}
  # if (!exists("soil", where = var_list)){var_list$soil = c()}
  # if (!exists("periods", where = var_list)){stop("No period specified")}
  # if (!exists("version", where = var_list)){var_list$version = "r100"}
  # length
  if(length(var_list$vegetation) == 0 &
     length(var_list$topography) == 0 &
     var_list$x == FALSE &
     var_list$y == FALSE &
     length(var_list$climate) == 0 &
     length(var_list$wildfire) == 0 &
     length(var_list$soil) == 0){stop("No variables specified")}
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
  # if(length(var_list$periods) != 0 &
  #    !all(var_list$periods %in% masterlist_variables$periods)){
  #   stop("Invalid periods variables")
  # }
  # if(length(var_list$version) != 0 &
  #    !all(var_list$version %in% masterlist_variables$version)){
  #   stop("Invalid version")
  # }
  return(var_list)
}

assert_gjamCall <- function(call_list){
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
  
  return(call_list)
}


## helper function get files ####
# returns a list of $files with filenames in data/gjamTime_data/ and $variables
get_filenames <- function(period, var_list){
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
  list <- list("files" = file_vec,
               "variables" = var_vec)
  return(list)
}


## generalized getdata function ####

# takes list of variables and returns the dataframe
# by default sorted after group, within group after time

get_geodata <- function(var_list, dropgroup = TRUE, dropperiod = TRUE){
  # initialize list of dataframes
  data_list <- list()
  
  # iterate over all periods
  cat("loading dataframes ... \n")
  n_time <- length(var_list$periods)
  for(t in 1:n_time){
    cat("loading period", var_list$periods[t],"\n")
    # get files and variables in same order
    file_var_list <- get_filenames(var_list$periods[t], var_list)
    # get files
    files_this_period <- file_var_list$files
    # make raster
    file_paths <- file.path(path_vars, files_this_period)
    raster_this_period <- rast(file_paths)
    names(raster_this_period) <- file_var_list$variables
    
    # make dataframe
    cat("      converting raster in dataframe \n")
    getxy <- (var_list$x | var_list$y)
    df <- as.data.frame(raster_this_period, xy = getxy,
                                        cells = TRUE, na.rm = NA)
    # Rename x and y to lat and lon
    colnames(df)[colnames(df) == "x"] <- "lon"
    colnames(df)[colnames(df) == "y"] <- "lat"
    if(getxy){
      if(!var_list$x){df <- df %>% dplyr::select(-lon)}
      if(!var_list$y){df <- df %>% dplyr::select(-lat)}
    }
    
    # add time
    df$period <- t
    data_list[[t]] <- df
    cat(t, "out of", n_time,"dataframes loaded \n")
  }
  cat("unite dataframes \n")
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
  cat("done\n")
  cat("returning dataframe of nrows =", nrow(ordered_df), ", ncols =", ncol(ordered_df), "\n")
  
  # returning dataframe
  return(ordered_df)
}


## helper functions other ####

## stop execution if NA
execute_code <- function() {
  # Prompt user to proceed
  confirmation <- readline(prompt = "Proceed? [y] / n: ")

  # Check user input
  if (tolower(confirmation) == "y"|| tolower(confirmation) == "") {
    cat("Proceeding with execution...")
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
      n_NA_col <- is.na(column_vector)
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
      dir_path <- file.path(path_save, paste0(name, "(", i, ")"))
      i <- i + 1
    }
  }
  return(dir_path)
}

# fill priorLists
fill_priorList <- function(all_vars, priorlist){
  
  # Values to add corresponding to lo and hi
  loValues <- rep(-Inf, length(all_vars))
  hiValues <- rep(Inf, length(all_vars))
  
  # Add variables and their values dynamically
  for (i in seq_along(all_vars)) {
    priorlist$lo[[all_vars[i]]] <- loValues[i]
    priorlist$hi[[all_vars[i]]] <- hiValues[i]
  }
  return(priorlist)
}

# normalize xdata
normalize_gjamInput <- function(xdata, vars){
  #each column must be normalized with mean and sd
  path_ref_list_fullname <- paste0(path_ref_list, ref_list_name)
  ref_list <- readRDS(path_ref_list_fullname)
  for(col in vars){
    xdata[[col]] <- (xdata[[col]]-ref_list[[col]]$mean)/ref_list[[col]]$sd
  }
  return(xdata)
}

## DELETE THIS START ####

# .getStandX_modified <- function (formula, xu, standRows, xmu = NULL, xsd = NULL, verbose = F) 
# {
#   cat("Debug: Inside .getStandX\n")
#   if (length(standRows) == 0) {
#     xs <- model.matrix(formula, data.frame(xu))
#     colnames(xs)[1] <- "intercept"
#     return(list(xstand = xs, xdataStand = xu, xmu = xmu, 
#                 xsd = xsd, U2S = diag(ncol(xs))))
#   }
#   xu <- xdataStand <- as.data.frame(xu)
#   cat("head xu: \n")
#   print(head(xu))
#   cat("dim xu:", dim(xu), "\n")
#   inCols <- colnames(xu)
#   print(inCols)
#   ifact <- which(sapply(data.frame(xu), is.factor))
#   if (is.null(xmu)) 
#     xmu <- colMeans(xu[, standRows, drop = F], na.rm = T)
#   if (is.null(xsd)) 
#     xsd <- apply(xu[, standRows, drop = F], 2, sd, na.rm = T)
#   xs <- xu
#   wna <- which(sapply(xu, is.na), arr.ind = T)
#   wni <- which(wna[, 2] %in% ifact)
#   if (length(wni) > 0) {
#     wni <- wna[wni, ]
#     wna <- wna[-wni, ]
#   }
#   wna <- which(is.na(xu[, standRows]), arr.ind = T)
#   if (length(wna) > 0) {
#     xs[, standRows][wna] <- xmu[wna[, 2]]
#     xu[, standRows][wna] <- xmu[wna[, 2]]
#     if (length(wni) > 0) {
#       fcol <- unique(wni[, 2])
#       for (m in fcol) {
#         mf <- levels(xu[, m])[1]
#         wm <- wni[wni[, 2] == m, ]
#         xu[wm] <- mf
#       }
#     }
#   }
#   sc <- standRows
#   if (!is.character(sc)) 
#     sc <- names(standRows)
#   xs[, sc] <- t((t(xs[, sc, drop = F]) - xmu[sc])/xsd[sc])
#   xdataStand[, sc] <- xs[, sc, drop = F]
#   xs <- model.matrix(formula, data.frame(xs))
#   colnames(xs)[1] <- "intercept"
#   xu <- model.matrix(formula, xu)
#   colnames(xs)[1] <- colnames(xu)[1] <- "intercept"
#   bigSD <- apply(xs, 2, range)
#   wk <- which(abs(bigSD) > 20, arr.ind = T)
#   if (length(wk) > 0) {
#     b <- paste0(colnames(bigSD)[wk[, 2]], collapse = ", ")
#     toConsole("\nNote: Values in x more than 20 SDs from the mean of fitted data", 
#               b, verbose)
#     FLAG <- T
#   }
#   XX <- crossprod(xs)
#   ssx <- try(solveRcpp(XX), T)
#   if (inherits(ssx, "try-error")) {
#     u2s <- diag(ncol(xs))
#     attr(u2s, "valid") <- FALSE
#   }
#   else {
#     u2s <- ssx %*% crossprod(xs, xu)
#     u2s[abs(u2s) < 1e-10] <- 0
#     attr(u2s, "valid") <- TRUE
#   }
#   rownames(u2s) <- colnames(xs)
#   colnames(u2s) <- colnames(xu)
#   if (length(wna) > 0) {
#     xs[wna] <- 0
#     xdataStand[wna] <- NA
#   }
#   list(xstand = xs, xdataStand = xdataStand, xmu = xmu, xsd = xsd, 
#        U2S = u2s)
# }
# 
# environment(.getStandX_modified) <- asNamespace('gjam')
# assignInNamespace(".getStandX", .getStandX_modified, ns = "gjam")
# 
# # Restart R session or reload package
# detach("package:gjam", unload=TRUE)
# library(gjam)

## DELETE THIS END ####

## fit gjam function ####

fit_gjamTime <- function(setup,
                         termB = FALSE,
                         termR = TRUE,
                         termA = TRUE,
                         saveOutput = TRUE,
                         showPlot = TRUE){

  xdata <- as.data.frame(setup$xdata)
  ydata <- as.matrix(setup$ydata)
  rownames(ydata) <- NULL
  edata <- matrix(1, nrow = nrow(ydata), ncol = ncol(ydata))
  colnames(edata) <- colnames(ydata)
  
  xvars_list <- setup$xvars
  yvars_list <- setup$yvars
  name <- setup$name

  ## Missing values in time series data

  # check if there are missing values
  check_dataframe(ydata)
  check_dataframe(xdata)

  # prepare
  timeCol   <- "period" # Column that stores time
  groupCol  <- "cell" # column that stores the group ID
  constVars <- c(xvars_list$topography,
                 xvars_list$soil) #TODO wildfire when fitting
  if(xvars_list$x){constVars <- c(constVars, "lon")}
  if(xvars_list$y){constVars <- c(constVars, "lat")}
  
  groupVars <- c("cell", constVars)
  allVars <- c(constVars, xvars_list$climate, xvars_list$wildfire)
  
  xdata <- normalize_gjamInput(xdata, allVars)
  missingEffort <- 0.1 # set this
  
  # fit missing values
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
  
  formula <- as.formula(paste("~ ", paste(allVars, collapse = " + ")))

  ##  set priorlist
  priorList <- list()
  if(termB){
    # set priordistribution (intercept = (-Inf, Inf) and all other vars = (-Inf, Inf))
    betaPrior  <- list(lo = list(intercept = -Inf),
                       hi = list(intercept = Inf) )
    betaPrior <- fill_priorList(allVars, betaPrior)
    formulaBeta <- formula
    priorList$formulaBeta = formulaBeta
    priorList$betaPrior = betaPrior
  }
  if(termR){
    # set priordistribution (intercept = (-1,1), all other vars = (-Inf, Inf))
    rhoPrior  <- list(lo = list(intercept = 0),
                      hi = list(intercept = 2) )
    rhoPrior <- fill_priorList(allVars, rhoPrior)
    formulaRho <- formula
    priorList$formulaRho= formulaRho
    priorList$rhoPrior = rhoPrior
  }
  if(termA){
    # from Nill 2022 Fig 6 https://doi.org/10.1016/j.rse.2022.113228
    # obs = a + b*pred
    # obs max = 10000,==> 10000 = a + b*predmax, here predmax in alpha_list
    alpha_list <- list("sh" = 9832.673,
                       "cf" = 18187.72,
                       "hb" = 11384.09,
                       "lc" = 24568.29) 
    alphaSign  <- matrix(0, n_spec, n_spec)
    colnames(alphaSign) <- rownames(alphaSign) <- s_names
    for(i in 1:n_spec){
      var <- s_names[i]
      alphaSign[var,var] <- alpha_list[[var]]
      # alphaSign[var,var] <- -1
    }
    priorList$alphaSign = alphaSign
  }
  tmp <- gjamTimePrior(xdata, ydata, edata, priorList)
  timeList <- mergeList(tlist, tmp)

  ## fit gjam
  modelList <- list(typeNames = 'DA', ng = 10000, burnin = 5000,  
                    timeList = timeList, effort = effort)
  output <- gjam(formula, xdata=xdata, ydata=ydata, modelList=modelList)

  ## save and plot
  outFolder <- get_outfolder(name)

  if(showPlot){
    plotPars  <- list(PLOTALLY=T, trueValues = trueValues,
                      SAVEPLOTS = saveOutput, outFolder = outFolder)
    gjamPlot(output, plotPars)
  }
  if(saveOutput){
    save(output, file = paste0(outFolder, "/output.rdata"))
  }

  ## return fitted gjam
  return(output)
}


## definitions ####

# paths
path_vars <- "data/gjamTime_data/"
path_save <- "data/gjamtime_outputs/"
ref_list_name <- "normalisation.rds"
path_ref_list <- "data/gjamTime_data/"

# masterlist of all variables
masterlist_variables <- list(
  vegetation = c("sh", "cf", "hb", "lc", "wt", "br"),
  topography = c("elev", "slope", "aspect", "tpi"),
  x = c(TRUE, FALSE),
  y = c(TRUE, FALSE),
  climate = c("tas", "tasw", "tass", "pr", "prw", "prs"),
  wildfire = c(),
  soil = c("wvol", "wvol05", "wvol15", "wvol30", "wvol60"),
  periods = c("1984-1990",
              "1991-1996",
              "1997-2002",
              "2003-2008",
              "2009-2014",
              "2015-2020"),
  version = c("full", "r100", "crop")
)

# masterlist of gjam call
mastervector_call <- c("name", "version", "periods", "xvars", "yvars")


## print when script is called ####

cat("Valid entries for variable list: \n")
for(keyname in names(masterlist_variables)){
  cat(keyname,": ", masterlist_variables[[keyname]], "\n")
}
