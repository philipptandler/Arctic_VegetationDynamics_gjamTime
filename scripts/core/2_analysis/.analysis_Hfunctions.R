################################################################################
## libraries ####
################################################################################

library(terra)

source("scripts/core/2_analysis/.chunk_process.R")


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

.validate_times <- function(call, times_out){
  if(is.logical(times_out)){
    if(length(times_out) != length(call$times)){
      warning("length times_out does not match length of call$times.
              Return times_out = ", paste(call$times[times_out], collapse = "; "))
    }
    times_out <- call$times[times_out]
  } else {
    henv <- new.env()
    source("scripts/core/1_gjamTime/.gjamTime_Hfunctions.R", local = henv)
    valid_times <- henv$.receive_validVariables()$times
    if(is.null(times_out)){
      times_out <- call$times
    } else {
      if(!(all(times_out %in% valid_times))){stop("one or more element of provided times_out not valid")}
    }
  }
  times_out
}

# returns list of all vars and all paths
.get_norm_raster_list <- function(tm, call, output_mask, full_path = TRUE){
  
  henv <- new.env()
  source("scripts/core/1_gjamTime/.gjamTime_Hfunctions.R", local = henv)

  if(isFALSE(call$subset)){
    dataIn <- "alldata"
  } else {
    dataIn <- henv$.toLowerCamelCase(tools::file_path_sans_ext(basename(call$subset$mask)))
  }
  timeCode <- henv$.binTimeCode(call$times)
  if(!is.null(output_mask)){
    if(file.exists(output_mask) || file.exists(file.path(path_masks, output_mask))){
      maskOut <- henv$.toLowerCamelCase(tools::file_path_sans_ext(basename(output_mask)))
    } else {stop("invalid output_mask provided")}
  } else {
    maskOut <- dataIn
  }
  
  constName <- henv$.receive_validVariables()$time_const
  
  # initialize VarVec and FileVec
  allVars <- henv$.load_variables(call$xvars, "all")
  fileVec <- vector(mode="character", length=length(allVars))
  
  # create basevars filenames
  baseFiles_list <- henv$.get_filenames(tm, call$xvars) # excluding lat/lon
  baseVars_ex <- baseFiles_list$variables
  baseFiles_ex <- tools::file_path_sans_ext(baseFiles_list$files)
  baseVars_in <- henv$.load_variables(call$xvars, "base") # including lat/lon  
  for(var in baseVars_in){
    if(!(var == "lat" || var == "lon")){
      short_name <- baseFiles_ex[which(var == baseVars_ex)]
      raster_name <- paste(short_name, dataIn, timeCode, maskOut, sep = "_")
    } else {
      raster_name <- paste("grid", constName, var, call$version,
                           dataIn, timeCode, maskOut, sep = "_")
    }
    raster_name <- paste0(raster_name, ".tif")
    fileVec[which(var == allVars)] = raster_name
  }
  # create powerVars file names
  for(var in call$xvars$powers){
    basePower <- henv$.extract_base_power(var)
    base <- basePower[1]
    power <- as.integer(basePower[2])
    base_name <- fileVec[which(base == allVars)] 
    pattern <- paste0("(?<=_)", base, "(?=_)")
    raster_name <- gsub(pattern, var, base_name, perl = TRUE)
    fileVec[which(var == allVars)] = raster_name
  }
  # create interactionVars file names
  constVars <- henv$.load_variables(call$xvars, "const")
  for(var in call$xvars$interaction){
    vars_sep <- unlist(strsplit(var, ":"))
    v_name <- paste(vars_sep, collapse = "-")
    t_name <- tm
    if(var %in% constVars) t_name = constName
    raster_name <- paste("inter", t_name, v_name, call$version,
                         dataIn, timeCode, maskOut, sep = "_")
    raster_name <- paste0(raster_name, ".tif")
    fileVec[which(var == allVars)] = raster_name
  }
  # return
  if(full_path){
    fileVec <- file.path(path_analysis_norm_predictors, fileVec)
  }
  r_list <- list(
    files = fileVec,
    variables = allVars
  )
  r_list
}


# checks if raster exists in library and creates if not exists
# times with valid entries for time points
# x_files from .get_norm_raster_list()
# tm is valid time interval
#output_mask NULL or valid mask


.check_and_write_norm_rasters <- function(x_files, tm, call, output_mask){
  cat("check and write normalized rasters for time =", tm, "\n")
  henv <- new.env()
  source("scripts/core/1_gjamTime/.gjamTime_Hfunctions.R", local = henv)
  
  allFiles <- x_files$files
  allVars <- x_files$variables #eg. c(tass, prs, elev, lat, lon, lat2, tass3, elev:tass)
  
  # check and write base vars raster
  baseVars <- henv$.load_variables(call$xvars, "base")
  baseFiles <- henv$.get_filenames(tm, call$xvars) # excluding lat/lon
  henv$.check_and_write_norm_param(call, reset=F)
  ref_list <- readRDS("scripts/project/.normalization/.norm_param.rds")
  version <- call$version
  subset <- call$subset
  subset_name <- "alldata"
  if(!isFALSE(subset)){
    subset_name <- tools::file_path_sans_ext(basename(subset$mask))
  }
  time_code <- henv$.binTimeCode(call$times)
  
  for(var in baseVars){
    if(!file.exists(allFiles[which(var == allVars)])){
      if(!(var == "lat" || var == "lon")){
        fname <- baseFiles$files[which(var == baseFiles$variables)]
        r_raw <- rast(file.path(path_gjamTime_in, fname))
      } else {
        mastermask <- rast(file.path(path_masks, name_master_mask))
        r_raw <- mastermask
        if(var == "lat"){
          values(r_raw) <- yFromCell(mastermask, 1:ncell(mastermask))
        }
        if(var == "lon"){
          values(r_raw) <- xFromCell(mastermask, 1:ncell(mastermask))
        }
        r_raw <- mask(r_raw, mastermask, maskvalues=0, updatevalue=NA)
      }
      if(!is.null(output_mask)){
        if(file.exists(file.path(path_masks,output_mask))){
          mask_out <- rast(file.path(path_masks,output_mask))
        } else if (file.exists(output_mask)){
          mask_out <- rast(output_mask)
        } else {stop("invalid output_mask provided")}
        if(ext(r_raw) != ext(mask_out)){r_raw <- crop(r_raw, mask_out)}
        r_raw <- mask(r_raw, mask_out, maskvalues=0, updatevalue=NA)
      } else if(!isFALSE(call$subset)){
        mask_subset <- rast(file.path(path_masks,call$subset$mask))
        if(ext(r_raw) != ext(mask_subset)){r_raw <- crop(r_raw, mask_subset)}
        r_raw <- mask(r_raw, mask_subset, maskvalues=0, updatevalue=NA)
      }
      mu <- ref_list[[version]][[subset_name]][[time_code]][[var]][1]
      sd <- ref_list[[version]][[subset_name]][[time_code]][[var]][2]
      r_norm <- (r_raw-mu)/(3*sd)
      writeRaster(r_norm, allFiles[which(var == allVars)])
    }
  }
  
  ## check and write power and interaction vars based on basevars
  # power vars first
  powerVars <- call$xvars$powers
  for(var in powerVars){
    if(!file.exists(allFiles[which(var == allVars)])){
      basePower <- henv$.extract_base_power(var)
      base <- basePower[1]
      power <- as.integer(basePower[2])
      r_base <- rast(allFiles[which(base == allVars)])
      r_power <- henv$.lengedre_poly(r_base, power)
      writeRaster(r_power, allFiles[which(var == allVars)])
    }
  }
  # interaction vars
  interactVars <- call$xvars$interaction
  for(var in interactVars){
    if(!file.exists(allFiles[which(var == allVars)])){
      vars <- henv$.split_interaction(var)
      r_int <- Reduce(`*`, lapply(vars, function(v) rast(allFiles[which(v == allVars)])))
      writeRaster(r_int, allFiles[which(var == allVars)])
    }
  }
}


#' in path_analysis_
#' times_out NULL, logical or valid times
#' output mask NULL or valid mask
.normalize_predictor_rasters <- function(call, times_out, output_mask){
  cat("normalizing predictor rasters\n")
  times <- .validate_times(call, times_out)
  
  for(tm in times){
    x_files <- .get_norm_raster_list(tm, call, output_mask)
    .check_and_write_norm_rasters(x_files, tm, call, output_mask)
  }
}

# loads rasters from library
## assumes not empr x_files, i.e. assumes non empty predictor variables
# TODO empty predictor rasters
.load_predictor_rasters <- function(call, times_out, output_mask, lyr_names){
  cat("loading raster predictors\n")
  # vector of valid times for output
  times <- .validate_times(call, times_out)
  x_list <- list()
  # create all predictor rasters
  for(tm in times){
    # contains files and vars for this time
    x_files <- .get_norm_raster_list(tm, call, output_mask)
    # check if files exist
    .check_and_write_norm_rasters(x_files, tm, call, output_mask)
    # load
    x_vars <- rast(x_files$files)
    x_1 <- ifel(is.na(x_vars[[1]]), NA, 1)
    x_pred <- c(x_1, x_vars)
    if(length(lyr_names) != nlyr(x_pred)){
      warning("number layers does not match lyr_names")
    }
    names(x_pred) <- lyr_names
    x_list[[tm]] <- x_pred
  }
  x_list
}


################################################################################
## calculate fixed point ####
################################################################################

.fixpt <- function(beta, rho, alpha, x, chunk_process = FALSE,
                   n_chunks = NULL, chunk_size = NULL){
  
  if(chunk_process){
    w_star <- .chunk_process(rasters = list(x = x),
                             FUN = .fixpt,
                             n_chunks = n_chunks,
                             chunk_size = chunk_size,
                             extra_args=list(
                               beta=beta,
                               rho=rho,
                               alpha=alpha
                             ))
    return(w_star)
  }
  
  # TODO actual computation
  
}


# argument can be calling_scrpt, outfolder or name (in scripts/project/.parameters/(name)_call.rds, (name)_output.rds)
.fixpt_geospatial <- function(argument,
                              out_folder = NULL,
                              output_mask = NULL,
                              times_out = NULL,
                              chunk_process = TRUE,
                              n_chunks = NULL,
                              chunk_size = NULL,
                              save = TRUE){
  
  if(FALSE){
    argument = arg
    out_folder = NULL
    output_mask = NULL
    times_out = NULL
    chunk_process = TRUE
    n_chunks = NULL
    chunk_size = NULL
    save = TRUE
  }
  
  # currently only for beta = FALSE, rho = TRUE, alpha = TRUE implemented
  cat("calling .fixpt_geospatial():
      Currently only implemented for beta=FALSE, rho=TRUE, alpha=TRUE, x_raster (predictors)")
  
  call <- .get_argument(argument, "call.rds", where = path_gjamTime_out)
  output <- .get_argument(argument, "output.rdata", where = path_gjamTime_out)
  
  # outfolder
  if(is.null(out_folder)) out_folder <- basename(call$outfolderBase)
  
  # assign parameters
  beta = output$betaMu
  rho = output$rhoMu
  alpha = output$alphaMu
  if(!is.null(beta))beta <- t(beta)
  if(!is.null(rho))rho <- t(rho)
  if(!is.null(alpha))alpha <- t(alpha)
  
  # load_predictor_raster(call)
  x_list <-.load_predictor_rasters(call, times_out, output_mask, lyr_names=colnames(rho))
  
  # calc_fixpt(alpha, rho, x)
  cat("iterating through all ouput rasters\n")
  w_star_list <- list()
  for (entry in names(x_list)){
    cat("calculate fixed point raster for time", entry, "\n")
    w_star <- .fixpt(beta, rho, alpha, x_list[[entry]],
                     chunk_process = chunk_process,
                     n_chunks = n_chunks,
                     chunk_size = chunk_size)
    if(save){
      writeRaster(w_star, file.path(out_folder, paste0("w_star_", entry, ".tif")))
    }
    w_star_list[[entry]] <- w_star
  }
  
  # return
  wstar_list
}