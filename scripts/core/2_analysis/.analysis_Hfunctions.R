################################################################################
## libraries ####
################################################################################

library(terra)
library(matlib)

source("scripts/core/2_analysis/.chunk_process.R")

################################################################################
## timing ####
################################################################################

.start_time <- function(){
  start_time <- Sys.time()
  return(start_time)
}
.end_time <- function(time){
  delta <- format(round(Sys.time() - time, 2))
  return(delta)
}

################################################################################
## retrieve parameters ####
################################################################################

# arg can be calling script, output or name in scripts/project/.parameters/(name)_output.rdata 
.get_argument <- function(arg, type, where = path_analysis){
  if(type != "call.rds" && type != "output.rdata" && type != "dir"){
    stop("invalid argument type:", type)
  }
  if(file.exists(arg) && !dir.exists(arg)){
    helper_env <- new.env()
    source("scripts/core/1_gjamTime/.gjamTime_Hfunctions.R", local = helper_env)
    call <- helper_env$.initialize_and_validate_call(arg)
    if(!file.exists(file.path("scripts/project/.parameters/",
                              paste(call$name, type, sep="_")))){
      call <- helper_env$.prepare_gjamTime_outfolder(call, arg, create.if.notFound = F)
      if(type == "call.rds") return(call)
      if(type == "output.rdata") return(.load_output_Rdata(call$outfolderBase))
      if(type == "dir") return(call$outfolderBase)
    }
    arg = call$name
  }
  if(file.exists(file.path("scripts/project/.parameters/",
                           paste(basename(arg), type, sep="_")))){
    if(type == "call.rds") return(readRDS(file.path("scripts/project/.parameters/",
                                                    paste(arg, type, sep="_"))))
    if(type == "output.rdata") return(.load_output_Rdata("scripts/project/.parameters/", prename=paste0(arg, "_")))
    if(type == "dir") stop("argument", arg, "requires type=='call.rds' or 'output.rdata'")
  }
  if(dir.exists(arg) && dirname(arg) == path_gjamTime_out){
    if(type == "call.rds") return(readRDS(file.path(arg,
                                                    type)))
    if(type == "output.rdata") return(.load_output_Rdata(arg))
    if(type == "dir") return(arg)
  }
  if(dir.exists(file.path(where, arg)) &&
     (type == "dir" || (type != "dir" && file.exists(file.path(where, arg, type))))){
    if(type == "call.rds") return(readRDS(file.path(where, arg, type)))
    if(type == "output.rdata") return(.load_output_Rdata(file.path(where, arg)))
    if(type == "dir") return(file.path(where, arg))
  }
  stop("Not found:", arg)
}

# loads function in local env
.load_output_Rdata <- function(output_path, prename=NULL){
  output_name <- file.path(output_path, paste0(prename, "output.rdata"))
  load(output_name)
  if(exists("output_save")){
    return(output_save)
  }
  if(exists("output_summary")){
    return(output_summary)
  }
}

#' returns name of the outfolder in data/gjamtime/out/somename, or provided
#' outfolder (in path_analysis) or as specified
.parse_outfolder <- function(call, out_folder){
  
  if(is.null(out_folder)){
    out_folder <- file.path(path_analysis,basename(call$outfolderBase))
  } else {
    if(!(dir.exists(out_folder) || dir.exists(dirname(out_folder)) ||
       dir.exists(dirname(dirname(out_folder))))){
      out_folder <- file.path(path_analysis, basename(out_folder))
    }
  }
  if(!dir.exists(out_folder)) dir.create(out_folder, recursive = TRUE,
                                         showWarnings = FALSE)
  
  out_folder
}

################################################################################
## create and retrieve predictor rasters ####
################################################################################
# Function to check if a string can be converted to a logical vector
.is_valid_logical_string <- function(input_str) {
  if(!is.character(input_str) || length(input_str) != 1) return(FALSE)
  elements <- strsplit(input_str, ",")[[1]]
  elements <- trimws(toupper(elements))  # Remove spaces and convert to uppercase
  
  all(elements %in% c("T", "F"))
}

# Function to convert a valid string to a logical vector
.convert_to_logical_vector <- function(input_str) {
  elements <- strsplit(input_str, ",")[[1]]
  elements <- trimws(toupper(elements))  # Remove spaces and convert to uppercase
  as.logical(elements == "T")
}


.validate_times <- function(call, times_out){
  if(.is_valid_logical_string(times_out)){
    times_out <- .convert_to_logical_vector(times_out)
  }
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
  if(!isFALSE(call$xvars$powers)){
    for(var in call$xvars$powers){
      basePower <- henv$.extract_base_power(var)
      base <- basePower[1]
      power <- as.integer(basePower[2])
      base_name <- fileVec[which(base == allVars)] 
      pattern <- paste0("(?<=_)", base, "(?=_)")
      raster_name <- gsub(pattern, var, base_name, perl = TRUE)
      fileVec[which(var == allVars)] = raster_name
    }
  }

  # create interactionVars file names
  if(!isFALSE(call$xvars$interaction)){
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
      # crop extent
      r_raw <- .crop_output_ext(r_raw, call, output_mask)
      mu <- ref_list[[version]][[subset_name]][[time_code]][[var]][1]
      sd <- ref_list[[version]][[subset_name]][[time_code]][[var]][2]
      # noramlize
      r_norm <- (r_raw-mu)/(3*sd)
      writeRaster(r_norm, allFiles[which(var == allVars)])
    }
  }
  
  ## check and write power and interaction vars based on basevars
  # power vars first
  powerVars <- call$xvars$powers
  if(!isFALSE(powerVars)){
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
  }
  # interaction vars
  interactVars <- call$xvars$interaction
  if(!isFALSE(interactVars)){
    for(var in interactVars){
      if(!file.exists(allFiles[which(var == allVars)])){
        vars <- henv$.split_interaction(var)
        r_int <- Reduce(`*`, lapply(vars, function(v) rast(allFiles[which(v == allVars)])))
        writeRaster(r_int, allFiles[which(var == allVars)])
      }
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
## raster operations ####
################################################################################

# crops raster to output_mask or call$subset
.crop_output_ext <- function(raster, call, output_mask){
  if(!is.null(output_mask)){
    if(file.exists(file.path(path_masks,output_mask))){
      mask_out <- rast(file.path(path_masks,output_mask))
    } else if (file.exists(output_mask)){
      mask_out <- rast(output_mask)
    } else {stop("invalid output_mask provided")}
    if(ext(raster) != ext(mask_out)){raster <- crop(raster, mask_out)}
    raster <- mask(raster, mask_out, maskvalues=0, updatevalue=NA)
  } else if(!isFALSE(call$subset)){
    mask_subset <- rast(file.path(path_masks,call$subset$mask))
    if(ext(raster) != ext(mask_subset)){raster <- crop(raster, mask_subset)}
    raster <- mask(raster, mask_subset, maskvalues=0, updatevalue=NA)
  }
  raster
}




# Matrix M and raster w, each pixel treated as vector by its layers
.matrixProd <- function(M, w){
  if (is.null(M) ||
      (is.matrix(M) && nrow(M) == 0) ||
      (is.vector(M) && length(M) == 0)
  ) {
    return(NULL)  # Return NULL if M is of dimension 0
  }
  layers <- list()
  if(is.vector(M)){
    M <- matrix(M, nrow = 1)
  }
  if(!is.matrix(M)){
    cat("typeof(M):\n")
    cat(typeof(M),"\n")
    stop("Unknown object M encountered in .matrixProd")
  }
  for(row in 1:nrow(M)){
    layer <- .vectorProd(M[row,], w)
    layers[[row]] <- layer
  }
  raster <- rast(layers)
  return(raster)
}
# vector v and raster w, where each pixel treated as vector
.vectorProd <- function(v,w){
  n_layers <- nlyr(w) # ==length of vector!
  if(n_layers != length(v)){stop("number of vector elements doesnt match number of layers")}
  result <- v[1] * w[[1]]
  if(n_layers == 1){
    return(result)
  }else{
    for(i in 2:n_layers){
      result <- result + v[i]*w[[i]]
    }
    return(result)
  }
}

# Treats spatial raster M as nXn matrix and w as lenght n vector
# M is interpreted by first filling the rows
# only implemented for 4x4 matrix currently
# TODO currently implemented only for dim = 4
.rasterMatrixProd <- function(M, w){
  if(nlyr(M) != 4**2){stop("dimensions do not match in matrix product")}
  result_list <- list()
  for(i in 1:4){
    layer <- rast(nrows = nrow(w), 
                  ncols = ncol(w), 
                  nlyrs = 1, 
                  crs = crs(w), 
                  ext = ext(w),
                  vals = 0)
    for(j in 1:4){
      layer <- layer + M[[pos(i, j)]]*w[[j]]
    }
    result_list[[i]] <- layer
  }
  result <- rast(result_list)
  names(result) <- names(w)
  return(result)
}


################################################################################
## calculate fixed point ####
################################################################################

# all possible combinations to try for solving LCP
# (except all layer TRUE, since this is the nontrivial solution)
.get_combinations <- function(dim, startWith=NULL){
  combin <- expand.grid(rep(list(c(TRUE, FALSE)), dim))
  combin <- combin[!apply(combin, 1, all), ]
  combin <- as.data.frame(combin)
  rownames(combin) <- NULL
  combin <- as.matrix(combin)
  combin
}

# propos a a solution to LCP
# rho[subs,]*x + alpha[subs, subs]*w = 0, solve for w and add w = 0 for !subs
# combination TRUE means nontrivial fixpt
# combination FALSE means trivial fixpt
.propose_LCP <- function(combination, rho, alpha, x_invalid){
  dim <- length(combination)
  # get indices
  subs <- c(1:dim)[combination] # nontrivial solution for subset where combination is TRUE
  subs_zero <- c(1:dim)[!combination]
  
  w_prop_triv <- rast(x_invalid[[1]])
  values(w_prop_triv) <- 0

  if(length(subs) > 0){
    rho_red <- matrix(rho[subs,], nrow = length(subs))
    alpha_red <- matrix(alpha[subs, subs], nrow = length(subs), ncol = length(subs))
    if(length(subs) == 1){
      alpha_inv <- 1/alpha_red
    } else {
      alpha_inv <- inv(alpha_red)
    }
    w_prop_nontriv <- .matrixProd(-alpha_inv %*% rho_red, x_invalid)
  }
  
  layer_list <- list()
  for(i in 1:dim){
    if(combination[i]){ # if nontrivial solution for this layer
      layer_list[[i]] <- w_prop_nontriv[[(which(subs == i))]]
    }else{ # if trivial solution for this layer
      layer_list[[i]] <- w_prop_triv
    }
  }
  w_prop <- rast(layer_list)
  
  w_prop
}

## for all values in x that are not NA check if LCP is solved
#' returns a raster upd where w >= 0 and rho x + alpha w <= 0
.check_LCP <- function(combination, rho, alpha, x_invalid, w){
  dim <- length(combination)
  subs <- c(1:dim)[combination]
  n_nontriv <- length(subs)
  subs_zero <- c(1:dim)[!combination]
  n_triv <- length(subs_zero)
  
  # if some w are nontrivial, check if w proposed are >= 0
  if(n_nontriv > 0){
    w_nontriv <- w[[which(combination)]]
    # mask that holds TRUE if all layers >= 0
    mask_wpositive <- if (nlyr(w_nontriv) > 1) {
      app(w_nontriv, function(x) all(x >= 0))
    } else {
      w_nontriv >= 0
    }
  }
  
  # if some w are trivial, check stability for trivial components
  if(n_triv > 0){
    # raster that holds the derivative of each component
    rho_red <- matrix(rho[subs_zero,], nrow = n_triv)
    q_delta <- .matrixProd(rho_red, x_invalid)
    if(n_nontriv > 0){
      alpha_red <- matrix(alpha[subs_zero, subs], nrow = n_triv, ncol = n_nontriv)
      alphaw <- .matrixProd(alpha_red, w_nontriv)
      q_delta <- q_delta + alphaw
    }
    # mask that holds TRUE if rho[subs_zero,]*x+alpha[subs_zero, subs]*w <= 0
    mask_stability <- if (nlyr(q_delta) > 1) {
      app(q_delta, function(x) all(x <= 0))
    } else {
      q_delta <= 0
    }
  }
  
  # check if Linear Complimentary Problem conditions are satisfied
  if(n_nontriv > 0 & n_triv > 0){
    result <- mask_wpositive & mask_stability
  }
  if(n_triv == 0){
    result <- mask_wpositive
  }
  if(n_nontriv == 0){
    result <- mask_stability
  }
  result
}

# finds a solution to LCP(A,d) where A is alpha, d is rho*x, s.th.
# rho*x + Ay <= 0, y >= 0
.solve_LCP <- function(rho, alpha, x, wstar, mask_valid, print=FALSE){
  if(!print){sink(tempfile())}
  dim <- nlyr(wstar)
  
  # x_invalid holds predictors for solutions yet to be found
  x_invalid <- mask(x, mask_valid, maskvalues=1, updatevalue=NA)

  # w_valid holds wstar which is valid (and unique! see Takeuchi & Adachi, 1980)
  w_valid <- mask(wstar, mask_valid, maskvalues=0, updatevalue=NA)
  
  # introduce all possible combinations of x or (b-Ax) = 0 in d/dt(x)
  combinations <- .get_combinations(dim)
  
  # for all combinations
  cat("probing LCP solutions (out of", nrow(combinations), "):")
  for(i in 1:nrow(combinations)){
    cat(".")
    # get combination
    combination <- combinations[i,]
    # propose solution
    w_propose <- .propose_LCP(combination, rho, alpha, x_invalid)

    # check solution, mask that contains TRUE where we have the solution
    valid_proposed <- .check_LCP(combination, rho, alpha, x_invalid, w = w_propose)

    # keep valid solutions
    w_true <- mask(w_propose, valid_proposed, maskvalues=0, updatevalue=NA)
    w_valid <- cover(w_valid, w_true)

    # reduce invalid solutions, where valid_proposed is TRUE, we can make 
    # the raster x to NA, since we have found a solution
    x_invalid <- mask(x_invalid, valid_proposed, maskvalues=1, updatevalue=NA)
  }
  
  if(!print){sink()}
  
  w_valid
}



## finds the fixed point for equation beta*x + rho*x*w + diag(w)alpha*w = 0
.fixpt <- function(beta, rho, alpha, x, chunk_process = FALSE,
                   n_chunks = NULL, chunk_size = NULL, nontriv = FALSE){
  if(chunk_process){
    w_star <- .chunk_process(rasters = list(x = x),
                             FUN = .fixpt,
                             n_chunks = n_chunks,
                             chunk_size = chunk_size,
                             extra_args=list(
                               beta=beta,
                               rho=rho,
                               alpha=alpha,
                               chunk_process=FALSE
                             ))
    return(w_star)
  }
  # .fixpt() is actually running
  cat("run .fixpt() ")
  
  # raster with solution w*= -Inv(alpha)*rho*x
  wstar_nontriv <- .matrixProd(-inv(alpha) %*% rho, x) 
  names(wstar_nontriv) <- rownames(rho)
  
  if(nontriv){
    cat("done.\n")
    return(wstar_nontriv)
  }
  
  # raster with TRUE where nontriv solution is valid
  mask_wstar_nonneg <- if (nlyr(wstar_nontriv) > 1) {
    app(wstar_nontriv, function(x) all(x >= 0))
  } else {
    wstar_nontriv >= 0
  }
  
  # solve linear complimentary problem with auxilary vector d
  # w* * d = 0 (fixed point)
  # w* >= 0 (only positive solutions)
  # d = rho*x + alpha*w <= 0 (stability)
  
  wstar_lcpsolved <- .solve_LCP(rho, alpha, x,
                                wstar = wstar_nontriv, 
                                mask_valid = mask_wstar_nonneg,
                                print = TRUE)
  cat("done.\n")
  return(wstar_lcpsolved)
}


# argument can be calling_scrpt, outfolder or name (in scripts/project/.parameters/(name)_call.rds, (name)_output.rds)
.fixpt_geospatial <- function(argument,
                              out_folder = NULL,
                              output_mask = NULL,
                              times_out = NULL,
                              chunk_process = TRUE,
                              n_chunks = 100,
                              chunk_size = NULL,
                              save = TRUE,
                              data_type = NULL){

  # currently only for beta = FALSE, rho = TRUE, alpha = TRUE implemented
  cat("calling .fixpt_geospatial():
      Currently only implemented for beta=FALSE, rho=TRUE, alpha=TRUE, x_raster (predictors)")
  
  call <- .get_argument(argument, "call.rds", where = path_gjamTime_out)
  output <- .get_argument(argument, "output.rdata", where = path_gjamTime_out)
  
  # outfolder
  out_folder <- .parse_outfolder(call, out_folder)

  # assign parameters
  beta = output$betaMu
  rho = output$rhoMu
  alpha = output$alphaMu
  if(!is.null(beta))beta <- t(beta)
  if(!is.null(rho))rho <- t(rho)
  if(!is.null(alpha))alpha <- alpha
  
  # load_predictor_raster(call)
  x_list <-.load_predictor_rasters(call, times_out, output_mask, lyr_names=colnames(rho))
  
  # calc_fixpt(alpha, rho, x)
  cat("iterating through all ouput rasters\n")
  w_star_list <- list()
  mastermask <- rast(file.path(path_masks, name_master_mask))
  if(length(x_list) > 0){
    mastermask <- crop(mastermask, x_list[[1]])
  }
  # for all times
  for (entry in names(x_list)){
    cat("calculate fixed point raster for time", entry, "\n")
    w_star <- .fixpt(beta, rho, alpha, x_list[[entry]],
                     chunk_process = chunk_process,
                     n_chunks = n_chunks,
                     chunk_size = chunk_size)
    w_star <- mask(w_star, mastermask, maskvalues=0, updatevalue=NA)
    if(save){
      valid_datatypes <- c("INT1U", "INT2U", "INT2S", "INT4U", "INT4S", "FLT4S", "FLT8S")
      if(is.null(data_type) || !(data_type %in% valid_datatypes)){
        data_type <- datatype(w_star[[1]])
      } 
      writeRaster(w_star, file.path(out_folder, paste0("w_star_", entry, ".tif")),
                  datatype = data_type)
    }
    w_star_list[[entry]] <- w_star
    cat("\n")
  }
  
  # return
  w_star_list
}


################################################################################
## calculate jacobian matrix ####
################################################################################

# binary code what layers are == 0
.get_wstar_zerocode <- function(w){
  w_nonzero <- (w >= 1)
  codes <- Reduce(`+`, lapply(c(1:nlyr(w_nonzero)), function(x) w_nonzero[[x]]*(2**(x-1))))
  return(codes)
}
.decode_zerocode <- function(code, dim) {
  # Convert the code to binary and split into a vector of digits
  bin_digits <- as.integer(intToBits(code)[1:dim])
  # Convert to TRUE/FALSE (1 -> TRUE, 0 -> FALSE)
  return(bin_digits == 1)
}

# get d
#' d is the linearisation at the fixed point in the direction of the trivial dimension
# for wi*(rho*x + sum(aij*wj, j)), the linearisation d/dwi at
# wi = 0 is rho*x + sum(aij*wj, j) =: di
.get_di <- function(i, alpha, rho, x, w, nontriv_ind){
  di <- .matrixProd(rho[i,], x)
  if(length(nontriv_ind) > 0){
    alphaW <- .matrixProd(alpha[i,nontriv_ind], w[[nontriv_ind]])
    di <- di+alphaW
  }
  # di must be <= 0 (as a consequence of the linear complementry problem)
  # therefore we assert this by overwriting with a minimal negative value
  mask_di_pos <- (di > alpha[i,i])
  di <- mask(di,mask_di_pos, maskvalues = 1, updatevalue = alpha[i,i])
  return(di)
}

# create names for jacobian and jacobian inversed
.get_namesJ <- function(n, name){
  
  # Generate names for the Jacobian entries (by row)
  jacobian_names <- as.vector(sapply(1:n, function(i) sapply(1:n, function(j) paste0(name, "_[", i, ",", j, "]"))))
  
  jacobian_names
}

## calculate jacobian and jacobian inverse
# returns a spatial raster r which holds the jacobian and the 
# jacobian^-1 filled by row
# for a given combination of 0 (trivial) and nonzero (nontrivial) solutions in wstar
.get_jacobian <- function(rho, alpha, x, w, mask, combination, regular, inverse){
  
  dim <- nlyr(w)
  
  pos <- function(row, col){
    m <- matrix(c(1:dim**2), byrow = T, ncol = dim)
    if(missing(row)){row <- 1:dim}
    return(as.vector(t(m[row, col])))
  }
  
  
  # get indices
  nontriv_ind <- c(1:dim)[combination]
  triv_ind <- c(1:dim)[!combination]
  n_nontriv <- length(nontriv_ind)
  n_triv <- length(triv_ind)
  
  # create raster to fill
  if(regular){Jr <- list() }# regular jacobian
  if(inverse){Ji <- list() }# inverse jacobian
  reclass_matrix <- matrix(c(0, NA,   # map 0 -> NA
                             1, 0),   # map 1 -> 0
                           ncol = 2, byrow = TRUE)
  
  if(n_triv > 0){
    # create zerolayer
    zero_layer <- classify(mask, reclass_matrix)
    # fill zero lines
    for(i in triv_ind){
      for(j in c(1:dim)[-i]){
        if(regular) Jr[[pos(i,j)]] <- zero_layer
        if(inverse) Ji[[pos(i,j)]] <- zero_layer
      }
      # calculate diagonal elements for trivial solutions
      d_i <- .get_di(i, alpha, rho, x, w, nontriv_ind) #rho*x + sum(aij*wj), contros if d == 0, add d <- aii
      if(regular) Jr[[pos(i,i)]] <- d_i
      if(inverse) Ji[[pos(i,i)]] <- 1/d_i
    }
  }
  
  if(n_nontriv > 0){
    if(inverse){
      if(n_nontriv >= 2){
        inv_alphaSubs <- inv(alpha[nontriv_ind, nontriv_ind])
      }else{ #i.e. if n_nontriv == 1
        inv_alphaSubs <- as.matrix(1/(alpha[nontriv_ind, nontriv_ind]))
      }
      if(n_triv > 0){
        inv_alpha_box <- inv_alphaSubs %*% alpha[nontriv_ind, triv_ind]
      }
    }
    for(i in nontriv_ind){
      ## jacobian
      if(regular){
        for(j in 1:dim){
          Jr[[pos(i,j)]] <- alpha[i,j]*w[[i]]
        }
      }
      ## inv(jacobian)
      if(inverse){
        # calculate outBlocks [non_tirv, non_triv]
        for(j in nontriv_ind){
          inv_alphaSubs_ij <- inv_alphaSubs[which(nontriv_ind==i), which(nontriv_ind==j)]
          Ji[[pos(i,j)]] <- inv_alphaSubs_ij/w[[j]]
        }
        # calculate inBlocks [non_tirv, triv]
        for(j in triv_ind){
          inv_alpha_box_ij <- inv_alpha_box[which(nontriv_ind==i), which(triv_ind==j)]
          Ji[[pos(i,j)]] <- -inv_alpha_box_ij*Ji[[pos(j,j)]]
        }
      }
    }
  }
  J_regular <- NULL
  J_inverse <- NULL
  if(regular){
    J_regular <- rast(Jr)
    names(J_regular) <- .get_namesJ(dim, "J")
  }
  if(inverse){
    J_inverse <- rast(Ji)
    names(J_inverse) <- .get_namesJ(dim, "Jinv")
  } 
  J_rast <- list(
    regular = J_regular,
    inverse = J_inverse
  )
  return(J_rast)
}


# returns jacobian matrix by row as layers of the raster
# first jacobian regular, second inverse if specified
.jacobian <- function (rho, alpha, x, w_star, chunk_process = FALSE,
                       n_chunks = NULL, chunk_size = NULL,
                       regular = TRUE, inverse = FALSE){
  if(chunk_process){
    jacobian <- .chunk_process(rasters=list(
                                 x=x,
                                 w_star = w_star),
                               FUN = .jacobian,
                               n_chunks = n_chunks,
                               chunk_size = chunk_size,
                               extra_args=list(
                                 rho=rho,
                                 alpha=alpha,
                                 chunk_process=FALSE,
                                 regular=regular,
                                 inverse=inverse
                               ))
    return(jacobian)
  }
  
  # raster storing which combination of layers is zero
  wstar_zerocode <- .get_wstar_zerocode(w_star)
  
  if(regular) jacobian_list <- list()
  if(inverse) jacobianInv_list <- list()
  
  dim <- nlyr(w_star)
  # for all combinations of 0 for w_star (since w_i == 0 simplifies the calculation)
  for(zero_code in 0:(dim**2-1)){
    # select and mask only for given combination
    combination <- .decode_zerocode(zero_code, dim)
    mask_comb <- (wstar_zerocode == zero_code)
    x_comb <- mask(x, mask_comb, maskvalues=0, updatevalue=NA)
    w_star_comb <- mask(w_star, mask_comb, maskvalues=0, updatevalue=NA)
    
    # calculate jacobian and its inverse for this combination
    jacobian_comb <- .get_jacobian(rho = rho,
                                   alpha = alpha,
                                   x = x_comb,
                                   w = w_star_comb,
                                   mask = mask_comb,
                                   combination = combination,
                                   regular = regular,
                                   inverse = inverse)

    # jacobian_subs_thiscomb <- WriteAndLoad(jacobian_subs_thiscomb, paste0("jacobian_subs_thiscomb_", zero_code))
    if(regular) jacobian_list[[zero_code+1]] <- jacobian_comb$regular
    if(inverse) jacobianInv_list[[zero_code+1]] <- jacobian_comb$inverse
  }
  jac_reg <- NULL
  jac_inv <- NULL
  if(regular) jac_reg <- do.call(mosaic, jacobian_list)
  if(inverse) jac_inv <- do.call(mosaic, jacobianInv_list)
  
  jacobian <- c(jac_reg, jac_inv)
  
  return(jacobian)
}



#' argument is folder name in analysis, folder name in gjamTime/out, name of 
#' call and output in scripts/project/.parameters
.jacobian_geospatial <- function(argument,
                                 fixed_point_files = NULL,
                                 out_folder = NULL,
                                 output_mask = NULL,
                                 times_out = NULL,
                                 regular = TRUE,
                                 inverse = FALSE,
                                 chunk_process = TRUE,
                                 n_chunks = 100,
                                 chunk_size = NULL,
                                 save = TRUE){
  
  call <- .get_argument(argument, "call.rds", where = path_gjamTime_out)
  output <- .get_argument(argument, "output.rdata", where = path_gjamTime_out)
  
  # outfolder
  out_folder <- .parse_outfolder(call, out_folder)
  
  # assign parameters
  beta = output$betaMu
  rho = output$rhoMu
  alpha = output$alphaMu
  if(!is.null(beta))beta <- t(beta)
  if(!is.null(rho))rho <- t(rho)
  if(!is.null(alpha))alpha <- alpha
  
  # load_predictor_raster(call)
  x_list <-.load_predictor_rasters(call, times_out, output_mask, lyr_names=colnames(rho))
  
  cat("iterating through all fixed points\n")
  jacobian_list <- list()
  
  # for all times
  n_iter <- length(x_list)
  i <- 1
  for (entry in names(x_list)){
    cat("calculate jacobian for time", entry, ",(", i, "/", n_iter, ")","\n")
    if(!is.null(fixed_point_files)){
      w_star <- rast(fixed_point_files[i])
    } else {
      w_star <- rast(file.path(out_folder, paste0("w_star_", entry, ".tif")))
    }
    dim <- nlyr(w_star)
    jacobian <- .jacobian(rho = rho, 
                          alpha = alpha, 
                          x = x_list[[entry]], 
                          w_star = w_star,
                          chunk_process = chunk_process,
                          n_chunks = n_chunks,
                          chunk_size = chunk_size,
                          regular = regular,
                          inverse = inverse)
    
    if(regular){
      jac_reg <- jacobian[[1:dim**2]]
      jacobian_list$regular[[entry]] <- jac_reg
      if(save){
        writeRaster(jac_reg, file.path(out_folder, paste0("jacobian_", entry, ".tif")),
                    overwrite = TRUE)
        cat("saved", paste0("jacobian_", entry, ".tif"), "in", 
            out_folder, "\n")
      }
    }
    if(inverse){
      from <- 1
      to <- dim**2
      if(regular){
        from <- from + dim**2
        to <- to + dim**2
      }
      jac_inv <- jacobian[[from:to]]
      jacobian_list$inverse[[entry]] <- jac_inv
      if(save){
        writeRaster(jac_inv, file.path(out_folder, paste0("jacobianInv_", entry, ".tif")),
                    overwrite = TRUE)
        cat("saved", paste0("jacobianInv_", entry, ".tif"), "in", 
            out_folder, "\n")
      }
    }
    cat("\n")
    i = i+1
  }
  # return
  jacobian_list
}


################################################################################
## write observed rasters for response (given in yvars) ####
################################################################################

# returns list of observed rasters, writes them as w_obs
.wobs_geospatial <- function(argument,
                             out_folder = NULL,
                             output_mask = NULL,
                             times_out = NULL,
                             save = TRUE){
  
  call <- .get_argument(argument, "call.rds", where = path_gjamTime_out)

  # out_folder
  out_folder <- .parse_outfolder(call, out_folder)
  
  cat("loading all observed rasters\n")
  times_out <- .validate_times(call, times_out)
  w_obs_list <- list()
  for(tm in times_out){
    # local env
    henv <- new.env()
    source("scripts/core/1_gjamTime/.gjamTime_Hfunctions.R", local = henv)
    # load files
    files_list <- henv$.get_filenames(tm, call$yvars)
    files <- files_list$files
    vars <- files_list$variables
    file_paths_in <- file.path(path_gjamTime_in, files)
    
    # load raster
    r_raw <- rast(file_paths_in)
    names(r_raw) <- vars
    
    # crop with out_mask or subset of call
    r_raw <- .crop_output_ext(r_raw, call, output_mask)
    
    if(save){
      # write
      writeRaster(r_raw, file.path(out_folder, paste0("w_obs_", tm, ".tif")))
    }
    # write entry
    w_obs_list[[tm]] <- r_raw
  }
  w_obs_list
}


################################################################################
## write change of rasters ####
################################################################################

# calls regression for chunks
.cell_regression <- function(raster, chunk_process = FALSE,
                             n_chunks = NULL, chunk_size = NULL,
                             outfolder = NULL, outname = NULL,
                             datatype = NULL){
  if(chunk_process){
    lmraster <- .chunk_process(rasters=list(
                                 raster=raster),
                               FUN = .cell_regression,
                               n_chunks = n_chunks,
                               chunk_size = chunk_size,
                               extra_args=list(
                                 chunk_process=FALSE,
                                 outfolder=outfolder,
                                 outname=outname,
                                 datatype=datatype)
                               )
    return(lmraster)
  }
  
  
  lm_rast <- regress(y = raster,
                     x = c(1:nlyr(raster)),
                     formula = y~x,
                     na.rm = TRUE,
                     filename = file.path(outfolder, outname),
                     overwrite = TRUE,
                     datatype = datatype)
  
  lm_rast
}

## returning w as mean, difference, mean_difference, linear model
.wrate_geospatial <- function(files,
                              outfolder = NULL,
                              mean = FALSE,
                              rate = TRUE,
                              mean_rate = TRUE,
                              linear_model = FALSE,
                              chunk_process = FALSE,
                              n_chunks = 100,
                              chunk_size = NULL,
                              save = TRUE,
                              datatype = NULL){
  
  n_files <- length(files) # number of files
  n_diff <- n_files-1 # number of differences
  
  # prepare
  result <- list()
  if(is.null(outfolder)){
    outfolder <- dirname(files[1])
  } else {
    if(!dir.exists(outfolder)){stop("invalid outfolder specified")}
  }
  
  # check if length is long enough
  if(n_files <= 0) return(NULL)
  
  cat("processing files:\n")
  for(i in 1:n_files){
    cat(i, ":", files[i], "\n")
  }
  
  # mean of files
  if(mean){
    rasters <- list()
    for(i in 1:n_files){
      rasters[[i]] <- rast(files[i])
    }
    sum <- Reduce(`+`, lapply(c(1:n_files), function(v) rasters[[v]]))
    res <- sum/n_files
    result$mean <- res
    if(save){
      fname <- "w_mean.tif"
      writeRaster(mean, file.path(outfolder, fname),
                  overwrite = TRUE,
                  datatype = datatype)
      cat("saved", fname, "in", outfolder, "\n")
    }
  }

  # check if length is long enough
  if(n_diff <= 0) return(NULL)
  
  # rate of change
  if(rate){
    for (i in 1:n_diff){
      name = paste0("diff_", i)
      r1 <- rast(files[i])
      r2 <- rast(files[i+1])
      diff <- r2 - r1
      if(save){
        fname <- paste0("w_rate_diff_", i, ".tif")
        writeRaster(diff, file.path(outfolder, fname),
                    overwrite = TRUE,
                    datatype = datatype)
        cat("saved", fname, "in", outfolder, "\n")
      }
      result[[name]] <- diff
    }
    # average change
    if(mean_rate){
      sum_changes <- Reduce(`+`, lapply(c(1:n_diff), function(v) result[[v]]))
      res <- sum_changes/n_diff
      result$mean_change <- res
      if(save){
        fname <- "w_rate_diff_mean.tif"
        writeRaster(res, file.path(outfolder, fname),
                    overwrite = TRUE,
                    datatype = datatype)
        cat("saved", fname, "in", outfolder, "\n")
      }
    }
  }

  # linear model of change
  if(linear_model){
    ref_raster <- rast(files[1])
    
    intercepts <- list()
    slopes <- list()
    
    n_lyr <- nlyr(ref_raster)
    lyrnames <- names(ref_raster)
    # check for unique layer names
    if(length(unique(lyrnames)) < n_lyr){
      warning("non-unique layer names enountered.\nConsider renaming your raster layers given in files.\n")
      lyrnames <- paste0("var", c(1:n_lyr))
    }
    
    # linear model for each variable in the predictor raster
    for(lyr in 1:n_lyr){
      cat("linear model for layer:", lyrnames[lyr], "\n")
      r_lyr <- list() # collect individual rasters for a given layer
      for(tm in 1:n_files){
        r_lyr[[tm]] <- rast(files[tm])[[lyr]]
      }
      rstack <- rast(r_lyr)
      outname <- paste0("w_rate_lm-bySpec_", lyrnames[lyr], ".tif" )
      lm_rast <- .cell_regression(rstack, chunk_process = chunk_process,
                                  n_chunks = n_chunks, chunk_size = chunk_size,
                                  outfolder = outfolder, outname = outname,
                                  datatype = datatype)

      cat("saved", outname, "in", outfolder, "\n\n")
      
      intercepts[[lyrnames[lyr]]] <- lm_rast[[1]]
      slopes[[lyrnames[lyr]]] <- lm_rast[[2]]
    }
    # save all interacepts
    result$intercept <- rast(intercepts)
    writeRaster(result$intercept,
                file.path(outfolder, "w_rate_lm_intercept.tif"),
                overwrite = TRUE,
                datatype = datatype)
    cat("saved w_rate_lm_intercept.tif in", outfolder, "\n")
    
    # save all slopes
    result$slope <- rast(slopes)
    writeRaster(result$slope,
                file.path(outfolder, "w_rate_lm_slope.tif"),
                overwrite = TRUE,
                datatype = datatype)
    cat("saved w_rate_lm_slope.tif in", outfolder, "\n")
  }
  result
}
