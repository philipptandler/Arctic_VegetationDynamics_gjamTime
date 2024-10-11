#' This Script holds helper functions to process and analyse the outputs of 
#' gjamTime.R

## libraries ####
library(terra)
# library(matlib)


## load output gjamTime ####

load_estimates_gjam <- function(folderPattern, directory=NULL, save=TRUE){
  
  directories <- list.dirs(file.path(directory),
                           recursive = FALSE, full.names = TRUE)
  matching_dirs <- directories[grepl(folderPattern, basename(directories))]
  
  #loads all outfiles independently
  samples_alphaMu <- list()
  samples_alphaSe <- list()
  samples_rhoMu <- list()
  samples_rhoSe <- list()

  # Loop over each matching directory
  for (dir in matching_dirs) {
    # Construct the path to the output.Rdata file
    rdata_path <- file.path(dir, "output.Rdata")
    
    # Check if the file exists
    if (file.exists(rdata_path)) {
      # Load the Rdata file
      load(rdata_path) #loads output_short list
      
      samples_alphaMu[[length(samples_alphaMu)+1]] <- output_short$alphaMu
      samples_alphaSe[[length(samples_alphaSe)+1]] <- output_short$alphaSe
      samples_rhoMu[[length(samples_rhoMu)+1]] <- output_short$rhoMu
      samples_rhoSe[[length(samples_rhoSe)+1]] <- output_short$rhoSe

    } else {
      warning(paste("File not found:", rdata_path))
    }
  }
  
  parameters_alpha <- estimate_parameters(samples_alphaMu, samples_alphaSe)
  parameters_rho <- estimate_parameters(samples_rhoMu, samples_rhoSe)
  
  # Initialize an empty list to store the somevalue outputs
  outputlist <- list(
    alphaMu = parameters_alpha$mean,
    alphaSe = parameters_alpha$sd,
    rhoMu = t(parameters_rho$mean),
    rhoSe = t(parameters_rho$sd)
  )
  
  #save
  if(save){
    for(estimate in names(outputlist)){
      est <- outputlist[[estimate]]
      saveRDS(est, file = file.path(path_analysis_saveParameters, paste0(".",estimate, ".rds")))
    }
    cat("saved estimates under", paste0(path_analysis_saveParameters, "/<estimate>.rds\n"))
  }
  
  # Return the list of somevalue
  return(outputlist)
}


## estimate parameters ####
#' returns estimated mean and sd for X~Norm(mu, sd) according to samples X_i
# mu_list and sd_list as non empty list, same length, identical dimensions of matrix
estimate_parameters <- function(mu_list, sd_list){
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

## normalize raster
normalizeRaster <- function(raster){
  path_norm_list_fullname <- file.path(path_norm_list, name_norm_list)
  norm_list <- readRDS(path_norm_list_fullname)
  for(var in names(raster)){
    raster[[var]] <- (raster[[var]]-norm_list[[var]]$mean)/norm_list[[var]]$sd
  }
  return(raster)
}


## raster linear algebra ####

# get matrix
# raster has nxn layers that represent the coefficients of a matrix filled by row
# returns matrix at i j
get_RasterMatrix <- function(raster, i=1,j=1){
  dim <- as.integer(sqrt(nlyr(raster)))
  values <- values(raster[i,j, drop = F])
  M <- matrix(values, nrow = dim, ncol = dim, byrow = T)
  return(M)
}


# Treats spatial raster M as nXn matrix and w as lenght n vector
# M is interpreted by first filling the rows
# only implemented for 4x4 matrix currently
rasterMatrixProd <- function(M, w){
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

# Matrix M and raster w, each pixel treated as vector by its layers
matrixProd <- function(M, w){
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
    stop("Unknown object M encountered in matrixProd")
  }
  for(row in 1:nrow(M)){
    layer <- vectorProd(M[row,], w)
    layers[[row]] <- layer
  }
  raster <- rast(layers)
  return(raster)
}
# vector v and raster w, where each pixel treated as vector
vectorProd <- function(v,w){
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

## printing memory usage
printMemory <- function(raster, name = ""){
  cat("          inMemory", name, ":", inMemory(raster), "\n")
}



## find nonegative fixedpoints (solve LCP) ####

get_combinations <- function(dim, startWith=NULL){
  if(!is.null(startWith)){
    if(length(startWith) %% dim != 0){
      stop("invalid argument startWith. Doesnt match dimensions of provided wstar")
    }
    firstCombinations <- matrix(startWith, ncol = dim, byrow = TRUE)
    if(!is.logical(firstCombinations)){stop("arguents in startWith must be logical")}
    firstCombinations <- as.data.frame(firstCombinations)
    
  }

  all_combinations <- expand.grid(rep(list(c(TRUE, FALSE)), dim))
  all_combinations <- all_combinations[!apply(all_combinations, 1, all), ]
  all_combinations <- as.data.frame(all_combinations)
  if(!is.null(startWith)){
    names(firstCombinations) <- names(all_combinations)
    all_combinations <- rbind(firstCombinations, all_combinations)
  }
  remaining_combinations <- all_combinations[!duplicated(all_combinations), ]
  rownames(remaining_combinations) <- NULL
  remaining_combinations <- as.matrix(remaining_combinations)
  return(remaining_combinations)
}

WriteAndLoad <- function(raster, name, path = path_analysis_tmprast, datatype = NULL){
  writeRaster(raster, file.path(path, paste0(name, ".tif")),
              overwrite = TRUE, datatype = datatype)
  raster <- rast(file.path(path, paste0(name, ".tif")))
  return(raster)
}

## proposa a solution to LCP
# rho[subs,]*x + alpha[subs, subs]*w = 0, solve for w and add w = 0 for !subs
# combination TRUE means nontrivial fixpt
# combination FALSE means trivial fixpt
propose_LCP <- function(combination, rho, alpha, x_invalid, subset){
  dim <- length(combination)
  # get indices
  subs <- c(1:dim)[combination] # nontrivial solution for subset where combination is TRUE
  subs_zero <- c(1:dim)[!combination]
  
  w_prop_triv <- rast(file.path(path_analysis_data_rast, "zero_layer.tif"))
  if(chunkprossessing){
    w_prop_triv <- w_prop_triv[subset[3]:subset[4], subset[1]:subset[2], drop = F]
    
  }

  if(length(subs) > 0){
    rho_red <- matrix(rho[subs,], nrow = length(subs))
    alpha_red <- matrix(alpha[subs, subs], nrow = length(subs), ncol = length(subs))
    if(length(subs) == 1){
      alpha_inv <- 1/alpha_red
    } else {
      alpha_inv <- inv(alpha_red)
    }
    w_prop_nontriv <- matrixProd(-alpha_inv %*% rho_red, x_invalid)
    w_prop_nontriv <- WriteAndLoad(w_prop_nontriv, "w_prop_nontriv", datatype = "INT2S")
  }
  
  layer_list <- list()
  for(i in 1:dim){
    if(combination[i]){ # if nontrivial solution
      layer_list[[i]] <- w_prop_nontriv[[(which(subs == i))]]
    }else{ # if tirvial solution
      layer_list[[i]] <- w_prop_triv
    }
  }
  w_prop <- rast(layer_list)
  w_prop <- WriteAndLoad(w_prop, "w_prop", datatype = "INT2S")
  
  return(w_prop)
  
}



## for all values in x that are not NA check if LCP is solved
#' returns a raster upd where w >= 0 and rho x + alpha w <= 0
#' 
check_LCP <- function(combination, rho, alpha, x_invalid,
                      useProposedNontriv=TRUE, w=NULL){
  dim <- length(combination)
  subs <- c(1:dim)[combination]
  n_nontriv <- length(subs)
  subs_zero <- c(1:dim)[!combination]
  n_triv <- length(subs_zero)
  
  # if some w are nontrivial, check if w proposed are >= 0
  if(n_nontriv > 0){
    if(useProposedNontriv){
      w_nontriv <- rast(file.path(path_analysis_tmprast, "w_prop_nontriv.tif"))
    }else{
      if(is.null(w)){
        stop("either useProposedNontriv must be TRUE or w must not be NULL")
      }
      w_nontriv <- w[[which(combination)]]
      w_nontriv <- WriteAndLoad(w_nontriv, "w_prop_nontriv", datatype = "INT2S")
    }

    # mask that holds TRUE if all layers >= 0
    mask_wpositive <- (w_nontriv[[1]] >= 0)
    if(n_nontriv >= 2){
      for(layer in 2:n_nontriv){mask_wpositive <- mask_wpositive & (w_nontriv[[layer]] >= 0)}
    }
    mask_wpositive <- WriteAndLoad(mask_wpositive, "mask_wpositive")
  }
  
  # if some w are trivial, check stability for trivial components
  if(n_triv > 0){
    # raster that holds the derivative of each component
    rho_red <- matrix(rho[subs_zero,], nrow = n_triv)
    q_delta <- matrixProd(rho_red, x_invalid)
    if(n_nontriv > 0){
      alpha_red <- matrix(alpha[subs_zero, subs], nrow = n_triv, ncol = n_nontriv)
      alphaw <- matrixProd(alpha_red, w_nontriv)
      alphaw <- WriteAndLoad(alphaw, "alphaw", datatype = "FLT4S")
      q_delta <- q_delta + alphaw
    }
    q_delta <- WriteAndLoad(q_delta, "q_delta", datatype = "FLT4S")
    
    # mask that holds TRUE if rho[subs_zero,]*x+alpha[subs_zero, subs]*w <= 0
    mask_stability <- (q_delta[[1]] <= 0)
    if(n_triv >= 2){
      for(layer in 2:n_triv){mask_stability <- mask_stability & (q_delta[[layer]] <= 0)}
    }
    mask_stability <- WriteAndLoad(mask_stability, "mask_stability")
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
  result <- WriteAndLoad(result, "valid_proposed")
  return(result)
}


# finds a solution to LCP(A,d) where A is alpha, d is rho*x, s.th.
# rho*x + Ay <= 0, y >= 0
solve_LCP <- function(rho, alpha, x, wstar, mask_valid, subset=NULL, startWith=NULL, print=FALSE){
  if(!print){sink(tempfile())}
  dim <- nlyr(wstar)

  # x_invalid holds predictors for solutions yet to be found
  x_invalid <- mask(x, mask_valid, maskvalues=1, updatevalue=NA)
  x_invalid <- WriteAndLoad(x_invalid, "x_invalid", datatype = "FLT4S")
  # w_valid holds wstar which is valid (and unique!)
  w_valid <- mask(wstar, mask_valid, maskvalues=0, updatevalue=NA)
  w_valid <- WriteAndLoad(w_valid, "w_valid", datatype = "INT2S")


  # introduce all possible combinations of x or (b-Ax) = 0 in d/dt(x)
  combinations <- get_combinations(dim, startWith)
  
  cat("solve LCP for combinations:\n")
  # for all combinations
  for(i in 1:nrow(combinations)){
    # get combination
    combination <- combinations[i,]
    cat("     combination",i,":", combination, "\n")
    # propose solution
    cat("     proposing lcp \n")
    w_propose <- propose_LCP(combination, rho, alpha, x_invalid, subset)
    printMemory(w_propose, "w_propose")
    
    # check solution, mask that contains TRUE where we have the solution
    cat("     checking lcp \n")
    valid_proposed <- check_LCP(combination, rho, alpha, x_invalid, useProposedNontriv=TRUE)
    printMemory(valid_proposed, "valid_proposed")
    
    # keep valid solutions
    cat("     updating valid solutions \n")
    w_true <- mask(w_propose, valid_proposed, maskvalues=0, updatevalue=NA)
    w_true <- WriteAndLoad(w_true, "w_true")
    printMemory(w_true, "w_true")
    
    w_valid <- cover(w_valid, w_true)
    w_valid <- WriteAndLoad(w_valid, "w_valid", datatype = "INT2S")
    printMemory(w_valid, "w_valid")
    
    # reduce invalid solutions, where valid_proposed is TRUE, we can make x NA
    # because we have found a solution
    x_invalid <- mask(x_invalid, valid_proposed, maskvalues=1, updatevalue=NA)
    x_invalid <- WriteAndLoad(x_invalid, "x_invalid", datatype = "FLT4S")
    printMemory(x_invalid, "x_invalid")
    
  }
  
  cat("done.:\n")
  if(!print){sink()}
  
  return(w_valid)
}

## calculate Jacobian ####

## euclidian distance in a raster

euclidianDist <- function(raster){
  result <- raster*raster
  result <- sum(result)
  result <- sqrt(result)
  return(result)
}

## Code for 0 solutions

get_wstar_zerocode <- function(w){
  w_nonzero <- (w >= 1)
  codes <- w_nonzero[[1]] * 1 + w_nonzero[[2]] * 2 + w_nonzero[[3]] * 4 + w_nonzero[[4]] * 8
  return(codes)
}
decode_code <- function(code) {
  # Ensure the code is within valid range
  if (code < 0 || code > 15) stop("Code must be between 0 and 15")
  
  # Convert the code to binary and split into a vector of digits
  bin_digits <- as.integer(intToBits(code)[1:4])
  
  # Convert to TRUE/FALSE (1 -> TRUE, 0 -> FALSE)
  return(bin_digits == 1)
}

## create names for jacobian and jacobian inversed

get_namesJ <- function(n){
  
  # Generate names for the Jacobian entries (by row)
  jacobian_names <- as.vector(sapply(1:n, function(i) sapply(1:n, function(j) paste0("J_", i, j))))
  
  # Generate names for the inverse Jacobian entries (by row)
  inverse_jacobian_names <- as.vector(sapply(1:n, function(i) sapply(1:n, function(j) paste0("Jinv_", i, j))))
  
  # Combine the two name sets into one vector
  layer_names <- c(jacobian_names, inverse_jacobian_names)
  return(layer_names)
}

# position function
pos <- function(row, col, which = "J"){
  m <- matrix(c(1:32), byrow = T, ncol = 4)
  if(missing(row)){row <- 1:4}
  if(which == "inv"){
    row <- row+4
  }
  return(as.vector(t(m[row, col])))
}

## get d
#' d is the linearisation at the fixedpoint in the idrection of the trivial dimension
# for wi*(rho*x + sum(aij*wj, j)), the linearisation d/dwi at
# wi = 0 is rho*x + sum(aij*wj, j) =: di

get_di <- function(i, alpha, rho, x, w, nontriv_ind){
  di <- matrixProd(rho[i,], x)
  if(length(nontriv_ind) > 0){
    alphaW <- matrixProd(alpha[i,nontriv_ind], w[[nontriv_ind]])
    di <- di+alphaW
  }
  mask_di_pos <- (di > alpha[i,i])
  di_pos <- mask(di,mask_di_pos, maskvalues = 1, updatevalue = alpha[i,i])
  return(di)
}


## calculate jacobian and jacobian inverse
# returns a spatial raster r which holds the jacobian r[[1:16]] and the 
# jacobian^-1 r[[17:32]] filled by row
# for a given combination of 0 (trivial) and nonzero (nontrivial) solutions in wstar
# this whole function is hardcoded for 4 dimensional w_star
get_jacobian <- function(alpha, rho, w, x, mask, combination){
  # get indices
  nontriv_ind <- c(1:4)[combination]
  triv_ind <- c(1:4)[!combination]
  n_nontriv <- length(nontriv_ind)
  n_triv <- length(triv_ind)
  
  # create raster to fill
  J <- list()
  reclass_matrix <- matrix(c(0, NA,   # map 0 -> NA
                             1, 0),   # map 1 -> 0
                           ncol = 2, byrow = TRUE)

  if(n_triv > 0){
    # create zerolayer
    zero_layer <- classify(mask, reclass_matrix)
    # fill zero lines
    for(i in triv_ind){
      for(j in c(1:4)[-i]){
        J[[pos(i,j,"J")]] <- zero_layer
        J[[pos(i,j,"inv")]] <- zero_layer
      }
      # calculate diagonal elements for trivial solutions
      d_i <- get_di(i, alpha, rho, x, w, nontriv_ind) #rho*x + sum(aij*wj), contros if d == 0, add d <- aii
      J[[pos(i,i,"J")]] <- d_i
      J[[pos(i,i,"inv")]] <- 1/d_i
    }
  }
  
  if(n_nontriv > 0){
    if(n_nontriv >= 2){
      inv_alphaSubs <- inv(alpha[nontriv_ind,nontriv_ind])
    }else{ #i.e. if n_nontriv == 1
      inv_alphaSubs <- as.matrix(1/(alpha[nontriv_ind,nontriv_ind]))
    }
    if(n_triv > 0){
      inv_alpha_box <- inv_alphaSubs %*% alpha[nontriv_ind, triv_ind]
    }
    for(i in nontriv_ind){
      ## jacobian
      for(j in 1:4){
        J[[pos(i,j,"J")]] <- alpha[i,j]*w[[i]]
      }
      ## inv(jacobian)
      # calculate outBlocks [non_tirv, non_triv]
      for(j in nontriv_ind){
        inv_alphaSubs_ij <- inv_alphaSubs[which(nontriv_ind==i), which(nontriv_ind==j)]
        J[[pos(i,j,"inv")]] <- inv_alphaSubs_ij/w[[j]]
      }
      # calculate inBlocks [non_tirv, triv]
      for(j in triv_ind){
        inv_alpha_box_ij <- inv_alpha_box[which(nontriv_ind==i), which(triv_ind==j)]
        J[[pos(i,j,"inv")]] <- -inv_alpha_box_ij*J[[pos(j,j,"inv")]]
      }
    }
  }
  J_rast <- rast(J)
  names(J_rast) <- get_namesJ(4)
  return(J_rast)
}

## mergeAnd Write chunks

mergeAndWrite <- function(name, save = TRUE, datatype = NULL){
  
  f_list <- list.files(path = path_analysis_chunkprocesses,
                     pattern = paste0(name,"*"),
                     full.names = TRUE)
  
  r_list <- lapply(f_list, rast)
  r <- do.call(mosaic, r_list)
  if(save){
    writeRaster(r,
                file.path(path_analysis_data_rast,
                          paste0(name,".tif")),
                overwrite = TRUE,
                datatype = datatype)
  }
  return(r)
}

## Eigenvalues ####

# Function to compute eigenvalues for each cell
compute_eigenvalues <- function(cell_values) {
  nsq <- length(cell_values)
  dim <- as.integer(sqrt(nsq))
  if (any(is.na(cell_values))) {
    return(rep(NA, times=nsq))  # Return 4 NAs if there's any NA in the input
  }
  
  # Reshape the vector of length 16 into a 4x4 matrix
  jacobian <- matrix(cell_values, nrow = dim, ncol = dim)
  
  # Compute the eigenvalues of the matrix
  eigenvals <- Re(eigen(jacobian)$values)
  
  return(eigenvals)
}



## variable definitions ####

# bash calls
sysArgs <- commandArgs(trailingOnly = TRUE)
if(!exists("useScratchifTerminal")){useScratchifTerminal <- FALSE}
if(!exists("useScratch")){useScratch <- FALSE}
if(length(sysArgs) > 0 & useScratchifTerminal){useScratch <- TRUE}

## paths

# set this! select gjam model fit
# gjamModel <- "gjam_singleVars"
gjamModel <- "gjam_interaction"

# paths
path_gjamTime_outputs <- "data/gjamTime_outputs"
path_norm_list <- "Scripts/gjamTime/"
path_ndvi <- "data/NDVI_trends_Ju"


if(gjamModel == "gjam_singleVars"){
  gjam_out_pattern <- "gjam_official_full_subs100_[0-9]{4}"
  path_analysis_saveParameters <- "Scripts/Analysis/.parameters_singleVars"
  path_analysis_data_rast <- "data/analysis_singleVars/rasters"
  path_analysis_tmprast <- "data/analysis_singleVars/tmp_rasters"
  path_analysis_lcpout <- "data/analysis_singleVars/wstar_lcp_out"
  path_analysis_chunkprocesses <- "data/analysis_singleVars/chunk_processes"
} else if(gjamModel == "gjam_interaction"){
  gjam_out_pattern <- "gjam_interaction_full_subs100_[0-9]{4}"
  path_analysis_saveParameters <- "Scripts/Analysis/.parameters_interaction"
  path_analysis_data_rast <- "data/analysis_interaction/rasters"
  path_analysis_tmprast <- "data/analysis_interaction/tmp_rasters"
  path_analysis_lcpout <- "data/analysis_interaction/wstar_lcp_out"
  path_analysis_chunkprocesses <- "data/analysis_interaction/chunk_processes"
} else {stop("please specify gjamModel, i.e. wihch gjam fitted gjam model to use")}

if(useScratch){
  path_analysis_tmprast <- "/cluster/scratch/tandlerp/tmp_rasters"
  path_analysis_chunkprocesses <- "/cluster/scratch/tandlerp/chunk_processes"
  path_analysis_data_rast <- "/cluster/scratch/tandlerp/rasters"
  path_ndvi <- "/cluster/scratch/tandlerp/NDVI"
  
}

## names
name_norm_list <- "normalization.rds"

## constants
X_DIM_RASTER <- 16000
Y_DIM_RASTER <- 13000

