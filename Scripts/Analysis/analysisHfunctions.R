#' This Script holds helper functions to process and analyse the outputs of 
#' gjamTime.R

## libraries ####
library(terra)
library(matlib)


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
      saveRDS(est, file = file.path(path_analysis_scripts, paste0(".",estimate, ".rds")))
    }
    cat("saved estimates under", paste0(path_analysis_scripts, "/<estimate>.rds\n"))
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
propose_LCP <- function(combination, rho, alpha, x_invalid){
  dim <- length(combination)
  # get indices
  subs <- c(1:dim)[combination] # nontrivial solution for subset where combination is TRUE
  subs_zero <- c(1:dim)[!combination]
  
  w_prop_triv <- rast(file.path(path_analysis_data_rast, "zero_layer.tif"))

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
solve_LCP <- function(rho, alpha, x, wstar, mask_valid, startWith=NULL){
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
    w_propose <- propose_LCP(combination, rho, alpha, x_invalid)
    # check solution, mask that contains TRUE where we have the solution
    cat("     checking lcp \n")
    valid_proposed <- check_LCP(combination, rho, alpha, x_invalid, useProposedNontriv=TRUE)
    # keep valid solutions
    cat("     updating valid solutions \n")
    w_true <- mask(w_propose, valid_proposed, maskvalue=0, updatevalue=NA)
    w_true <- WriteAndLoad(w_true, "w_true")
    w_valid <- cover(w_valid, w_true)
    w_valid <- WriteAndLoad(w_valid, "w_valid", datatype = "INT2S")
    # reduce invalid solutions, where valid_proposed is TRUE, we can make x NA
    # because we have found a solution
    x_invalid <- mask(x_invalid, valid_proposed, maskvalue=1, updatevalue=NA)
    x_invalid <- WriteAndLoad(x_invalid, "x_invalid", datatype = "FLT4S")
  }
  cat("done.:\n")
  return(w_valid)
}


## variable definitions ####
# bash calls
sysArgs <- commandArgs(trailingOnly = TRUE)

## paths
path_gjamTime_outputs <- "data/gjamTime_outputs"
path_analysis_scripts <- "Scripts/Analysis"
path_analysis_data_rast <- "data/analysis/rasters"
path_analysis_tmprast <- "data/analysis/tmp_rasters"
path_analysis_lcpout <- "data/analysis/wstar_lcp_out"
path_norm_list <- "Scripts/gjamTime/"

## names
name_norm_list <- "normalization.rds"

## constants
X_DIM_RASTER <- 16000
Y_DIM_RASTER <- 13000

