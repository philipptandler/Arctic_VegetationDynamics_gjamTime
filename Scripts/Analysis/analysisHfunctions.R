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
  path_norm_list_fullname <- file.path(path_norm_list, norm_list_name)
  norm_list <- readRDS(path_norm_list_fullname)
  for(var in names(raster)){
    raster[[var]] <- (raster[[var]]-norm_list[[var]]$mean)/norm_list[[var]]$sd
  }
  return(raster)
}


## raster linear algebra ####

# Matrix M and raster w, each pixel treated as vector
matrixProd <- function(M, w){
  layers <- list()
  cat("calculate layer:")
  for (row in 1:nrow(M)){
    cat(" ", row)
    layer <- vectorProd(M[row,], w)
    layers[[row]] <- layer
  }
  cat("\n")
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

## make solution of w_subset to full solution again
make_fulldim <- function(w, subset, subset_zero){
  dim_red <- nlyr(w)
  
}


#' for all values in x that are not NA check if LCP is solved
#' returns a raster upd where w >= 0 and rho x + alpha w <= 0
check_LCP <- function(dim, x, mask_valid, combination){
  subset <- c(1:dim)[combination]
  subset_zero <- c(1:dim)[!combination]
  
  M <- -inv(alpha[subset, subset]) %*% rho[subset,]
  w_upd <- matrixProd(M, x)
  w_upd <- make_fulldim(w_upd, subset, subset_zero)
  mask_upd <- (w_upd[[1]] >= 0 &
                                        w_upd[[2]] >= 0 &
                                        w_upd[[3]] >= 0 &
                                        w_upd[[4]] >= 0)
  
}


# finds a solution to LCP(A,d) where A is alpha, d is rho*x, s.th.
# rho*x + Ay <= 0, y >= 0
solve_LCP <- function(rho, alpha, x, wstar, mask_valid){
  dim <- nlyr(wstar)
  # x_invalid holds predictors for solutions yet to be found
  x_invalid <- mask(x, mask_valid, maskvalues=1, updatevalue=NA)
  # w_valid holds wstar which is valid (and unique!)
  w_valid <- mask(wstar, mask_valid, maskvalues=0, updatevalue=NA)  

  # introduce all possible combinations of x or (b-Ax) = 0 in d/dt(x)
  combinations <- as.matrix(expand.grid(rep(list(c(TRUE, FALSE)), dim)))

  # for all combinations
  for(i in 2:(dim**2)){
    # get combination
    
    # propose solution
    w_propose <- propose_LCP(dim, rho, alpha, x_invalid)
    # check solution
    valid_proposed <- check_LCP(alpha, rho, x_invalid)
    # keep valid solutions
    w_true <- mask(w_propose, valid_proposed, maskvalue=0, updatevalue=NA)
    w_valid <- cover(w_valid, w_true)
    # reduce invalid solutions
    x_invalid <- mask(x_invalid, valid_proposed, maskvalue=1, updatevalue=NA)
    
    
    
    
    
    combination <- combinations[i,]
    # check solution for remaining pixels where we have no valid solution yet
    # subset x where mask_valid is still false
    x_false <- mask(x, mask_valid, maskvalues=1, updatevalue=NA)
    writeRaster(x_false, file.path(path_analysis_tmprast), "x_false.tif", overwrite=T)
    # search solutions
    mask_upd <- check_LCP(dim, x_false, mask_valid, combination)
    mask_valid <- mask_valid | mask_upd
    writeRaster(mask_valid, file.path(path_analysis_tmprast), "mask_valid.tif", overwrite=T)
    # update valid solutions
    w_upd <- rast(file.path(path_analysis_tmprast), "w_upd.tif")
    w_valid <- cover(w_valid, w_upd)
    
  }

}


## variable definitions ####
## paths
path_gjamTime_outputs <- "data/gjamTime_outputs"
path_analysis_scripts <- "Scripts/Analysis"
path_analysis_data_rast <- "data/analysis/rasters"
path_analysis_tmprast <- "data/analysis/tmp_rasters"

path_norm_list <- "Scripts/gjamTime/"
norm_list_name <- "normalization.rds"

## constants


