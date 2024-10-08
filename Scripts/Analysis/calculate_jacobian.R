#  this Script calculates the Jacobian at the fixed point for each pixel
# !!! This script is for simplicity hardcoded for 4 dimensions (sh, cf, hb, lc)

#set up environment:
library(here)
setwd(here::here())
useScratchifTerminal <- TRUE 
#' if useScratchifTerminal is TRUE, it uses a SCRATCH directory (very large) to
#' save the indermediant and final results. Designed for EULER Cluster at ETH Zürich

source("Scripts/Analysis/analysisHfunctions.R")

## General SETUP ####
time <- 1
chunk_size <- 80
chunkprossessing <- TRUE

## system Arguments
if(length(sysArgs) > 0){time <- as.integer(sysArgs[1])}
if(time < 0 || time > 2){stop("invalid argument: time")}
if(length(sysArgs) > 1){
  chunk_size <- as.integer(sysArgs[2])
  chunkprossessing = TRUE
}
if(chunk_size < 1){stop("invalid argument: chunk_size")}
n_chunks <- ceiling(X_DIM_RASTER/chunk_size)

# periods
periods_list <- list(
  "1" = "1990",
  "2" = "2020",
  "3" = "2100"
)
period_char <- periods_list[[time]]
name_jacobian <-paste0("jacobian_", period_char)
name_jacobianInv <- paste0("jacobianInv_", period_char)
if(time == 1 | time == 2){
  name_lag_star <- paste0("w_lag_",period_char, "_star")
  name_lag_norm_star <- paste0("w_lagnorm_",period_char, "_star")
}
if(time == 1){
  name_lag_obs <- paste0("w_lag_",period_char, "_obs")
  name_lag_norm_obs <- paste0("w_lagnorm_",period_char, "_obs")
}

# load matrices
alpha <- readRDS(file.path(path_analysis_saveParameters, ".alphaMu.rds"))
rho <- readRDS(file.path(path_analysis_saveParameters, ".rhoMu.rds"))

# load rasters
x_time <- rast(file.path(path_analysis_data_rast,
                         paste0("x_", period_char,".tif")))
w_star <- rast(file.path(path_analysis_data_rast,
                         paste0("wstar_lcpSolved_", period_char,".tif")))
# make wstar_zero which contains a code {0,..,15} which layers are nonzero
wstar_zerocode <- get_wstar_zerocode(w_star)
wstar_zerocode <- WriteAndLoad(wstar_zerocode, paste0("wstarLCP_zerocode_",period_char),
                               path_analysis_data_rast, datatype = "INT1S")
# wstar_zerocode <- rast(file.path(path_analysis_data_rast,
#                                  paste0("wstarLCP_zerocode_",period_char,".tif")))
if(time == 1 | time == 2){
  w_rate <- rast(file.path(path_analysis_data_rast,
                           paste0("wrate_",period_char,".tif")))
}
if(time == 1){
  w_rate_obs <- rast(file.path(path_analysis_data_rast,
                           paste0("wrate_1990_obs.tif")))
}

## solve the Linear Complimentary Problem for w in chunks ####
cat("Calculating Jacobian for", period_char, ":\n")

for(chunk in 1:n_chunks){
  cat("process", chunk, "of", n_chunks, "chunks: ")
  # subset boundaries
  xmin <- (chunk-1)*chunk_size+1
  xmax <- min(chunk*chunk_size, X_DIM_RASTER)
  ymin <- 1
  ymax <- Y_DIM_RASTER
  # ## to test
  # xmin <- 4623
  # xmax <- 4683
  # ymin <- 7001+chunk*100
  # ymax <- 7100+chunk*100
  # do subset
  x_time_subs <- x_time[ymin:ymax, xmin:xmax, drop = F]
  w_star_subs <- w_star[ymin:ymax, xmin:xmax, drop = F]
  wstar_zerocode_subs <- wstar_zerocode[ymin:ymax, xmin:xmax, drop = F]
  if(time == 1 | time == 2){
    w_rate_subs <- w_rate[ymin:ymax, xmin:xmax, drop = F]
  }
  if(time == 1){
    w_rate_obs_subs <- w_rate_obs[ymin:ymax, xmin:xmax, drop = F]
  }
  
  cat("calculating jacobians")
  jacobian_list <- list()
  jacobianInv_list <- list()
  for(zero_code in 0:15){
    cat(".")
    combination <- decode_code(zero_code)
    mask_thisComb <- (wstar_zerocode_subs == zero_code)
    x_time_subs_thisComb <- mask(x_time_subs, mask_thisComb, maskvalues=0, updatevalue=NA)
    w_star_subs_thisComb <- mask(w_star_subs, mask_thisComb, maskvalues=0, updatevalue=NA)
    
    # calculate jacobian and its inverse for this combination
    jacobian_subs_thiscomb <- get_jacobian(alpha = alpha,
                                           rho = rho,
                                           w = w_star_subs_thisComb,
                                           x = x_time_subs_thisComb,
                                           mask = mask_thisComb,
                                           combination = combination)
    # jacobian_subs_thiscomb <- WriteAndLoad(jacobian_subs_thiscomb, paste0("jacobian_subs_thiscomb_", zero_code))
    jacobian_list[[zero_code+1]] <- jacobian_subs_thiscomb[[1:16]]
    jacobianInv_list[[zero_code+1]] <- jacobian_subs_thiscomb[[17:32]]
  }
  ## merge all jacobians and save temporarily in chunk processes directory
  #jacobian
  jacobian_subs <- do.call(mosaic, jacobian_list)
  writeRaster(jacobian_subs,
              file.path(path_analysis_chunkprocesses,
                        paste0(name_jacobian, "-", chunk,".tif")),
              overwrite = TRUE,
              datatype = "FLT4S")
  #jacobianInv
  jacobianInv_subs <- do.call(mosaic, jacobianInv_list)
  writeRaster(jacobianInv_subs,
              file.path(path_analysis_chunkprocesses,
                        paste0(name_jacobianInv, "-", chunk,".tif")),
              overwrite = TRUE,
              datatype = "FLT4S")
  cat("done. ")
  
  ## find lagging rate ##
  if(time == 1 | time == 2){
    cat("Find lagging... ")
    # vector
    w_laggin_subs <- rasterMatrixProd(jacobianInv_subs, w_rate_subs)
    writeRaster(w_laggin_subs,
                file.path(path_analysis_chunkprocesses,
                          paste0(name_lag_star, "-", chunk,".tif")),
                overwrite = TRUE,
                datatype = "INT4S")
    # euclidian
    w_laggin_euc_subs <- euclidianDist(w_laggin_subs)
    writeRaster(w_laggin_euc_subs,
                file.path(path_analysis_chunkprocesses,
                          paste0(name_lag_norm_star, "-", chunk,".tif")),
                overwrite = TRUE,
                datatype = "INT4S")
  }
  if(time == 1){
    # vector
    w_laggin_obs_subs <- rasterMatrixProd(jacobianInv_subs, w_rate_obs_subs)
    writeRaster(w_laggin_obs_subs,
                file.path(path_analysis_chunkprocesses,
                          paste0(name_lag_obs,"-", chunk,".tif")),
                overwrite = TRUE,
                datatype = "INT4S")
    #euclidian
    w_laggin_obs_euc_subs <- euclidianDist(w_laggin_obs_subs)
    writeRaster(w_laggin_obs_euc_subs,
                file.path(path_analysis_chunkprocesses,
                          paste0(name_lag_norm_obs,"-", chunk,".tif")),
                overwrite = TRUE,
                datatype = "INT4S")
    
  }
  cat("done.\n")
}

## merge all files produced ####
## merge and write Jacobian
cat("merging chunks Jacobian \n")
jacobian <- mergeAndWrite(name_jacobian, save = TRUE, datatype = "FLT4S")
jacobianInv <- mergeAndWrite(name_jacobianInv, save = TRUE, datatype = "FLT4S")

cat(" => merged Jacobian and inv(Jacobian) for wstar", period_char,":))\n\n\n")

## merge and write lagging wstar
if(time == 1 | time == 2){
  cat("merging chunks w_lag \n")
  w_lag_star <- mergeAndWrite(name_lag_star, save = TRUE, datatype = "INT4S")
  w_lag_star_norm <- mergeAndWrite(name_lag_norm_star, save = TRUE, datatype = "INT4S")
}

## merge and write lagging observed
if(time == 1){
  w_lag_obs <- mergeAndWrite(name_lag_obs, save = TRUE, datatype = "INT4S")
  w_lag_obs_norm <- mergeAndWrite(name_lag_norm_obs, save = TRUE, datatype = "INT4S")
}
cat(" => merged w_lag for", period_char,":))\n\n\n\n\n\n")

