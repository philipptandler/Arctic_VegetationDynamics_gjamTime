#  this Script calculates the Jacobian at the fixed point for each pixel
# !!! This script is for simplicity hardcoded for 4 dimensions (sh, cf, hb, lc)

#set up environment:
library(here)
setwd(here::here())
useScratchifTerminal <- TRUE
source("Scripts/Analysis/analysisHfunctions.R")

## General SETUP ####
time <- 1
chunk_size <- 10
chunkprossessing <- TRUE

## system Arguments
if(length(sysArgs) > 0){time <- as.integer(sysArgs[1])}
if(time < 0 || time > 3){stop("invalid argument: time")}
if(length(sysArgs) > 1){
  chunk_size <- as.integer(sysArgs[2])
  chunkprossessing = TRUE
}
if(chunk_size <= 1){stop("invalid argument: chunk_size")}
n_chunks <- ceiling(X_DIM_RASTER/chunk_size)

# periods
periods_list <- list(
  "1" = "1990",
  "2" = "2020",
  "3" = "2100"
)
period_char <- periods_list[[time]]
name_jacobian <-  paste0("jacobian_", period_char)
name_jacobianInv <-  paste0("jacobianInv_", period_char)


# load matrices
alpha <- readRDS(file.path(path_analysis_saveParameters, ".alphaMu.rds"))
alpha_inv <- inv(alpha)
rho <- readRDS(file.path(path_analysis_saveParameters, ".rhoMu.rds"))

# load rasters
x_time <- rast(file.path(path_analysis_data_rast,
                         paste0("x_", period_char,".tif")))
w_star <- rast(file.path(path_analysis_data_rast,
                         paste0("wstar_lcpSolved_", period_char,".tif")))
# make wstar_zero which contains a code {0,..,15} which layers are 0
wstar_zerocode <- get_wstar_zerocode(w_star) 
wstar_zerocode <- WriteAndLoad(wstar_zerocode, paste0("wstarLCP_zerocode_",period_char),
                               path_analysis_data_rast, datatype = "INT1S")


## solve the Linear Complimentary Problem for w in chunks ####
cat("Calculating Jacobian for", period_char, ":\n")

for(pos in 1:n_chunks){
  cat("prcess", pos, "of", n_chunks, "chunks: ")
  # subset boundaries
  xmin <- (pos-1)*chunk_size+1
  xmax <- min(pos*chunk_size, X_DIM_RASTER)
  ymin <- 1
  ymax <- Y_DIM_RASTER
  # do subset
  x_time_subs <- x_time[ymin:ymax, xmin:xmax, drop = F]
  w_star_subs <- w_star[ymin:ymax, xmin:xmax, drop = F]
  wstar_zerocode_subs <- wstar_zerocode[ymin:ymax, xmin:xmax, drop = F]
  
  cat("zerocode: ")
  jacobian_list <- list()
  jacobianInv_list - list()
  for(zero_code in 0:15){
    cat(zero_code, ", ")
    combination <- decode_code(zero_code)
    mask_thisComb <- (wstar_zerocode_subs == zero_code)
    x_time_subs_thisComb <- mask(x_time_subs, mask_thisComb, maskvalues=0, updatevalue=NA)
    w_star_subs_thisComb <- mask(w_star_subs, mask_thisComb, maskvalues=0, updatevalue=NA)
    
    # calculate jacobian and its inverse for this combination
    jacobian_subs_thiscomb <- get_jacobian(alpha = alpha,
                                           alpha_inv = alpha_inv,
                                           rho = rho,
                                           w = w_star_subs_thisComb,
                                           x = x_time_subs_thisComb,
                                           mask = mask_thisComb,
                                           combination = combination)
    jacobian_subs_thiscomb <- WriteAndLoad(jacobian_subs_thiscomb, paste0("jacobian_subs_thiscomb_", zero_code))
    jacobian_list[[zero_code]] <- jacobian_subs_thiscomb[[1:16]]
    jacobianInv_list[[zero_code]] <- jacobian_subs_thiscomb[[17:32]]
  }
  # merge all jacobians and save temporarily in chunk processes
  jacobian <- do.call(mosaic, jacobian_list)
  jacobianInv <- do.call(mosaic, jacobianInv_list)
  writeRaster(jacobian,
              file.path(path_analysis_chunkprocesses,
                        paste0(name_jacobian, "-", pos,".tif")),
              datatype = "FLT4S")
  writeRaster(jacobianInv,
              file.path(path_analysis_chunkprocesses,
                        paste0(name_jacobianInv, "-", pos,".tif")),
              datatype = "FLT4S")
  cat(" merged.\n")
  
}

delta_chunks_list <- list.files(path = path_analysis_chunkprocesses,
                                pattern = "delta_zero_*",
                                full.names = TRUE)
rasters <- lapply(delta_chunks_list, rast)

merged_raster <- do.call(mosaic, rasters)
writeRaster(merged_raster,
            file.path(path_analysis_data_rast,
                      "delta_zero.tif"),
            datatype = "INT2S")



cat(" => solved jacobian for wstar chunks", period_char,"\n\n\n\n\n\n")

## merge all files produced ####
cat("merging chunks \n")
wstar_chunks_list <- list.files(path = path_analysis_lcpout,
                                pattern = paste0(name_jacobian,"*"),
                                full.names = TRUE)
rasters <- lapply(wstar_chunks_list, rast)

merged_raster <- do.call(mosaic, rasters)
writeRaster(merged_raster,
            file.path(path_analysis_data_rast,
                      paste0(name_jacobian,".tif")),
            datatype = "INT2S")

cat(" => solved LCP for wstar", period_char,"\n\n\n\n\n\n")

