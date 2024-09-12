# this Script writes the observed and predicted species raster w

#set up environment:
library(here)
setwd(here::here())
source("Scripts/Analysis/analysisHfunctions.R")

## General SETUP ####
time <- 1
chunk_size <- 160
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
nameChunkconst <-  paste0("wstar_lcpSolved_", period_char, "_", chunk_size)

# load matrices
alpha <- readRDS(file.path(path_analysis_scripts, ".alphaMu.rds"))
rho <- readRDS(file.path(path_analysis_scripts, ".rhoMu.rds"))

# load rasters
x_time <- rast(file.path(path_analysis_data_rast,
                         paste0("x_", period_char,".tif")
                         ))
w_star <- rast(file.path(path_analysis_data_rast,
                         paste0("wstar_", period_char,"_nontriv.tif")
                         ))
mask_wstar_nonneg <- rast(file.path(path_analysis_data_rast,
                                    paste0("mask_wstar_", period_char,"_nonneg.tif")
                                    ))

# first combinations
startWith_list <- list(
  "1990" = c(c(T,T,T,F), c(T,T,F,F)),
  "2020" = c(c(T,F,T,T), c(T,F,T,F), c(T,T,T,F)),
  "2100" = c(F,T,T,T)
)

## solve the Linear Complimentary Problem for w in chunks ####
cat("Solving LCP for wstar", period_char, ":\n")

for(pos in 1:n_chunks){
  cat("processing", pos, "out of", n_chunks, "chunks.\n")
  # subset boundaries
  xmin <- (pos-1)*chunk_size+1
  xmax <- min(pos*chunk_size, X_DIM_RASTER)
  ymin <- 1
  ymax <- Y_DIM_RASTER
  subset <- c(xmin, xmax, ymin, ymax)
  # do subset
  x_time_subs <- x_time[subset[3]:subset[4], subset[1]:subset[2], drop = F]
  w_star_subs <- w_star[subset[3]:subset[4], subset[1]:subset[2], drop = F]
  mask_wstar_nonneg_subs <- mask_wstar_nonneg[subset[3]:subset[4], subset[1]:subset[2], drop = F]
  #solve LCP
  wstar_lcpsolved <- solve_LCP(rho, alpha, x = x_time_subs,
                               wstar = w_star_subs, mask_valid = mask_wstar_nonneg_subs,
                               subset = subset,
                               startWith=startWith_list[[time]],
                               print=FALSE)
  writeRaster(wstar_lcpsolved,
              file.path(path_analysis_lcpout,
                        paste0(nameChunkconst, "-", pos,".tif")),
              datatype = "INT2S")
  
}
cat(" => solved LCP for wstar chunks", period_char,"\n\n\n\n\n\n")

## merge all files produced ####
cat("merging chunks \n")
wstar_chunks_list <- list.files(path = path_analysis_lcpout,
			       	pattern = paste0(nameChunkconst,"*"),
				full.names = TRUE)
rasters <- lapply(wstar_chunks_list, rast)

merged_raster <- do.call(mosaic, rasters)
writeRaster(merged_raster,
	    file.path(path_analysis_data_rast,
		      paste0(nameChunkconst,".tif")),
	    datatype = "INT2S")

cat(" => solved LCP for wstar", period_char,"\n\n\n\n\n\n")

