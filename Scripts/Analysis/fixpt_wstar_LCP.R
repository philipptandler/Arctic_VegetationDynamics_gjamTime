# this Script writes the observed and predicted species raster w

#set up environment:
library(here)
setwd(here::here())
source("Scripts/Analysis/analysisHfunctions.R")

## system Arguments
time <- 1
if(length(sysArgs) > 0){time <- as.integer(sysArgs[1])}
if(time < 0 || time > 3){stop("invalid argument time")}

n_chunks <- 1000
if(length(sysArgs) > 1){n_chunks <- as.integer(sysArgs[2])}
if(n_chunks < 0){stop("invalid argument n_chunks")}

chunks_ID <- 1
if(length(sysArgs) > 2){chunks_ID <- as.integer(sysArgs[3])}
if(chunks_ID < 1 || chunks_ID > n_chunks){stop("invalid argument chunks_ID")}


# periods
periods_list <- list(
  "1" = "1990",
  "2" = "2020",
  "3" = "2100"
)
period_char <- periods_list[[time]]

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

# subset rasters

# first combinations
startWith_list <- list(
  "1990" = c(c(T,T,T,F), c(T,T,F,F)),
  "2020" = c(c(T,F,T,T), c(T,F,T,F), c(T,T,T,F)),
  "2100" = c(F,T,T,T)
)

## solve the Linear Complimentary Problem for w ###

## solve the Linear Complimentary Problem for w ####
#' finding the nonnegative stable solution
chunkprossessing <- FALSE
# crop_tile <- list(
#   x = c(7000:7099),
#   y = c(7000:7099),
#   drop = FALSE
# )
# 
# xtest <- x_1990[crop_tile$x, crop_tile$y, drop = crop_tile$drop]
# wstartest <- wstar_1990[crop_tile$x, crop_tile$y, drop = crop_tile$drop]
# mask_validtest <- mask_wstar1990_nonneg[crop_tile$x, crop_tile$y, drop = crop_tile$drop]
# 
# cat("Running test:\n ")
# wstar_1990_noneg <- solve_LCP(rho, alpha, x = xtest,
#                               wstar = wstartest, mask_valid = mask_validtest,
#                               startWith=c(c(T,T,T,F), c(T,T,F,F)))
# writeRaster(wstar_1990_noneg,
#             file.path(path_analysis_data_rast, "wstar_lcpsolved_1990.tif"),
#             datatype = "INT2S")

cat("Running 1990:\n ")
wstar_1990_noneg <- solve_LCP(rho, alpha, x = x_1990,
                              wstar = wstar_1990, mask_valid = mask_wstar1990_nonneg,
                              startWith=c(c(T,T,T,F), c(T,T,F,F)))
writeRaster(wstar_1990_noneg,
            file.path(path_analysis_data_rast, "wstar_lcpsolved_1990.tif"),
            datatype = "INT2S")

cat("\n\n\n\n\n Running 2020:\n ")
wstar_2020_noneg <- solve_LCP(rho, alpha, x = x_2020,
                              wstar = wstar_2020, mask_valid = mask_wstar2020_nonneg,
                              startWith=c(c(T,F,T,T), c(T,F,T,F), c(T,T,T,F)))
writeRaster(wstar_2020_noneg,
            file.path(path_analysis_data_rast, "wstar_lcpsolved_2020.tif"),
            datatype = "INT2S")

cat("\n\n\n\n\n Running 2100:\n ")
wstar_2100_noneg <- solve_LCP(rho, alpha, x = x_2100,
                              wstar = wstar_2100, mask_valid = mask_wstar2100_nonneg,
                              startWith=c(F,T,T,T)) 
writeRaster(wstar_2100_noneg,
            file.path(path_analysis_data_rast, "wstar_lcpsolved_2100.tif"),
            datatype = "INT2S")
cat(" => solved LCP for wstar", period_char,"\n\n\n\n\n\n")

