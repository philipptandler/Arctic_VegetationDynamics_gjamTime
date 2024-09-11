# this Script writes the observed and predicted species raster w

#set up environment:
library(here)
setwd(here::here())
source("Scripts/Analysis/analysisHfunctions.R")

#load matrices
alpha <- readRDS(file.path(path_analysis_scripts, ".alphaMu.rds"))
rho <- readRDS(file.path(path_analysis_scripts, ".rhoMu.rds"))

#load rasters
x_1990 <- rast(file.path(path_analysis_data_rast,"x_1990.tif"))
wstar_1990 <- rast(file.path(path_analysis_data_rast, "wstar_1990_nontriv.tif"))
mask_wstar1990_nonneg <- rast(file.path(path_analysis_data_rast, "mask_wstar_1990_nonneg.tif"))

x_2020 <- rast(file.path(path_analysis_data_rast,"x_2020.tif"))
wstar_2020 <- rast(file.path(path_analysis_data_rast, "wstar_2020_nontriv.tif"))
mask_wstar2020_nonneg <- rast(file.path(path_analysis_data_rast, "mask_wstar_2020_nonneg.tif"))

x_2100 <- rast(file.path(path_analysis_data_rast,"x_2100.tif"))
wstar_2100 <- rast(file.path(path_analysis_data_rast, "wstar_2100_nontriv.tif"))
mask_wstar2100_nonneg <- rast(file.path(path_analysis_data_rast, "mask_wstar_2100_nonneg.tif"))


## solve the Linear Complimentary Problem for w ####
#' finding the nonnegative stable solution

wstar_1990_noneg <- solve_LCP(rho, alpha, d = x_1990,
                              wstar = wstar_1990, mask = mask_wstar_1990_nonneg) 
wstar_2020_noneg <- solve_LCP(rho, alpha, d = x_2020,
                              wstar = wstar_2020, mask = mask_wstar_2020_nonneg,
                              startWith=c(c(T,F,T,T), c(T,F,T,F), c(T,T,T,F))) 
wstar_2100_noneg <- solve_LCP(rho, alpha, d = x_2100,
                              wstar = wstar_2100, mask = mask_wstar_2100_nonneg) 
