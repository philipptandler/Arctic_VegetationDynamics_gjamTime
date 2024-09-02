# this Script writes the observed and predicted species raster w

#set up environment:
library(here)
setwd(here::here())
source("Scripts/Analysis/analysisHfunctions.R")

#load matrices
alpha <- readRDS(file.path(path_analysis_scripts, ".alphaMu.rds"))
rho <- readRDS(file.path(path_analysis_scripts, ".rhoMu.rds"))
alpha_inv <- readRDS(file.path(path_analysis_scripts, ".alpha_inv.rds"))

# wstar 2020
x_2020 <- rast(file.path(path_analysis_data_rast,"x_2020.tif"))
wstar_2020 <- matrixProd(-alpha_inv %*% rho, x_2020)
names(wstar_2020) <- rownames(rho)
writeRaster(wstar_2020,
            file.path(path_analysis_data_rast, "wstar_2020_2.tif"))
# wstar 2100
x_2100 <- rast(file.path(path_analysis_data_rast,"x_2100.tif"))
wstar_2100 <- matrixProd(-alpha_inv %*% rho, x_2100)
names(wstar_2100) <- rownames(rho)
writeRaster(wstar_2100,
            file.path(path_analysis_data_rast, "wstar_2100.tif"))


## difference ####
wobs_2020 <- rast(file.path(path_analysis_data_rast, "wobs_2020.tif"))
wdiff_2020 <- wobs_2020 - wstar_2020
writeRaster(wdiff_2020,
            file.path(path_analysis_data_rast, "wdiff_2020.tif"))

