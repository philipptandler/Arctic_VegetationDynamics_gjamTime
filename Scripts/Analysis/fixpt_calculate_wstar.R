# this Script writes the observed and predicted species raster w

#set up environment:
library(here)
setwd(here::here())
source("Scripts/Analysis/analysisHfunctions.R")

#load matrices
alpha <- readRDS(file.path(path_analysis_scripts, ".alphaMu.rds"))
rho <- readRDS(file.path(path_analysis_scripts, ".rhoMu.rds"))
alpha_inv <- readRDS(file.path(path_analysis_scripts, ".alpha_inv.rds"))

x_2020 <- rast(file.path(path_analysis_data_rast,"x_2020.tif"))
wstar_2020 <- matrixProd(-alpha_inv %*% rho, x_2020)
names(wstar_2020) <- rownames(rho)
writeRaster(wstar_2020,
            file.path(path_analysis_data_rast, "wstar_2020.tif"))



