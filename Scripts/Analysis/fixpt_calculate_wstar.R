# this Script writes the observed and predicted species raster w

#set up environment:
library(here)
setwd(here::here())
source("Scripts/Analysis/analysisHfunctions.R")

#load matrices
alpha <- readRDS(file.path(path_analysis_scripts, ".alphaMu.rds"))
rho <- readRDS(file.path(path_analysis_scripts, ".rhoMu.rds"))
alpha_inv <- readRDS(file.path(path_analysis_scripts, ".alpha_inv.rds"))

alpha_inv %*% rho
x_2020 <- rast(file.path(path_analysis_data_rast, "x_2020.tif"))


