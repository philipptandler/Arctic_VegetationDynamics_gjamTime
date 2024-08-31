# this Script writes the observed and predicted species raster w

#set up environment:
library(here)
setwd(here::here())
source("Scripts/Analysis/analysisHfunctions.R")

#load matrices
alpha <- readRDS(file.path(path_analysis, ".alphaMu.rds"))
rho <- readRDS(file.path(path_analysis, ".rhoMu.rds"))
#inverse alpha
alpha_inv <- inv(alpha)



