# this Script writes the observed and predicted species raster w

#set up environment:
library(here)
setwd(here::here())
source("Scripts/Analysis/analysisHfunctions.R")

#load matrices
alpha <- readRDS(file.path(path_analysis_scripts, ".alphaMu.rds"))
rho <- readRDS(file.path(path_analysis_scripts, ".rhoMu.rds"))



## solve the Linear Complimentary Problem for w ####
#' finding the nonnegative stable solution

# wstar_2020_noneg <- solve_LCP(d = x_2020, wstar = wstar_2020, mask = mask_wstar_2020_nonneg) 
