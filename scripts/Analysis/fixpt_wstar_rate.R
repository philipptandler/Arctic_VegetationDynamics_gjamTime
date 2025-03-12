#  this Script calculates the rate of change of the fixedpoint w_rate
# for future wstar2100 - wstar2020 and past wstar2020-wstar1990

#set up environment:
library(here)
setwd(here::here())
source("Scripts/Analysis/analysisHfunctions.R")



## w_rates, rate of change in the ficedpoint per 6yr period
wstar_1990 <- rast("data/analysis_interaction/rasters/wstar_lcpSolved_1990.tif")
wstar_2020 <- rast("data/analysis_interaction/rasters/wstar_lcpSolved_2020.tif")
wstar_2100 <- rast("data/analysis_interaction/rasters/wstar_lcpSolved_2100.tif")
wobs_1990 <- rast("data/analysis_interaction/rasters/wobs_1990.tif")
wobs_2020 <- rast("data/analysis_interaction/rasters/wobs_2020.tif")

# parameters
dTime_1990 <- mean(c(2020,2015)) - mean(c(1990,1984))
dTime_2020 <- mean(c(2100,2071)) - mean(c(2020,2015))
dtime_period <- 6

# get rates
wrate_1990 <- (wstar_2020 - wstar_1990)*(dtime_period/dTime_1990)
wrate_2020 <- (wstar_2100 - wstar_2020)*(dtime_period/dTime_2020)
wrate_1990_obs <- (wobs_2020 - wobs_1990)*(dtime_period/dTime_1990)

# write rasters
writeRaster(wrate_1990,
            file.path(path_analysis_data_rast,
                      paste0("wrate_1990.tif")))
writeRaster(wrate_2020,
            file.path(path_analysis_data_rast,
                      paste0("wrate_2020.tif")))
writeRaster(wrate_1990_obs,
            file.path(path_analysis_data_rast,
                      paste0("wrate_1990_obs.tif")))
# normalized distances
wrate_1990_norm <- euclidianDist(wrate_1990)
writeRaster(wrate_1990_norm,
            file.path(path_analysis_data_rast,
                      paste0("wrate_1990_norm.tif")))
wrate_2020_norm <- euclidianDist(wrate_2020)
writeRaster(wrate_2020_norm,
            file.path(path_analysis_data_rast,
                      paste0("wrate_2020_norm.tif")))
wrate_1990_obs_norm <- euclidianDist(wrate_1990_obs)
writeRaster(wrate_1990_obs_norm,
            file.path(path_analysis_data_rast,
                      paste0("wrate_1990_obs_norm.tif")))
