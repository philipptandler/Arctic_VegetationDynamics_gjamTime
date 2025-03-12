#  this Script calculates the rate of change of the fixedpoint w_rate
# for future wstar2100 - wstar2020 and past wstar2020-wstar1990

#set up environment:
library(here)
setwd(here::here())
source("Scripts/Analysis/analysisHfunctions.R")

wrate_1990_norm <- rast("data/analysis_interaction/rasters/wrate_1990_norm.tif")
wrate_2020_norm <- rast("data/analysis_interaction/rasters/wrate_2020_norm.tif")
wrate_1990_obs_norm <- rast("data/analysis_interaction/rasters/wrate_1990_obs_norm.tif")

wlag_1990_norm <- rast("data/analysis_interaction/rasters/w_lagnorm_1990_star.tif")
wlag_2020_norm <- rast("data/analysis_interaction/rasters/w_lagnorm_2020_star.tif")
wlag_1990_obs_norm <- rast("data/analysis_interaction/rasters/w_lagnorm_1990_obs.tif")

wlag_wrate_1990 <- wlag_1990_norm/wrate_1990_norm
writeRaster(wlag_wrate_1990,
            file.path(path_analysis_data_rast,
                      paste0("wlag_wrate_star_1990.tif")))
wlag_wrate_2020 <- wlag_2020_norm/wrate_2020_norm
writeRaster(wlag_wrate_2020,
            file.path(path_analysis_data_rast,
                      paste0("wlag_wrate_star_2020.tif")))
wlag_wrate_1990_obs <- wlag_1990_obs_norm/wrate_1990_obs_norm
writeRaster(wlag_wrate_1990_obs,
            file.path(path_analysis_data_rast,
                      paste0("wlag_wrate_obs_1990.tif")))


