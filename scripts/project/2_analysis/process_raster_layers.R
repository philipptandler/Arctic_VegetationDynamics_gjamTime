## this script to manipulate and order rasters and layers

library(here)
# Set working directory only if not already set
setwd(here::here())

source("config/config_local.R")
source("scripts/core/2_analysis/lm_geospatial.R")

folder <- "probe1_base"
mastermask <- rast(file.path(path_masks, name_master_mask))

## This script is just used to prepare raster layers for linear model:
# select layer and mask with mastermask and name layer

# ## write lambda_sh as mean(jacobian_1984-1990[[1]], jacobian_1991-1996[[1]], ...)
# 
# lambda_sh_list <- list()
# jacobian_files <- list.files(file.path(path_analysis, folder), pattern = "jacobian_",
#                       full.names = TRUE)
# for(i in 1:length(jacobian_files)){
#   lambda_sh_list[[i]] = rast(jacobian_files[i])[[1]]
# }
# lambda_sh <- mean(rast(lamda_sh_list))
# lambda_sh <- mask(lambda_sh, mastermask, maskvalues=0, updatevalue=NA)
# names(lambda_sh) <- "lambda_sh"
# writeRaster(lambda_sh, file.path(path_analysis, folder, "lambda_sh_mean.tif"), overwrite=T)


## write lambda_cf as mean(jacobian_1984-1990[[1]], jacobian_1991-1996[[1]], ...)
cat("writing lambda_cf ...\n")
lambda_cf_list <- list()
jacobian_files <- list.files(file.path(path_analysis, folder), pattern = "jacobian_w_star_",
                             full.names = TRUE)
for(i in 1:length(jacobian_files)){
  lambda_cf_list[[i]] = rast(jacobian_files[i])[[6]]
}
lambda_cf <- mean(rast(lambda_cf_list))
lambda_cf <- mask(lambda_cf, mastermask, maskvalues=0, updatevalue=NA)
names(lambda_cf) <- "lambda_cf"
writeRaster(lambda_cf, file.path(path_analysis, folder, "lambda_cf_mean.tif"), overwrite=T)
cat("done.\n")
## write lambda_shcf for each period (jacobian_1984-1990[[c(1,2,5,6)]], jacobian_1991-1996[[c(1,2,5,6)]], ...)
cat("Writing jacobian_shrub_conifer ... \n")
jacobian_files <- list.files(file.path(path_analysis, folder), pattern = "jacobian_w_star_",
                             full.names = TRUE)
for(i in 1:length(jacobian_files)){
  cat("(", i, "/", length(jacobian_files), ")\n")
  raster <- rast(jacobian_files[i])[[c(1,2,5,6)]]
  raster <- mask(raster, mastermask, maskvalues=0, updatevalue=NA)
  writeRaster(raster, file.path(gsub("jacobian_w_star_", "jacobian_w_star_shcf_", jacobian_files[i])), overwrite=T)
}
cat("done.\n")
# ## write w_rate_sh as (w_rate_lm_slope[[1]])
# 
# w_rate <- rast(file.path(path_analysis, folder, "w_rate_lm_slope.tif"))
# w_rate_sh <- w_rate[[1]]
# w_rate_sh <- mask(w_rate_sh, mastermask, maskvalues=0, updatevalue=NA)
# names(w_rate_sh) <- "w_rate_sh"
# writeRaster(w_rate_sh, file.path(path_analysis, folder, "w_rate_lm_slope_sh.tif"), overwrite=T)
