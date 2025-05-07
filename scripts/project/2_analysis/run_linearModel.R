library(here)
# Set working directory only if not already set
setwd(here::here())

source("config/config_local.R")
source("scripts/core/2_analysis/lm_geospatial.R")

folder <- "probe1_base_nf"

## load response and name if necessary
ndvi_trend <- rast(file.path(path_NDVI_Ju, "ndvi_trend.tif"))
names(ndvi_trend) <- "NDVI_trend"
ndvi_sig <- rast(file.path(path_NDVI_Ju, "ndvi_sig.tif"))
names(ndvi_sig) <- "NDVI_trend_significance"

## load predictors
lambda_sh <- rast(file.path(path_analysis, folder, "lambda_sh_mean.tif"))
wrate_sh <- rast(file.path(path_analysis, folder, "w_rate_lm_slope_sh.tif"))
# lambda_cf <- rast(file.path(path_analysis, folder, "lambda_cf_mean.tif"))
# wrate_cf <- rast(file.path(path_analysis, folder, "w_rate_lm_slope_cf.tif"))
# lambda_shcf <- rast(file.path(path_analysis, folder, "lambda_shcf_mean.tif"))
# wrate_shcf <- rast(file.path(path_analysis, folder, "w_rate_lm_slope_shcf.tif"))

# prepare lists

response_list <- list(
  ndvi_trend=ndvi_trend,
  ndvi_sig=ndvi_sig
)

predictor_list <- list(
  lambda_sh = lambda_sh,
  wrate_sh = wrate_sh,
  lamda_wrate_sh = c(lambda_sh, wrate_sh)
)

# predictor_list <- list(
#   lambda_sh=lambda_sh,
#   wrate_sh = wrate_sh,
#   lamda_wrate_sh = c(lambda_sh, wrate_sh),
#   lambda_cf=lambda_cf,
#   wrate_cf = wrate_cf,
#   lamda_wrate_cf = c(lambda_cf, wrate_cf),
#   lambda_shcf=lambda_shcf,
#   wrate_shcf = wrate_shcf,
#   lamda_wrate_shcf = c(lambda_shcf, wrate_shcf)
# )


lm_geospatial(response_list, predictor_list, mode="factorial", 
              path_save = file.path(path_analysis, folder),
              save = TRUE, subsample = FALSE,
              sink = TRUE, sink_file = "linear_models_out.txt")

