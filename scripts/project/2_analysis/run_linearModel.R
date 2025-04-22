library(here)
# Set working directory only if not already set
setwd(here::here())

source("config/config_local.R")
source("scripts/core/2_analysis/lm_geospatial.R")

# names NDVI files
name_ndvi_trend <- "ndvi_trend"
name_ndvi_sig <- "ndvi_sig"

## load response
ndvi_trend <- rast(file.path(path_NDVI_Ju, paste0(name_ndvi_trend, ".tif")))
names(ndvi_trend) <- "NDVI_trend"
ndvi_sig <- rast(file.path(path_NDVI_Ju, paste0(name_ndvi_sig, ".tif")))
names(ndvi_sig) <- "NDVI_trend_significance"

response_list <- list(
  ndvi_trend=ndvi_trend,
  ndvi_sig=ndvi_sig
)

# load predictors
folder <- "probe1_base"
lambda_sh <- rast(file.path(path_analysis, folder, "lambda_sh_mean.tif"))
wrate_sh <- rast(file.path(path_analysis, folder, "w_rate_lm_slope_sh.tif"))


predictor_list <- list(
  lambda_sh=lambda_sh,
  wrate_sh = wrate_sh,
  lamda_wrate_sh = c(lambda_sh, wrate_sh)
)

# load list with linear models
if(file.exists(file.path(path_analysis, folder, "linear_models.rds"))){
  linear_models <- readRDS(file.path(path_analysis, folder, "linear_models.rds"))
} else {
  linear_models <- list()
}

sink(file.path(path_analysis, folder,"linear_models_out.txt"))

# iterate through all models
for(response in response_list){
  for(predictor in predictor_list){
    lm_return <- lm_geospatial(y = response, x = predictor)
    linear_models[[lm_return$name]] <- lm_return$lm
  }
}
sink()
saveRDS(linear_models, file.path(path_analysis, folder, "linear_models.rds"))
