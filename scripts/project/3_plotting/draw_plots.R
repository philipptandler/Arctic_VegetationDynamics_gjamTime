library(here)
# Set working directory only if not already set
setwd(here::here())

source("config/config_local.R")
source("scripts/core/2_analysis/TODO")

lm_summary_coef <- readRDS(file.path(path_analysis, "probe1_base", "linear_models_summary_coef.rds"))
plot_path <- file.path(path_analysis, "probe1_base", "plots")

ndvi_trend <- rast(file.path(path_NDVI_Ju, "ndvi_trend.tif"))
ndvi_sig <- rast(file.path(path_NDVI_Ju, "ndvi_sig.tif"))
lambda_sh <- rast(file.path(path_analysis, folder, "lambda_sh_mean.tif"))
wrate_sh <- rast(file.path(path_analysis, folder, "w_rate_lm_slope_sh.tif"))


hex_plot_geospatial(x = ndvi_trend, y = wrate_sh, subsample = TRUE,
                    size = 1e6,
                    abline_intercept = lm_summary_coef$`NDVI_trend  ~  w_rate_sh`[1],
                    abline_slope = lm_summary_coef$`NDVI_trend  ~  w_rate_sh`[2],
                    save = TRUE, file = file.path(plot_path, "ndvi_trend_wrate_sh.png"))














plotlist <- list(
  list(
    pair = c(ndvi_trend, wrate_sh),
    intercept = lm_summary_coef$`NDVI_trend  ~  w_rate_sh`[1],
    slope = lm_summary_coef$`NDVI_trend  ~  w_rate_sh`[2]
  ),
  list(
    pair = c(ndvi_trend, lambda_sh),
    intercept = lm_summary_coef$`NDVI_trend  ~  lambda_sh`[1],
    slope = lm_summary_coef$`NDVI_trend  ~  lambda_sh`[2]
  ),
  list(
    pair = c(ndvi_sig, wrate_sh),
    intercept = lm_summary_coef$`NDVI_trend_significance  ~  w_rate_sh`[1],
    slope = lm_summary_coef$`NDVI_trend_significance  ~  w_rate_sh`[2]
  ),
  list(
    pair = c(ndvi_sig, lambda_sh),
    intercept = lm_summary_coef$`NDVI_trend_significance  ~  lambda_sh`[1],
    slope = lm_summary_coef$`NDVI_trend_significance  ~  lambda_sh`[2]
  )
)

for()