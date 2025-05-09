library(here)
# Set working directory only if not already set
setwd(here::here())

source("config/config_local.R")
source("scripts/core/3_plotting/hexplot_geospatial.R")

folder <- "probe1_base_nf"

lm_summary_coef <- readRDS(file.path(path_analysis, folder, "linear_models_summary_coef.rds"))
# lm_gjamint<- readRDS("analysis/gjam_interaction/rasters/lm_list.rds")
plot_path <- file.path(path_analysis, folder, "plots")

ndvi_trend <- rast(file.path(path_NDVI_Ju, "ndvi_trend.tif"))
# ndvi_trend_nf <- rast(file.path(path_NDVI_Ju, "ndvi_trend_nofire.tif"))

lambda_sh <- rast(file.path(path_analysis, folder, "lambda_sh_mean.tif"))
lambda_cf <- rast(file.path(path_analysis, folder, "lambda_cf_mean.tif"))

wrate_sh <- rast(file.path(path_analysis, folder, "w_rate_lm_slope_sh.tif"))
wrate_cf <- rast(file.path(path_analysis, folder, "w_rate_lm_slope_cf.tif"))



plotlist <- hexplot_geospatial(y = ndvi_trend, x = lambda_sh,
                               ylab = "NDVI trend - no fire", xlab = expression(lambda[sh]),
                               subsample = TRUE,
                               size = 1e6, abline = TRUE,
                               abline_intercept = lm_summary_coef$NDVI_trend__on__lambda_sh[1],
                               abline_slope = lm_summary_coef$NDVI_trend__on__lambda_sh[2],
                               save = TRUE, file = file.path(plot_path, "ndvi_trend_sh_lambda.png"))
