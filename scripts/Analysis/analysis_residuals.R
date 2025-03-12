#set up environment:
library(here)
setwd(here::here())
source("Scripts/Analysis/analysisHfunctions.R")

## set paths and names

name_ndvi_trend <- "ndvi_trend"
name_ndvi_sig <- "ndvi_sig"
name_ndvi_abs_trend <- "ndvi_abs_trend"
name_ndvi_abs_sig <- "ndvi_abs_sig"

## load response
ndvi_trend <- rast(file.path(path_ndvi, paste0(name_ndvi_trend, ".tif")))
ndvi_sig <- rast(file.path(path_ndvi, paste0(name_ndvi_sig, ".tif")))

ndvi_trend_nf <- rast(file.path(path_ndvi, paste0(name_ndvi_trend, "_nofire.tif")))
ndvi_sig_nf <- rast(file.path(path_ndvi, paste0(name_ndvi_sig, "_nofire.tif")))


response_list <- list(
  "ndvi_trend"=ndvi_trend,
  "ndvi_trend_nf"=ndvi_trend_nf,
  "ndvi_sig"=ndvi_sig,
  "ndvi_sig_nf"=ndvi_sig_nf
)

## load predictors
lamda_sh <- rast(file.path(path_analysis_data_rast,"lamda_sh_mean_1990-2020.tif"))
lamda_cf <- rast(file.path(path_analysis_data_rast,"lamda_cf_mean_1990-2020.tif"))
lamda_shcf_harm <- rast(file.path(path_analysis_data_rast,
                                  "lamda_shcf_harmonic_mean_1990-2020.tif"))

wrate_sh <- rast(file.path(path_analysis_data_rast,"wrate_sh.tif"))
wrate_cf <- rast(file.path(path_analysis_data_rast,"wrate_cf.tif"))
wrate_shcf <- rast(file.path(path_analysis_data_rast,"wrate_shcf.tif"))


predictor_list <- list(
  "lamda_sh"=lamda_sh,
  "lamda_cf"=lamda_cf,
  "lamda_shcf_harmonic"=lamda_shcf_harm,
  "wrate_sh" = wrate_sh,
  "wrate_cf" = wrate_cf,
  "wrate_shcf" = wrate_shcf
)

## load linear model ####
lm_list <- readRDS(file = file.path(path_analysis_data_rast,"lm_sum.rds"))

## calculate residuals
for(response in names(response_list)){
  for(predictor in names(predictor_list)){
    cat("calculating residuals", response, "~", predictor, "\n")
    r_pred <- predictor_list[[predictor]]
    r_resp <- response_list[[response]]
    a <- lm_list[[response]][[predictor]][1]
    b <- lm_list[[response]][[predictor]][2]
    # residuals r_i = y_i - a - b*x_i
    model <- a + b*r_pred
    writeRaster(model, file.path(path_analysis_data_rast, "residuals", 
                                 paste0("model_",response, "_", predictor, ".tif")),
                overwrite=TRUE)
    resid <- r_resp - model
    writeRaster(resid, file.path(path_analysis_data_rast, "residuals",
                                 paste0("resid_",response, "_", predictor, ".tif")),
                overwrite=TRUE)
  }
}

## residual ratio
resid_list <- list(
  c("resid_ndvi_trend", "ndvi_trend"),
  c("resid_ndvi_trend_nf", "ndvi_trend_nf")
)

ratio_list <- list(
  c("lamda_sh", "wrate_sh", "sh"),
  c("lamda_cf", "wrate_cf", "cf"),
  c("lamda_shcf_harmonic", "wrate_shcf", "shcf")
)

for(resid in resid_list){
  for(ratio in ratio_list){
    cat("calculating", resid[2], ratio[3], "\n")
    nominator <- rast(file.path(path_analysis_data_rast,
                                "residuals",
                                paste0(resid[1], "_", ratio[1], ".tif")))
    denominator <- rast(file.path(path_analysis_data_rast,
                                "residuals",
                                paste0(resid[1], "_", ratio[2], ".tif")))
    small_value <- exp(-24)

    log_r1 <- log(abs(ifel(nominator == 0, small_value, nominator)))
    log_r2 <- log(abs(ifel(denominator == 0, small_value, denominator)))
    m <- log_r1 - log_r2
    writeRaster(m, file.path(path_analysis_data_rast, "residuals",
                                 paste0("SI_",resid[2],"_",ratio[3], ".tif")),
                overwrite=TRUE)
    
  }
}

