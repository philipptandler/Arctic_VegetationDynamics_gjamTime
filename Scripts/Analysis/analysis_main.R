#set up environment:
library(here)
setwd(here::here())
# useScratchifTerminal <- TRUE 
# useScratch <- TRUE
source("Scripts/Analysis/analysisHfunctions.R")

## set paths and names

name_ndvi_trend <- "ndvi_trend"
name_ndvi_sig <- "ndvi_sig"
name_ndvi_abs_trend <- "ndvi_abs_trend"
name_ndvi_abs_sig <- "ndvi_abs_sig"

## load response
ndvi_abs_trend <- rast(file.path(path_ndvi, paste0(name_ndvi_abs_trend, ".tif")))
ndvi_abs_sig <- rast(file.path(path_ndvi, paste0(name_ndvi_abs_sig, ".tif")))
ndvi_trend <- rast(file.path(path_ndvi, paste0(name_ndvi_trend, ".tif")))
ndvi_sig <- rast(file.path(path_ndvi, paste0(name_ndvi_sig, ".tif")))

ndvi_abs_trend_nf <- rast(file.path(path_ndvi, paste0(name_ndvi_abs_trend, "_nofire.tif")))
ndvi_abs_sig_nf <- rast(file.path(path_ndvi, paste0(name_ndvi_abs_sig, "_nofire.tif")))
ndvi_trend_nf <- rast(file.path(path_ndvi, paste0(name_ndvi_trend, "_nofire.tif")))
ndvi_sig_nf <- rast(file.path(path_ndvi, paste0(name_ndvi_sig, "_nofire.tif")))


response_list <- list(
  "ndvi_abs_trend"=ndvi_abs_trend,
  "ndvi_abs_sig"=ndvi_abs_sig,
  "ndvi_trend"=ndvi_trend,
  "ndvi_sig"=ndvi_sig,
  "ndvi_abs_trend_nf"=ndvi_abs_trend_nf,
  "ndvi_abs_sig_nf"=ndvi_abs_sig_nf,
  "ndvi_trend_nf"=ndvi_trend_nf,
  "ndvi_sig_nf"=ndvi_trend_nf
)

## load predictors
lamda_harmonic <- rast(file.path(path_analysis_data_rast,"lamda_harmonic_mean_1990-2020.tif"))
tau_harmonic <- rast(file.path(path_analysis_data_rast,"tau_harmonic_mean_1990-2020.tif"))
lamda_sh <- rast(file.path(path_analysis_data_rast,"lamda_sh_mean_1990-2020.tif"))
tau_sh <- rast(file.path(path_analysis_data_rast,"tau_sh_mean_1990-2020.tif"))

predictor_list <- list(
  "lamda_sh"=lamda_sh,
  "tau_sh"=tau_sh,
  "lamda_harmonic"=lamda_harmonic,
  "tau_harmonic"=tau_harmonic
)
sink("output.txt")
## linear model
for(response in names(response_list)){
  for(predictor in names(predictor_list)){
    cat("=====================================================================\n")
    cat("Linear Model", response, "~", predictor,":\n")
    y <- values(response_list[[response]])
    x <- values(predictor_list[[predictor]])
    this_lm <- lm(y ~ x)
    summary(this_lm)
    cat("\n\n\n\n\n")
  }
}
sink()
