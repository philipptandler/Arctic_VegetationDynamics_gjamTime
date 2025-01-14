# This is to plot the data

#set up environment:
library(here)
setwd(here::here())
source("Scripts/Plotting/plottingHfunctions.R")

## load linear model ####
lm_list <- readRDS(file = file.path(path_analysis_data_rast,"lm_sum.rds"))

## load rasters ####
# as list
var_list <- list(
  "ndvi_trend"=list(r=ndvi_trend,lab="NDVI trend"),
  "ndvi_trend_nf"=list(r=ndvi_trend_nf,lab="NDVI trend"),
  "ndvi_sig"=list(r=ndvi_sig,lab="NDVI significance"),
  "ndvi_sig_nf"=list(r=ndvi_sig_nf,lab="NDVI significance"),
  "lamda_sh"=list(r=lamda_sh,lab=expression(lambda[sh])),
  "lamda_cf"=list(r=lamda_cf,lab=expression(lambda[cf])),
  "lamda_shcf_harmonic"=list(r=lamda_shcf_harm,lab=expression(lambda[harmonic])),
  "wrate_sh"=list(r=wrate_sh,lab=expression(wrate[sh])),
  "wrate_cf"=list(r=wrate_cf,lab=expression(wrate[cf])),
  "wrate_shcf"=list(r=wrate_shcf,lab=expression(wrate[shcf])),
  "elev"=list(r=rast("data/gjamTime_data/topo_const_elev_full.tif"),lab="elevation"),
  "slope"=list(r=rast("data/gjamTime_data/topo_const_slope_full.tif"),lab="slope"),
  "SI_ndvi_trend"=list(r=rast("data/analysis_interaction/rasters/residuals/SI_ndvi_trend_sh.tif"),lab="Stability Index shrub")
)


## plot list ####

# plotlist <- list(
#   c("ndvi_trend", "lamda_shcf_harmonic"),
#   c("ndvi_trend", "lamda_sh"),
#   c("ndvi_trend", "lamda_cf"),
#   c("ndvi_sig", "lamda_shcf_harmonic"),
#   c("ndvi_sig", "lamda_sh"),
#   c("ndvi_sig", "lamda_cf"),
#   c("ndvi_trend_nf", "lamda_shcf_harmonic"),
#   c("ndvi_trend_nf", "lamda_sh"),
#   c("ndvi_trend_nf", "lamda_cf"),
#   c("ndvi_sig_nf", "lamda_shcf_harmonic"),
#   c("ndvi_sig_nf", "lamda_sh"),
#   c("ndvi_sig_nf", "lamda_cf")
# )

# plotlist <- list(
#   c("ndvi_trend", "wrate_shcf"),
#   c("ndvi_trend", "wrate_sh"),
#   c("ndvi_trend", "wrate_cf"),
#   c("ndvi_sig", "wrate_shcf"),
#   c("ndvi_sig", "wrate_sh"),
#   c("ndvi_sig", "wrate_cf"),
#   c("ndvi_trend_nf", "wrate_shcf"),
#   c("ndvi_trend_nf", "wrate_sh"),
#   c("ndvi_trend_nf", "wrate_cf"),
#   c("ndvi_sig_nf", "wrate_shcf"),
#   c("ndvi_sig_nf", "wrate_sh"),
#   c("ndvi_sig_nf", "wrate_cf")
# )
plotlist <- list(
  c("ndvi_trend", "lamda_sh")
)

## plotting ####
for(pair in plotlist){
  #raster
  ry <- var_list[[pair[1]]]$r
  rx <- var_list[[pair[2]]]$r
  # plotnames
  laby <- var_list[[pair[1]]]$lab
  labx <- var_list[[pair[2]]]$lab
  # file out
  filename <- paste0(pair[1], "_", pair[2], ".png")
  raster <- c(ry,rx)
  # lm to plot
  lm_sum = NULL
  if(check_exists(lm_list, pair[1], pair[2])){
    lm_sum <- lm_list[[pair[1]]][[pair[2]]]
  }
  hexagon_plot(raster,
               lnames = c(laby,labx),
               lm = lm_sum,
               samplesize = 1e6,
               save=F,
               xlim = c(-0.4,0.0),
               showplot=T,
               filename=filename)
  
}
