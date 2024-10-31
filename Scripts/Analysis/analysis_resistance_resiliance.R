
#set up environment:
library(here)
setwd(here::here())
source("Scripts/Analysis/analysisHfunctions.R")


## read rasters
lamda_sh <- rast(file.path(path_analysis_data_rast,"lamda_sh_mean_1990-2020.tif"))
lamda_cf <- rast(file.path(path_analysis_data_rast,"lamda_cf_mean_1990-2020.tif"))

wrate_sh <- rast(file.path(path_analysis_data_rast,"wrate_sh.tif"))
wrate_cf <- rast(file.path(path_analysis_data_rast,"wrate_cf.tif"))
w_star1990 <- rast(file.path(path_analysis_data_rast,"wstar_lcpSolved_1990.tif"))
w_star2020 <- rast(file.path(path_analysis_data_rast,"wstar_lcpSolved_2020.tif"))
w_star_avg <- (w_star1990[[1:2]] + w_star2020[[1:2]])/2
writeRaster(w_star_avg, file.path(path_analysis_data_rast, "w_star_avg_shcf.tif"))


## get resiliance
get_resiliance <- function(lamda, save=F,time = 1, name="resiliance"){
  resiliance <- (1-exp(lamda))/(1+exp(lamda))
  if(save){
    writeRaster(resiliance, file.path(path_analysis_data_rast, paste0(name, ".tif")))
  }
  return(resiliance)
}
resiliance_sh <- get_resiliance(lamda_sh, save=T, name = "resiliance_sh")
resiliance_cf <- get_resiliance(lamda_cf, save=T, name = "resiliance_cf")


## get resistance
get_resistance <- function(wrate, wstar, save=F,time = 1, name="resistance"){
  resistance <- (wstar - abs(wrate))/(wstar + abs(wrate))
  if(save){
    writeRaster(resistance, file.path(path_analysis_data_rast, paste0(name, ".tif")))
  }
  return(resistance)
}
resistance_sh <- get_resistance(wrate_sh, w_star_avg[[1]], save=T, name = "resistance_sh")
resistance_cf <- get_resistance(wrate_cf, w_star_avg[[2]], save=T, name = "resistance_cf")


## difference

StabDiff <- function(resiliance, resistance, save = F, name = "stab_diff"){
  sdiff <- resistance - resiliance
  if(save){
    writeRaster(sdiff, file.path(path_analysis_data_rast, paste0(name, ".tif")))
    
  }
  return(sdiff)
}
stab_diff_sh <- StabDiff(resiliance_sh, resistance_sh, save = T, name = "stab_diff_sh")
stab_diff_cf <- StabDiff(resiliance_cf, resistance_cf, save = T, name = "stab_diff_cf")




