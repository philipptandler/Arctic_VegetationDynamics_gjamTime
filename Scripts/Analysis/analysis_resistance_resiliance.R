
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
# w_star_avg <- rast(file.path(path_analysis_data_rast, "w_star_avg_shcf.tif"))

## time dependence
timesteps <- 5

## get resiliance
get_resiliance <- function(lamda, save=F,time = 1, name="resiliance"){
  lamda_time <- lamda*timesteps
  resiliance <- (1-exp(lamda_time))/(1+exp(lamda_time))
  if(save){
    writeRaster(resiliance, file.path(path_analysis_data_rast, paste0(name,"_",timesteps,".tif")),
                overwrite=TRUE)
  }
  return(resiliance)
}
resiliance_sh <- get_resiliance(lamda_sh, save=T, name = "resiliance_sh")
resiliance_cf <- get_resiliance(lamda_cf, save=T, name = "resiliance_cf")


## get resistance
get_resistance <- function(wrate, wstar, save=F,time = 1, name="resistance"){
  resistance <- (wstar - timesteps*abs(wrate))/(wstar + timesteps*abs(wrate))
  if(save){
    writeRaster(resistance, file.path(path_analysis_data_rast, paste0(name,"_",timesteps,".tif")),
                overwrite=TRUE)
  }
  return(resistance)
}
resistance_sh <- get_resistance(wrate_sh, w_star_avg[[1]], save=T, name = "resistance_sh")
resistance_cf <- get_resistance(wrate_cf, w_star_avg[[2]], save=T, name = "resistance_cf")


## difference

StabDiff <- function(resiliance, resistance, save = F, name = "stab_diff"){
  sdiff <- resistance - resiliance
  if(save){
    writeRaster(sdiff, file.path(path_analysis_data_rast, paste0(name,"_",timesteps, ".tif")),
                overwrite=TRUE)
    
  }
  return(sdiff)
}
stab_diff_sh <- StabDiff(resiliance_sh, resistance_sh, save = T, name = "stab_diff_sh")
stab_diff_cf <- StabDiff(resiliance_cf, resistance_cf, save = T, name = "stab_diff_cf")


## find time t where resiliene = resisiance
# this requires to solve exp(lamda*t)=t*abs(rate)/wstar
# shrub
wrate_wstar_sh <- abs(wrate_sh/w_star_avg[[1]])
writeRaster(wrate_wstar_sh, file.path(path_analysis_data_rast, "wrate_wstar_sh.tif"),
            overwrite=TRUE)
max_sh_mask <- (
  (lamda_sh >= -0.001 & wrate_wstar_sh <= 0.016)|
  (lamda_sh >= -0.01 & wrate_wstar_sh <= 0.01)|
  (wrate_wstar_sh <= 0.0001)
  ) #updatavalue = 60

#conifer
wrate_wstar_cf <- abs(wrate_cf/w_star_avg[[2]])
writeRaster(wrate_wstar_cf, file.path(path_analysis_data_rast, "wrate_wstar_cf.tif"),
            overwrite=TRUE)
max_cf_mask <- (
  (lamda_cf >= -0.001 & wrate_wstar_cf <= 0.016)|
  (lamda_cf >= -0.01 & wrate_wstar_cf <= 0.01)|
  (wrate_wstar_cf <= 0.0001)
) #updatavalue = 60


#solves exp(kx)=x*d
solve_approx <- function(r) {
  k <- r[[1]]
  d <- r[[2]]
  max_iter <- 10
  x <- 2/(4*d-k)
  for (i in 1:10) {
    cat("Iteration", i, "\n")
    f_x <- exp(k * x) - x * d
    f_prime_x <- k * exp(k * x) - d
    x <- x - f_x / f_prime_x
  }
  return(x)
}
# shrub

calc_sh <- c(
  lamda_sh,
  wrate_wstar_sh
)
calc_sh <- mask(calc_sh, max_sh_mask, maskvalues=1, updatevalue=NA)
time_equalStability_sh <- solve_approx(calc_sh)
time_equalStability_sh <- mask(time_equalStability_sh, max_sh_mask, maskvalues=1, updatevalue=60)
writeRaster(time_equalStability_sh, file.path(path_analysis_data_rast,
                                              "time_equalStability_sh.tif"))
# conifer
calc_cf <- c(
  lamda_cf,
  wrate_wstar_sh
)
calc_cf <- mask(calc_cf, max_cf_mask, maskvalues=1, updatevalue=NA)
time_equalStability_cf <- solve_approx(calc_cf)
time_equalStability_cf <- mask(time_equalStability_cf, max_cf_mask, maskvalues=1, updatevalue=60)
writeRaster(time_equalStability_cf, file.path(path_analysis_data_rast,
                                              "time_equalStability_cf.tif"))
