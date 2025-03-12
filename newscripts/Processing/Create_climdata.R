# This Script sets up 
library(terra)
library(here)
setwd(here::here())

## general definitions ####
path_CHELSA_pastcrop <- "data/CHELSA_climatedata/data_past_crop/"
path_CHELSA_futurecrop <- "data/CHELSA_climatedata/data_future_crop/"
path_CHELSA <- "data/CHELSA_climatedata/"
path_gjamTime_data <- "data/gjamTime_data/"

# Read masks
mask_StudyRegion <- rast("data/Masks/study_region_mask.tif")
mastermask <- rast("data/Masks/master_mask.tif")
mastermask_r100 <- rast("data/Masks/master_mask_r100.tif")
mastermask_crop <- rast("data/Masks/master_mask_crop.tif")


## define files ####

# period_list <- list(
#   "1984-1990" = c(1984:1990),
#   "1991-1996" = c(1991:1996),
#   "1997-2002" = c(1997:2002),
#   "2003-2008" = c(2003:2008),
#   "2009-2014" = c(2009:2014),
#   "2015-2020" = c(2015:2019),
#   "2071-2100" = "2071_2100"
# )
period_list <- list(
  "2071-2100" = "2071_2100"
)
var_vec <- c("tas", "tasw", "tass", "pr", "prw", "prs")

# period_list <- list(
#   "1984-1990" = c(1984:1990)
# )
# var_vec <- c("tass")



## helperfunctions ####

get_raster <- function(periodname, var){
  # find relevant months and variable of CHELSA
  months <- vector()
  if(var == "tas" || var == "pr"){months <- c(1:12)}
  if(var == "tass" || var == "prs"){months <- c(5:9)}
  if(var == "tasw" || var == "prw"){months <- c(1:4, 10:12)}
  varname <- NULL
  if(var == "tas" || var == "tass"|| var == "tasw"){varname <- "tas"}
  if(var == "pr" || var == "prs"|| var == "prw"){varname <- "pr"}
  
  # r_vec collects raster over all relevant months for this period and variable
  r_vec <- c()
  # for each month (01, 02, ...)
  for (month in months){
    # raster specific month for this period and variable
    raster_this_month <- NULL
    if(periodname != "2071-2100"){
      # rastervec for this (e.g. 01) month with all years (e.g. 1997-2002)
      # collects raster of given month for each year in period
      r_vec_this_month <- c()
      # for all years in e.g. 1997-2002
      period_years <- period_list[[periodname]]
      if(varname == "pr" && periodname == "2015-2020"){period_years <- c(2015:2018)}
      for (year in period_years){
        file <- paste0(path_CHELSA_pastcrop, "CHELSA_", varname, "_",
                           sprintf("%02d", month), "_", year, "_V.2.1.tif")
        
        r <- rast(file)
        r_vec_this_month <- append(r_vec_this_month, r)
      }

      raster_this_month <- mean(r_vec_this_month)
    }
    
    if(periodname == "2071-2100"){
      file <- paste0(path_CHELSA_futurecrop, "CHELSA_gfdl-esm4_r1i1p1f1_w5e5_ssp585_",
                          varname, "_",
                          sprintf("%02d", month), "_", "2071_2100_norm.tif")
      raster_this_month <- rast(file)
    }
    # add raster of this month to vector containing all relevant rasters
    r_vec <- append(r_vec, raster_this_month)
  }
  raster_done <- NULL
  if(varname == "tas"){raster_done <- mean(r_vec)}
  if(varname == "pr"){raster_done <- sum(r_vec)}
  if(periodname == "2071-2100"){raster_done <- raster_done*10 + 2731.5}
  return(raster_done)
}

## load and process ####

n_iter <- length(period_list)*length(var_vec)
n <- 1
for (periodname in names(period_list)){
  for (var in var_vec){
    cat("Loading", n, "out of", n_iter,":", periodname, "-", var, "\n")
    # get the raw raster for this period and this variable
    raster <- get_raster(periodname, var)
    names(raster) <- var
    cat("   raster loaded \n")
    
    # project to Studyregion
    cat("   Projecting raster \n")
    raster_proj <- project(raster, mask_StudyRegion)
    # apply mastermasks
    cat("   Masking raster \n")
    raster_proj_full <- mask(x=raster_proj, mask = mastermask, maskvalues=0, updatevalue=NA)
    raster_proj_r100 <- mask(x=raster_proj, mask = mastermask_r100, maskvalues=0, updatevalue=NA)
    raster_proj_crop <- mask(x=raster_proj, mask = mastermask_crop, maskvalues=0, updatevalue=NA)
    
    # save
    cat("   Writing rasters:")
    writeRaster(raster_proj_full,
                paste0(path_gjamTime_data, "clim_", periodname, "_", var, "_full.tif"),
                overwrite=TRUE)
    cat("1")
    writeRaster(raster_proj_r100,
                paste0(path_gjamTime_data, "clim_", periodname, "_", var, "_r100.tif"),
                overwrite=TRUE)
    cat(", 2")
    writeRaster(raster_proj_crop,
                paste0(path_gjamTime_data, "clim_", periodname, "_", var, "_crop.tif"),
                overwrite=TRUE)
    cat(", 3 done  \n")
    cat("\n")
    # iteration
    n <- n + 1
  }
}
    
    
    



