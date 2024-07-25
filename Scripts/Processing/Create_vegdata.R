library(here)
setwd(here::here())
library(terra)

##Prepare paths and masks ####
path_Vegetation_data <- "data/Nill_ArcticVCF/"
path_gjamTime_data <- "data/gjamTime_data/"

# prepare files
channel_list <- list(
  "sh" = "-1-",
  "cf" = "-2-",
  "hb" = "-3-",
  "lc" = "-4-",
  "wt" = "-5-",
  "br" = "-6-"
)
period_list <- list(
  "1984-1990",
  "1991-1996",
  "1997-2002",
  "2003-2008",
  "2009-2014",
  "2015-2020"
)

# Read masks
mastermask <- rast("data/Masks/master_mask.tif")
mastermask_r100 <- rast("data/Masks/master_mask_r100.tif")
mastermask_crop <- rast("data/Masks/master_mask_crop.tif")

## Read all vegetation geotif, mask, safe for using in gjamTime ####


n_iter <- length(period_list)*length(channel_list)
n <- 1
for (period in period_list){
  for (name in names(channel_list)){
    cat("Processing", n, "out of", n_iter, "\n")
    # read
    file <- paste0(period, "_krr-avg_int16-10e3_class", channel_list[[name]], name, ".tif")
    raster <- rast(paste0(path_Vegetation_data, file))
    # mask
    # raster_full <- mask(x=raster, mask = mastermask, maskvalues=0, updatevalue=NA)
    raster_r100 <- mask(x=raster, mask = mastermask_r100, maskvalues=0, updatevalue=NA)
    raster_crop <- mask(x=raster, mask = mastermask_crop, maskvalues=0, updatevalue=NA)
    
    # save 
    # writeRaster(raster_full,
    #             paste0(path_gjamTime_data, "veg_", period, "_", name, "_full.tif"),
    #             overwrite=TRUE)
    writeRaster(raster_r100,
                paste0(path_gjamTime_data, "veg_", period, "_", name, "_r100.tif"),
                overwrite=TRUE)
    writeRaster(raster_crop,
                paste0(path_gjamTime_data, "veg_", period, "_", name, "_crop.tif"),
                overwrite=TRUE)
    # increment
    n = n+1
  }
}

