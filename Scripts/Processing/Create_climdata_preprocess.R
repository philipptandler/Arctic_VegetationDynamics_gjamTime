# This Script sets up 
library(terra)
rootdir <- "C:/Users/phili/Documents/Dateien_Philipp/EnvironmentalSciencesMSc/MasterThesis/"
setwd(rootdir)

## general definitions ####
path_CHELSA_pastraw <- "data/CHELSA_climatedata/data_past_raw/"
path_CHELSA_pastcrop <- "data/CHELSA_climatedata/data_past_crop/"
path_CHELSA_futureraw <- "data/CHELSA_climatedata/data_future_raw/"
path_CHELSA_futurecrop <- "data/CHELSA_climatedata/data_future_crop/"
path_CHELSA <- "data/CHELSA_climatedata/"



# Read masks
mask_StudyRegion <- rast("data/Masks/study_region_mask.tif")
mastermask <- rast("data/Masks/master_mask.tif")
mastermask_r100 <- rast("data/Masks/master_mask_r100.tif")
mastermask_crop <- rast("data/Masks/master_mask_crop.tif")

study_region_extent_CHELSA_CRS <- c(-142, -128, 66.5, 70.5)

## helperfunctions ####

crop_files <- function(pattern, pathFrom, pathTo){
  filelist <- list.files(pathFrom, pattern = pattern, full.names = FALSE)
  n_iter <- length(filelist)
  n <- 1
  for(file in filelist){
    cat("processing", n, "out of", n_iter, "rasters \n")
    raster <- rast(paste0(pathFrom, file))
    raster_crop <- crop(raster, study_region_extent_CHELSA_CRS)
    writeRaster(raster_crop, paste0(pathTo, file), overwrite=TRUE)
    n <- n+1
  }
}


## define file patters ####

# pattern_tas_past <- "CHELSA_tas_\\d{2}_\\d{4}_V\\.2\\.1\\.tif"
pattern_pr_past <- "CHELSA_pr_\\d{2}_\\d{4}_V\\.2\\.1\\.tif"
# pattern_future <- "CHELSA_.*_2071_2100_norm\\.tif"


# crop_files(pattern_tas_past, path_CHELSA_pastraw, path_CHELSA_pastcrop)
crop_files(pattern_pr_past, path_CHELSA_pastraw, path_CHELSA_pastcrop)
crop_files(pattern_future, path_CHELSA_futureraw, path_CHELSA_futurecrop)
