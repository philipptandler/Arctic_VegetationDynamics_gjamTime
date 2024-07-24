setwd("C:/Users/phili/Documents/Dateien_Philipp/EnvironmentalSciencesMSc/MasterThesis/")
library(terra)


##Prepare paths and masks ####

path_firedata<- "data/CNFDB/fire_polygons_all/"
path_gjamTime_data <- "data/gjamTime_data/"

# Read masks
mask_StudyRegion <- rast("data/Masks/study_region_mask.tif")
mastermask_full <- rast("data/Masks/master_mask.tif")
mastermask_r100 <- rast("data/Masks/master_mask_r100.tif")
mastermask_crop <- rast("data/Masks/master_mask_crop.tif")

## process polygon layer ####

# read shapefile

# project to study area

# crop to study area

# create fire mask total and per period






