library(here)
setwd(here::here())
library(terra)

path_save_masks <- "data/masks/"

## Create Template Mask reprojections ####

# Read first vegetation geotiff as template
path_studyArea<- "data/Nill_ArcticVCF/1984-1990_krr-avg_int16-10e3_class-1-sh.tif"
img_studyArea <- rast(path_studyArea)
# Create the mask where vegetation raster is not NA
study_region_mask <- !is.na(img_studyArea)
names(study_region_mask) <- "StudyArea"
varnames(study_region_mask) <- "data_available"
print(study_region_mask)


# Save the mask as a new raster
writeRaster(study_region_mask, paste0(path_save_masks, "study_region_mask.tif"), overwrite=TRUE)


## Create a Landmask of the extent of study region mask ####

path_watermask_global<- "data/masks/multitemporal_watermask.tif"
watermask_global <- rast(path_watermask_global)

# Crop the water mask to match the extent of the studyArea
water_mask <- crop(watermask_global, study_region_mask)


# Save the mask as a new raster
writeRaster(water_mask, paste0(path_save_masks, "water_mask.tif"), overwrite=TRUE)


## Create a mastermask ####
# where study_region == TRUE and watermask == TRUE (i.e. no water)
mastermask <-  study_region_mask & water_mask
writeRaster(mastermask, paste0(path_save_masks, "master_mask.tif"), overwrite=TRUE)


