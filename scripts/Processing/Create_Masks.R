library(here)
setwd(here::here())
library(terra)

path_save_masks <- "data/Masks/"

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

path_watermask_global<- "data/Masks/multitemporal_watermask.tif"
watermask_global <- rast(path_watermask_global)

# Crop the water mask to match the extent of the studyArea
water_mask <- crop(watermask_global, study_region_mask)


# Save the mask as a new raster
writeRaster(water_mask, paste0(path_save_masks, "water_mask.tif"), overwrite=TRUE)


## Create a mastermask ####
# where study_region == TRUE and watermask == TRUE (i.e. no water)
mastermask <-  study_region_mask & water_mask
writeRaster(mastermask, paste0(path_save_masks, "master_mask.tif"), overwrite=TRUE)

## Create a reduced maskermask ####
# create a reduced mastermask for quicker handling of the data and try out code
mastermask <- rast(paste0(path_save_masks, "master_mask.tif"))

# Create an empty raster with the same dimensions
sample_mask <- mastermask
sample_mask[] <- FALSE

# Get the dimensions of the raster
dims <- dim(sample_mask)

# Create indices for the reduced sampling
reduce_i <- seq(1, dims[1], by = 100)
reduce_j <- seq(1, dims[2], by = 100)

# Create a matrix of indices to set to TRUE
indices <- expand.grid(reduce_i, reduce_j)
sample_mask[cbind(indices$Var1, indices$Var2)] <- TRUE

# Overlay with mastermask:
mastermask_reduced <-  mastermask & sample_mask
writeRaster(mastermask_reduced, paste0(path_save_masks, "master_mask_r100.tif"), overwrite=TRUE)

## crop area for testing ####
# read mastermask
mastermask <- rast(paste0(path_save_masks, "master_mask.tif"))

cropped_area <- vect(paste0(path_save_masks, "cropped_area_for_testing/cropped_area_testing.shp"))
mask_cropped <- mask(mastermask, cropped_area)
mastermask_cropped <- mastermask & !is.na(mask_cropped)
writeRaster(mastermask_cropped, paste0(path_save_masks, "master_mask_crop.tif"), overwrite=TRUE)


## Testing the mask ####

#try out and see if it works -> mask img_studyarea with watermask
masked_veg <- mask(img_studyArea, water_mask, maskvalues=0, 
                   updatevalue=NA)
writeRaster(masked_veg, paste0(path_save_masks, "masked_vegetation.tif"), overwrite=TRUE)


