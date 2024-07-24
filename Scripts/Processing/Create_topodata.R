setwd("C:/Users/phili/Documents/Dateien_Philipp/EnvironmentalSciencesMSc/MasterThesis/")
library(terra)
##Prepare paths and masks ####

path_ArcticDEM_32m<- "data/ArcticDEM_32m/"
path_CopernicusDEM_30m<- "data/CopernicusDEM_30m/"
path_gjamTime_data <- "data/gjamTime_data/"

# Read masks
mask_StudyRegion <- rast("data/Masks/study_region_mask.tif")
mask_StudyRegion_ext <- extend(mask_StudyRegion, 5) # slightly extend study area
mastermask <- rast("data/Masks/master_mask.tif")
mastermask_r100 <- rast("data/Masks/master_mask_r100.tif")
mastermask_crop <- rast("data/Masks/master_mask_crop.tif")

## helperfunction ####
# n is an odd number
tpiKernel <- function(n){
  if ((n-1) %% 2 != 0 || n <= 1){stop("Invalid integer provided")}
  val <- 1/(n**2 - 1)
  large_kernel <- matrix(val, nrow=n, ncol=n)
  middle <- (n+1)/2
  large_kernel[middle,middle] = 0
  return(large_kernel)
}


## Read and Project the DEM ####

#read Arctic DEM 32m and COP30m
ARC_DEM_32 <- rast(paste0(path_ArcticDEM_32m, "output_hh.tif"))
COP_DEM_30 <- rast(paste0(path_CopernicusDEM_30m, "output_COP30.tif"))

# Project and (save & load) the Arctic and Copernicus DEM
# ARC_DEM_32_proj <- project(ARC_DEM_32, mask_StudyRegion)
# COP_DEM_30_proj <- project(COP_DEM_30, mask_StudyRegion)
ARC_DEM_32_proj_ext <- project(ARC_DEM_32, mask_StudyRegion_ext)
COP_DEM_30_proj_ext <- project(COP_DEM_30, mask_StudyRegion_ext)

# writeRaster(ARC_DEM_32_proj,
#             paste0(path_ArcticDEM_32m, "Arc_DEM_32m_proj.tif"), overwrite=TRUE)
# writeRaster(COP_DEM_30_proj,
#             paste0(path_CopernicusDEM_30m, "COP_DEM_30_proj.tif"), overwrite=TRUE)
writeRaster(ARC_DEM_32_proj_ext,
            paste0(path_ArcticDEM_32m, "Arc_DEM_32m_proj_ext.tif"), overwrite=TRUE)
writeRaster(COP_DEM_30_proj_ext,
            paste0(path_CopernicusDEM_30m, "COP_DEM_30_proj_ext.tif"), overwrite=TRUE)

# ARC_DEM_32_proj <- rast(paste0(path_ArcticDEM_32m, "Arc_DEM_32m_proj.tif"))
# COP_DEM_30_proj <- rast(paste0(path_CopernicusDEM_30m, "COP_DEM_30_proj.tif"))
ARC_DEM_32_proj_ext <- rast(paste0(path_ArcticDEM_32m, "Arc_DEM_32m_proj_ext.tif"))
COP_DEM_30_proj_ext <- rast(paste0(path_CopernicusDEM_30m, "COP_DEM_30_proj_ext.tif"))

## fill NAs in ARC_DEM with COP_DEM ####

# create masks where either of the DEM is NA
mask_ARC <- is.na(ARC_DEM_32_proj_ext)
mask_COP <- is.na(COP_DEM_30_proj_ext)
# common area, subsampled at factor 1:10000 to reduce data, and thier indices
commonArea <- !mask_ARC & !mask_COP & extend(mastermask_r100, 5)
area_indices <- which(values(commonArea) == 1)
# values for a linear model
ARC_DEM_32_proj_values <- values(ARC_DEM_32_proj_ext)[area_indices]
COP_DEM_30_proj_values <- values(COP_DEM_30_proj_ext)[area_indices]

#fit a linear model ARc ~ COP
plot(ARC_DEM_32_proj_values ~ COP_DEM_30_proj_values)
lm_SA <- lm(ARC_DEM_32_proj_values ~ COP_DEM_30_proj_values)
# create a linear transformed DEM of Copermicus
COP_DEM_30_proj_ext_lm <- coef(lm_SA)[1] + coef(lm_SA)[2]*COP_DEM_30_proj_ext

# Merge pixels with no data in Arctic DEM - ARCDEM and COP30 are biased!
ARC_DEM_32_proj_ext_filled <- cover(ARC_DEM_32_proj_ext, COP_DEM_30_proj_ext_lm)
writeRaster(ARC_DEM_32_proj_ext_filled,
            paste0(path_ArcticDEM_32m, "Arc_DEM_32m_proj_ext_lm.tif"), overwrite=TRUE)


## Create Terrain files ####
ARC_DEM_32_proj_ext_filled <- rast(paste0(path_ArcticDEM_32m, "Arc_DEM_32m_proj_ext_lm.tif"))

ARC_DEM_32_slope <- terrain(ARC_DEM_32_proj_ext_filled, v = "slope")
ARC_DEM_32_aspect <- terrain(ARC_DEM_32_proj_ext_filled, v = "aspect")
ARC_DEM_32_tpi <- terrain(ARC_DEM_32_proj_ext_filled, v = "TPI")

# ARC_DEM_32_mean17 <- focal(ARC_DEM_32_proj_ext_filled, w=tpiKernel(17), fun=mean, pad=TRUE, na.rm=TRUE)
# ARC_DEM_32_tpi17 <- ARC_DEM_32_proj_ext_filled - ARC_DEM_32_mean17


# crop on extent of study region
ARC_DEM_32_elev <- crop(ARC_DEM_32_proj_ext_filled, mask_StudyRegion)
ARC_DEM_32_slope <- crop(ARC_DEM_32_slope, mask_StudyRegion)
ARC_DEM_32_aspect <- crop(ARC_DEM_32_aspect, mask_StudyRegion)
ARC_DEM_32_tpi <- crop(ARC_DEM_32_tpi, mask_StudyRegion)

# ARC_DEM_32_tpi17 <- crop(ARC_DEM_32_tpi17, mask_StudyRegion)


# save the created raster
writeRaster(ARC_DEM_32_elev,
            paste0(path_ArcticDEM_32m, "Arc_DEM_32m_elev.tif"), overwrite=TRUE)
writeRaster(ARC_DEM_32_slope,
            paste0(path_ArcticDEM_32m, "Arc_DEM_32m_slope.tif"), overwrite=TRUE)
writeRaster(ARC_DEM_32_aspect,
            paste0(path_ArcticDEM_32m, "Arc_DEM_32m_aspect.tif"), overwrite=TRUE)
writeRaster(ARC_DEM_32_tpi,
            paste0(path_ArcticDEM_32m, "Arc_DEM_32m_tpi.tif"), overwrite=TRUE)

# writeRaster(ARC_DEM_32_tpi17,
#             paste0(path_ArcticDEM_32m, "Arc_DEM_32m_tpi17.tif"), overwrite=TRUE)

## write terrainfiles and apply mastermask ####
ARC_DEM_32_elev <- rast(paste0(path_ArcticDEM_32m, "Arc_DEM_32m_elev.tif"))
ARC_DEM_32_slope <- rast(paste0(path_ArcticDEM_32m, "Arc_DEM_32m_slope.tif"))
ARC_DEM_32_aspect <- rast(paste0(path_ArcticDEM_32m, "Arc_DEM_32m_aspect.tif"))
ARC_DEM_32_tpi <- rast(paste0(path_ArcticDEM_32m, "Arc_DEM_32m_tpi.tif"))
# merge in one raster to be quicker
topodata <- c(ARC_DEM_32_elev,
              ARC_DEM_32_slope,
              ARC_DEM_32_aspect,
              ARC_DEM_32_tpi)
names(topodata) <- c("elevation", "slope", "aspect", "tpi")

# mask with mastermask and mastermask_r100
topodata_masked_full <- mask(x=topodata, mask = mastermask, maskvalues=0, updatevalue=NA)
topodata_masked_r100 <- mask(x=topodata, mask = mastermask_r100 , maskvalues=0, updatevalue=NA)
topodata_masked_crop <- mask(x=topodata, mask = mastermask_crop , maskvalues=0, updatevalue=NA)
topo_masterlist = c("elev", "slope", "aspect", "tpi")


for (i in c(1:4)){
  varname <- topo_masterlist[i]
  writeRaster(topodata_masked_full[[i]],
              paste0(path_gjamTime_data, "topo_const_", varname, "_full.tif"),
              overwrite=TRUE)
  writeRaster(topodata_masked_r100[[i]],
              paste0(path_gjamTime_data, "topo_const_", varname, "_r100.tif"),
              overwrite=TRUE)
  writeRaster(topodata_masked_crop[[i]],
              paste0(path_gjamTime_data, "topo_const_", varname, "_crop.tif"),
              overwrite=TRUE)
}


