library(here)
setwd(here::here())
library(terra)
library(mvtnorm)
##Prepare paths and masks ####

path_soilGrids_raw <- "data/SoilGrids/raw/"
path_soilGrids <- "data/SoilGrids/"
path_gjamTime_data <- "data/gjamTime_data/"
path_stockersoil <- "data/Stocker_Soil"

# Read masks
mask_StudyRegion <- rast("data/Masks/study_region_mask.tif")
mastermask <- rast("data/Masks/master_mask.tif")
mastermask_r100 <- rast("data/Masks/master_mask_r100.tif")
mastermask_crop <- rast("data/Masks/master_mask_crop.tif")


## helperfunctions ####

# Function to generate an n x n matrix approximating a 2D normal distribution
generate_normal_matrix <- function(n, mean = c(0, 0), sigma = matrix(c(1, 0, 0, 1), ncol = 2)) {
  sd_dist <- 4
  # Generate grid of x and y values
  x <- seq(-sd_dist, sd_dist, length.out = n)
  y <- seq(-sd_dist, sd_dist, length.out = n)
  
  # Create grid of x and y coordinates
  xy <- expand.grid(x = x, y = y)
  
  # Calculate bivariate normal density using mvtnorm::dmvnorm
  z <- dmvnorm(xy, mean = mean, sigma = sigma)
  
  # Reshape z to n x n matrix
  M <- matrix(z, nrow = n, ncol = n, byrow = TRUE)
  
  return(M)
}

## Read soildata ####

read_soildata <- function(path, tile_vec = c(1:14)){
  # loop over all tiles
  tile_list <- list()
  for (tile in tile_vec) {
    print(paste0("entering loop, tile = ", tile))
    # identify layers
    filepattern_10 <- paste0("^10kPa_", tile, "_\\d+\\.tif$")
    filepattern_1500 <- paste0("^1500kPa_", tile, "_\\d+\\.tif$")
    list_thisTile_10kPa <- list.files(path_soilGrids_raw,
                                      pattern = filepattern_10, full.names = TRUE)
    list_thisTile_1500kPa <- list.files(path_soilGrids_raw,
                                        pattern = filepattern_1500, full.names = TRUE)
    # make raster
    stack_10kPa<- rast(list_thisTile_10kPa)
    stack_1500kPa <- rast(list_thisTile_1500kPa)
    
    # make nodata (0, -32768)
    stack_10kPa <- subst(stack_10kPa, -32768, NA)
    stack_10kPa <- subst(stack_10kPa, 0, NA)
    stack_1500kPa <- subst(stack_1500kPa, -32768, NA)
    stack_1500kPa <- subst(stack_1500kPa, 0, NA)
    
    # water °/oo (Field capacity) - water °/oo (Wilting Point) / 0.6 meters
    delta <- (stack_10kPa - stack_1500kPa)/0.6
    # save difference in tile list
    tile_list[[length(tile_list)+1]] <- delta #here crash
  }
  
  # merge all tiles
  rsrc <- sprc(tile_list)
  merged_waterstorage <- merge(rsrc)
  # return
  return(merged_waterstorage)
}

# retrieves stacked soildata
water_storage_stack <- read_soildata(path_soilGrids, c(1:14))

# average out nodata
water_storage_stack_avg <- focal(water_storage_stack,
                                 w = generate_normal_matrix(31),
                                 fun = "mean", na.policy="only", na.rm=TRUE)

# sum up with repective layer depth /1000 because the water content in°/oo
water_storage_sum_avg <-(
  water_storage_stack_avg[[1]] * 5 +
  water_storage_stack_avg[[2]] * 10 +
  water_storage_stack_avg[[3]] * 15 +
  water_storage_stack_avg[[4]] * 30
  )/60

# save those rasters
writeRaster(water_storage_stack_avg,
            paste0(path_soilGrids, "water_storage_stack.tif"), overwrite=TRUE)
writeRaster(water_storage_sum_avg,
            paste0(path_soilGrids, "water_storage_sum.tif"), overwrite=TRUE)

# project to study area
water_storage_stack_avg_proj <- project(water_storage_stack_avg, mask_StudyRegion)
water_storage_sum_avg_proj <- project(water_storage_sum_avg, mask_StudyRegion)
names(water_storage_stack_avg_proj) <- c("wvol_05", "wvol_15", "wvol_30", "wvol_60")
names(water_storage_sum_avg_proj) <- c("wvol")

# save projected rasters
writeRaster(water_storage_stack_avg_proj,
            paste0(path_soilGrids, "water_storage_stack_proj.tif"), overwrite=TRUE)
writeRaster(water_storage_sum_avg_proj,
            paste0(path_soilGrids, "water_storage_sum_proj.tif"), overwrite=TRUE)

# load projected rasters
water_storage_stack_avg_proj <- rast(paste0(path_ArcticDEM_32m, "water_storage_stack_proj.tif"))
water_storage_sum_avg_proj <- rast(paste0(path_ArcticDEM_32m, "water_storage_sum_proj.tif"))


## apply masks ####
# to stack
water_storage_stack_masked_full <- mask(x=water_storage_stack_avg_proj, mask = mastermask, maskvalues=0, updatevalue=NA)
water_storage_stack_masked_r100 <- mask(x=water_storage_stack_avg_proj, mask = mastermask_r100, maskvalues=0, updatevalue=NA)
water_storage_stack_masked_crop <- mask(x=water_storage_stack_avg_proj, mask = mastermask_crop, maskvalues=0, updatevalue=NA)

# save stack

soil_masterlist = c("wvol05", "wvol15", "wvol30", "wvol60")

for (i in c(1:4)){
  varname <- soil_masterlist[i]
  writeRaster(water_storage_stack_masked_full[[i]],
              paste0(path_gjamTime_data, "soil_const_", varname, "_full.tif"),
              overwrite=TRUE)
  writeRaster(water_storage_stack_masked_r100[[i]],
              paste0(path_gjamTime_data, "soil_const_", varname, "_r100.tif"),
              overwrite=TRUE)
  writeRaster(water_storage_stack_masked_crop[[i]],
              paste0(path_gjamTime_data, "soil_const_", varname, "_crop.tif"),
              overwrite=TRUE)
}


# to sum
water_storage_sum_masked_full <- mask(x=water_storage_sum_avg_proj, mask = mastermask, maskvalues=0, updatevalue=NA)
water_storage_sum_masked_r100 <- mask(x=water_storage_sum_avg_proj, mask = mastermask_r100, maskvalues=0, updatevalue=NA)
water_storage_sum_masked_crop <- mask(x=water_storage_sum_avg_proj, mask = mastermask_crop, maskvalues=0, updatevalue=NA)

# save sum
writeRaster(water_storage_sum_masked_full,
            paste0(path_gjamTime_data, "soil_const_", "wvol", "_full.tif"),
            overwrite=TRUE)
writeRaster(water_storage_sum_masked_r100,
            paste0(path_gjamTime_data, "soil_const_", "wvol", "_r100.tif"),
            overwrite=TRUE)
writeRaster(water_storage_sum_masked_crop,
            paste0(path_gjamTime_data, "soil_const_", "wvol", "_crop.tif"),
            overwrite=TRUE)


## STOCKER SOILDATA ####
soilstocker <- rast(file.path(path_stockersoil, "testStocker_cutProjection.tif"))
# average out nodata
soilstocker <- focal(soilstocker,
                      w = generate_normal_matrix(21),
                    fun = "mean", na.policy="only", na.rm=TRUE)
# project
soilstocker <- project(soilstocker, mask_StudyRegion)
soilstocker <- mask(soilstocker, mastermask, maskvalues=0, updatevalue=NA)
soilstocker_crop <- mask(soilstocker, mastermask_crop , maskvalues=0, updatevalue=NA)

writeRaster(soilstocker,
            paste0(path_gjamTime_data, "soil_const_", "scwd", "_full.tif"),
            overwrite=TRUE)
writeRaster(soilstocker_crop,
            paste0(path_gjamTime_data, "soil_const_", "scwd", "_crop.tif"),
            overwrite=TRUE)
