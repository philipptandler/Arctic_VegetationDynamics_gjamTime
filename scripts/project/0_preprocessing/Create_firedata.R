library(here)
setwd(here::here())
library(terra)


##Prepare paths and masks ####

path_firedata_In<- "data/CanadianNationalFireDataBase/fire_polygons_all"
path_firedata_Out<- "data/CanadianNationalFireDataBase"
name_wildfire_in <- "NFDB_poly_20210707.shp"
name_wildfire_out <- "wildfire_mask"

# Read masks
mask_StudyRegion <- rast("data/masks/study_region_mask.tif")
mastermask <- rast("data/masks/master_mask.tif")

## parameter
selectTimeperiod <- TRUE
cutOffYear <- 1978
cutInYear <- 2020


## process polygon layer ####
# read shapefile
wildfire_perimeters <- vect(file.path(path_firedata_In, name_wildfire_in))

# project to CRS used
wildfire_perimeters <- project(wildfire_perimeters, crs(mask_StudyRegion))
wildfire_perimeters <- crop(wildfire_perimeters, mask_StudyRegion)

# select wildfires
if(selectTimeperiod){
  wildfire_perimeters <- wildfire_perimeters[wildfire_perimeters$YEAR >= cutOffYear,]
  wildfire_perimeters <- wildfire_perimeters[wildfire_perimeters$YEAR <= cutInYear,]
}

# rasterize
wildfire_raster <- rasterize(wildfire_perimeters, mask_StudyRegion, field=0,
                             background = 1, touches=TRUE)
# mask with mastermask to have NA (no data, i.e. water, glacier, ...), FALSE (no fire) or TRUE (fire)
combined_mask <- mask(wildfire_raster, mastermask, maskvalues=0, updatevalue=NA)

# write Raster
writeRaster(combined_mask, file.path(path_firedata_Out, paste0(name_wildfire_out,
                                                           "_", cutOffYear,
                                                           "-", cutInYear,
                                                           ".tif")),
            overwrite = T)




