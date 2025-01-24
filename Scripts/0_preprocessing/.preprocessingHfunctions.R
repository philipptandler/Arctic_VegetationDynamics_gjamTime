library(here)
library(terra)
# Set working directory only if not already set
if (getwd() != here::here()) {
  setwd(here::here())
}

source("config/set_gjam_variables.R")
source("config/config_local.R")

mastermask <- rast(file.path(path_masks, name_master_mask))

# uses terra project ot preject to master mask
# use also any other method of terra::project
project_to_mastermask <- function(raster, method="bilinear"){
  r <- project(raster, mastermask)
  return(r)
}

# mask a raster with mastermask
mask_with_mastermask <- function(raster){
  r <- mask(raster, mastermask, maskvalues=0, updatevalue=NA)
  return(r)
}

