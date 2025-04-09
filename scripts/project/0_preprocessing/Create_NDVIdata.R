library(here)
setwd(here::here())
library(terra)

##' source:
##' Ju, J., and J.G. Masek. 2018. ABoVE: NDVI Trends across Alaska and Canada
##' from Landsat, 1984-2012. ORNL DAAC, Oak Ridge, Tennessee, USA.
##' https://doi.org/10.3334/ORNLDAAC/1576

## paths and names
path_NDVI_raw <- "data/NDVI_trends_Ju/raw"
path_ndvi_save <- "data/NDVI_trends_Ju"
pattern_ndvi_trend <- "*trend_Ah*"
pattern_ndvi_sig <- "*trend_sig*"
name_ndvi_trend <- "ndvi_trend"
name_ndvi_sig <- "ndvi_sig"
name_ndvi_abs_trend <- "ndvi_abs_trend"
name_ndvi_abs_sig <- "ndvi_abs_sig"

## master mask
mastermask <- rast("data/masks/master_mask.tif")

## classify
#' according to datasource it -10000 are NA, 10000 water and 10001 ice. But this 
#' is only true for _sig. The normal trend is also in "INT2S", but presented as floating point
reclass_matrix_trend <- matrix(c(-10001,-0.01, NA,   # map nodata -> NA
                                 0.01,10002, NA),  # map water and snow/ice -> NA
                               ncol = 3, byrow = TRUE)
reclass_matrix_sig <- matrix(c(-10000, NA,   # map nodata -> NA
                               10000, NA,  # map water(10000) -> NA
                               10001, NA),  # map snow/ice(10000) -> NA
                             ncol = 3, byrow = TRUE)


## list with both data
ndvi_list <- list(
  "trend" = list(pattern = pattern_ndvi_trend,
                 name = name_ndvi_trend,
                 name_abs = name_ndvi_abs_trend,
                 reclass_matrix = reclass_matrix_trend),
  "sig" = list(pattern = pattern_ndvi_sig,
               name = name_ndvi_sig,
               name_abs = name_ndvi_abs_sig,
               reclass_matrix = reclass_matrix_sig)
)

## process to raw raster
for(name in names(ndvi_list)){
  ## load files
  ndvi_list[[name]]$f_list <- list.files(path = path_NDVI_raw,
                                         pattern = ndvi_list[[name]]$pattern,
                                         full.names = TRUE)
  ## merge files
  ndvi_list[[name]]$r_list <- lapply(ndvi_list[[name]]$f_list, rast)
  ## mosaic rasters
  ndvi_list[[name]]$raster_raw <- do.call(mosaic,ndvi_list[[name]]$r_list)
}


## classify

for(name in names(ndvi_list)){
  reclass_matrix_this <- ndvi_list[[name]]$reclass_matrix
  ndvi_list[[name]]$raster_raw <- classify(ndvi_list[[name]]$raster_raw, reclass_matrix_this)
}

#workarouond for "sig"
mask_NA <- ndvi_list[["sig"]]$raster_raw < 10
ndvi_list[["sig"]]$raster_raw <- mask(ndvi_list[["sig"]]$raster_raw , mask_NA,
                                      maskvalues=0, updatevalue=NA)

## project and mask and save
for(name in names(ndvi_list)){
  ndvi_list[[name]]$raster_proj <- project(ndvi_list[[name]]$raster_raw,mastermask)
  ndvi_list[[name]]$raster_proj <- mask(ndvi_list[[name]]$raster_proj, mastermask,
                                        maskvalues = 0, updatevalue = NA)
  writeRaster(ndvi_list[[name]]$raster_proj,
              file.path(path_ndvi_save, paste0(ndvi_list[[name]]$name, ".tif")))
  ndvi_list[[name]]$raster_abs <- abs(ndvi_list[[name]]$raster_proj)
  writeRaster(ndvi_list[[name]]$raster_abs,
              file.path(path_ndvi_save, paste0(ndvi_list[[name]]$name_abs, ".tif")))
}

## mask fires
# load
ndvi_trend <- rast(file.path(path_ndvi_save, paste0(name_ndvi_trend, ".tif")))
ndvi_abs_trend <- rast(file.path(path_ndvi_save, paste0(name_ndvi_abs_trend, ".tif")))
ndvi_sig <- rast(file.path(path_ndvi_save, paste0(name_ndvi_sig, ".tif")))
ndvi_abs_sig <- rast(file.path(path_ndvi_save, paste0(name_ndvi_abs_sig, ".tif")))
# mask
wildfire_mask_1990_2012 <- rast("data/CanadianNationalFireDataBase/wildfire_mask_1970-2012.tif")
ndvi_trend <- mask(ndvi_trend, wildfire_mask_1990_2012, maskvalues=1, updatevalue=NA)
ndvi_abs_trend <- mask(ndvi_abs_trend, wildfire_mask_1990_2012, maskvalues=1, updatevalue=NA)
ndvi_sig <- mask(ndvi_sig, wildfire_mask_1990_2012, maskvalues=1, updatevalue=NA)
ndvi_abs_sig <- mask(ndvi_abs_sig, wildfire_mask_1990_2012, maskvalues=1, updatevalue=NA)
#write
writeRaster(ndvi_trend, file.path(path_ndvi_save, paste0(name_ndvi_trend, "_nofire.tif")))
writeRaster(ndvi_abs_trend, file.path(path_ndvi_save, paste0(name_ndvi_abs_trend, "_nofire.tif")))
writeRaster(ndvi_sig, file.path(path_ndvi_save, paste0(name_ndvi_sig, "_nofire.tif")))
writeRaster(ndvi_abs_sig, file.path(path_ndvi_save, paste0(name_ndvi_abs_sig, "_nofire.tif")))