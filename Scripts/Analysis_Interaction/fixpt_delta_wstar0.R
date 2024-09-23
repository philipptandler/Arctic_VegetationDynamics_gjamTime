# this produces a map of the paramater di, s.th. wi = wi2020 + di(wi2100-wi2020) = 0

#set up environment:
library(here)
setwd(here::here())
source("Scripts/Analysis_Interaction/analysisHfunctions.R")

wstar_2020 <- rast(file.path(path_analysis_data_rast, "wstar_2020_nontriv.tif"))
wdelta <- rast(file.path(path_analysis_data_rast, "wdelta.tif"))


n_chunks <- 100
chunk_size <- 160 #multiplies to dim(raster)[1]

for(pos in 1:n_chunks){
  cat("processing", pos, "out of", n_chunks, "chunks.\n")
  # subset boundaries
  xmin <- (pos-1)*chunk_size+1
  xmax <- min(pos*chunk_size, X_DIM_RASTER)
  ymin <- 1
  ymax <- Y_DIM_RASTER
  subset <- c(xmin, xmax, ymin, ymax)
  # do subset
  wstar_2020_subs <- wstar_2020[subset[3]:subset[4], subset[1]:subset[2], drop = F]
  wdelta_subs <- wdelta[subset[3]:subset[4], subset[1]:subset[2], drop = F]
  
  delta0_subs <- -100*wstar_2020_subs/wdelta_subs
  writeRaster(delta0_subs, file.path(path_analysis_chunkprocesses, paste0("delta_zero_",pos,".tif")), datatype = "INT2S")
  
}

delta_chunks_list <- list.files(path = path_analysis_chunkprocesses,
                                pattern = "delta_zero_*",
                                full.names = TRUE)
rasters <- lapply(delta_chunks_list, rast)

merged_raster <- do.call(mosaic, rasters)
writeRaster(merged_raster,
            file.path(path_analysis_data_rast,
                      "delta_zero.tif"),
            datatype = "INT2S")