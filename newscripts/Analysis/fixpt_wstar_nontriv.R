# this Script writes the observed and predicted species raster w

#set up environment:
library(here)
setwd(here::here())
source("Scripts/Analysis/analysisHfunctions.R")

#load matrices
alpha <- readRDS(file.path(path_analysis_saveParameters, ".alphaMu.rds"))
rho <- readRDS(file.path(path_analysis_saveParameters, ".rhoMu.rds"))

## wstar 1990 ####
cat("calculate wstar_1990\n")
x_1990 <- rast(file.path(path_analysis_data_rast,"x_1990.tif"))
wstar_1990 <- matrixProd(-inv(alpha) %*% rho, x_1990) 
names(wstar_1990) <- rownames(rho)
writeRaster(wstar_1990,
            file.path(path_analysis_data_rast, "wstar_1990_nontriv.tif"),
            datatype = "INT2S", overwrite = TRUE)
wstar_1990 <- rast(file.path(path_analysis_data_rast, "wstar_1990_nontriv.tif"))


## negative values mask 
cat("calculate wstar_1990_nonneg\n")
mask_wstar_1990_nonneg <- (wstar_1990[[1]] >= 0 &
                             wstar_1990[[2]] >= 0 &
                             wstar_1990[[3]] >= 0 &
                             wstar_1990[[4]] >= 0)

writeRaster(mask_wstar_1990_nonneg,
            file.path(path_analysis_data_rast, "mask_wstar_1990_nonneg.tif"),
            overwrite =TRUE)


## wstar 2020 ####
cat("calculate wstar_2020\n")
x_2020 <- rast(file.path(path_analysis_data_rast,"x_2020.tif"))
wstar_2020 <- matrixProd(-inv(alpha) %*% rho, x_2020)
names(wstar_2020) <- rownames(rho)
writeRaster(wstar_2020,
            file.path(path_analysis_data_rast, "wstar_2020_nontriv.tif"),
            datatype = "INT2S", overwrite =TRUE)
wstar_2020 <- rast(file.path(path_analysis_data_rast, "wstar_2020_nontriv.tif"))


## negative values mask
cat("calculate wstar_2020_nonneg\n")
mask_wstar_2020_nonneg <- (wstar_2020[[1]] >= 0 &
                             wstar_2020[[2]] >= 0 &
                             wstar_2020[[3]] >= 0 &
                             wstar_2020[[4]] >= 0)

writeRaster(mask_wstar_2020_nonneg,
            file.path(path_analysis_data_rast, "mask_wstar_2020_nonneg.tif"),
            overwrite =TRUE)

## wstar 2100 ####
cat("calculate wstar_2100\n")
x_2100 <- rast(file.path(path_analysis_data_rast,"x_2100.tif"))
wstar_2100 <- matrixProd(-inv(alpha) %*% rho, x_2100)
names(wstar_2100) <- rownames(rho)
writeRaster(wstar_2100,
            file.path(path_analysis_data_rast, "wstar_2100_nontriv.tif"),
            datatype = "INT2S", overwrite = TRUE)
wstar_2100 <- rast(file.path(path_analysis_data_rast, "wstar_2100_nontriv.tif"))

## negative values mask
cat("calculate wstar_2100_nonneg\n")
mask_wstar_2100_nonneg <- (wstar_2100[[1]] >= 0 &
                              wstar_2100[[2]] >= 0 &
                              wstar_2100[[3]] >= 0 &
                              wstar_2100[[4]] >= 0)
writeRaster(mask_wstar_2100_nonneg,
            file.path(path_analysis_data_rast, "mask_wstar_2100_nonneg.tif"), overwrite = TRUE)


## differences ####
wdelta <- wstar_2100 - wstar_2020
wdelta <- WriteAndLoad(wdelta, "wdelta", path = path_analysis_data_rast, datatype = "INT2S")
