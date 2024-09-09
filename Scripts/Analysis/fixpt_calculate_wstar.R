# this Script writes the observed and predicted species raster w

#set up environment:
library(here)
setwd(here::here())
source("Scripts/Analysis/analysisHfunctions.R")

#load matrices
alpha <- readRDS(file.path(path_analysis_scripts, ".alphaMu.rds"))
rho <- readRDS(file.path(path_analysis_scripts, ".rhoMu.rds"))

# ## wstar 1990 ####
# x_1990 <- rast(file.path(path_analysis_data_rast,"x_1990.tif"))
# wstar_1990 <- matrixProd(-inv(alpha) %*% rho, x_1990)
# names(wstar_1990) <- rownames(rho)
# writeRaster(wstar_1990,
#             file.path(path_analysis_data_rast, "wstar_1990_nontriv.tif"), datatype = "INT2U")
# wstar_1990 <- rast(file.path(path_analysis_data_rast, "wstar_1990_nontriv.tif"))
# 
# 
# ## negative values mask ####
# mask_wstar_1990_nonneg <- (wstar_1990[[1]] >= 0 &
#                              wstar_1990[[2]] >= 0 &
#                              wstar_1990[[3]] >= 0 &
#                              wstar_1990[[4]] >= 0)
# 
# writeRaster(mask_wstar_1990_nonneg,
#             file.path(path_analysis_data_rast, "mask_wstar_1990_nonneg.tif"),
#             overwrite =TRUE)
# 
# mask_wstar_1990_sumneg <- ((wstar_1990[[1]] < 0) +
#                              (wstar_1990[[2]] < 0) +
#                              (wstar_1990[[3]] < 0) +
#                              (wstar_1990[[4]] < 0))
# writeRaster(mask_wstar_1990_sumneg,
#             file.path(path_analysis_data_rast, "mask_wstar_1990_sumneg.tif"), overwrite =TRUE)
# mask_wstar_1990_sumneg <- rast(file.path(path_analysis_data_rast, "mask_wstar_1990_sumneg.tif"))

## wstar 2020 ####
x_2020 <- rast(file.path(path_analysis_data_rast,"x_2020.tif"))
wstar_2020 <- matrixProd(-inv(alpha) %*% rho, x_2020)
names(wstar_2020) <- rownames(rho)
writeRaster(wstar_2020,
            file.path(path_analysis_data_rast, "wstar_2020_nontriv.tif"), datatype = "INT2U")
wstar_2020 <- rast(file.path(path_analysis_data_rast, "wstar_2020_nontriv.tif"))


# ## negative values mask ####
# mask_wstar_2020_nonneg <- (wstar_2020[[1]] >= 0 &
#                              wstar_2020[[2]] >= 0 &
#                              wstar_2020[[3]] >= 0 &
#                              wstar_2020[[4]] >= 0)
# 
# writeRaster(mask_wstar_2020_nonneg,
#             file.path(path_analysis_data_rast, "mask_wstar_2020_nonneg.tif"),
#             overwrite =TRUE)
# 
# mask_wstar_2020_sumneg <- ((wstar_2020[[1]] < 0) +
#                              (wstar_2020[[2]] < 0) +
#                              (wstar_2020[[3]] < 0) +
#                              (wstar_2020[[4]] < 0))
# writeRaster(mask_wstar_2020_sumneg,
#             file.path(path_analysis_data_rast, "mask_wstar_2020_sumneg.tif"), overwrite =TRUE)
# mask_wstar_2020_sumneg <- rast(file.path(path_analysis_data_rast, "mask_wstar_2020_sumneg.tif"))
# 
# ## wstar 2100 ####
# x_2100 <- rast(file.path(path_analysis_data_rast,"x_2100.tif"))
# wstar_2100 <- matrixProd(-inv(alpha) %*% rho, x_2100)
# names(wstar_2100) <- rownames(rho)
# writeRaster(wstar_2100,
#             file.path(path_analysis_data_rast, "wstar_2100_nontriv.tif"), datatype = "INT2U")
# wstar_2100 <- rast(file.path(path_analysis_data_rast, "wstar_2100_nontriv.tif"))
# 
# ## negative values mask ####
# mask_wstar_2100_nonneg <- (wstar_2100[[1]] >= 0 &
#                               wstar_2100[[2]] >= 0 &
#                               wstar_2100[[3]] >= 0 &
#                               wstar_2100[[4]] >= 0)
# writeRaster(mask_wstar_2100_nonneg,
#             file.path(path_analysis_data_rast, "mask_wstar_2100_nonneg.tif"), overwrite =TRUE)
# 
# mask_wstar_2100_sumneg <- ((wstar_2100[[1]] < 0) +
#                              (wstar_2100[[2]] < 0) +
#                              (wstar_2100[[3]] < 0) +
#                              (wstar_2100[[4]] < 0))
# writeRaster(mask_wstar_2100_sumneg,
#             file.path(path_analysis_data_rast, "mask_wstar_2100_sumneg.tif"), overwrite =TRUE)
# mask_wstar_2100_sumneg <- rast(file.path(path_analysis_data_rast, "mask_wstar_2100_sumneg.tif"))
# 
# ## difference 2020 to 2100 ####
# wobs_2020 <- rast(file.path(path_analysis_data_rast, "wobs_2020.tif"))
# wdiff_2020 <- wobs_2020 - wstar_2020
# writeRaster(wdiff_2020,
#             file.path(path_analysis_data_rast, "wdiff_2020.tif"))
