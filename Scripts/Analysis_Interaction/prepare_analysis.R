#' This Scirpt prepares the matrices (the parameters from gjam) and rasters
#' (the predictors, thought of the vectors in the model)


#set up environment:
library(here)
setwd(here::here())
source("Scripts/Analysis_Interaction/analysisHfunctions.R")


## prepare parameter estimations ####
# estimates alpha (Mu, Se) and rho (Mu & Se)
estimates_all <- load_estimates_gjam(folderPattern = "gjam_interaction_full_subs100_[0-9]{4}",
                                     directory = path_gjamTime_outputs,
                                     save = TRUE)

alphaMu <- estimates_all$alphaMu
alphaSe <- estimates_all$alphaSe
rhoMu <- estimates_all$rhoMu
rhoSe <- estimates_all$rhoSe


## prepare rasters ####
# wobs_1990 (sh, cf, hb, lc) in this order!
files_wobs1990 <- c("data/gjamTime_data/veg_1984-1990_sh_full.tif",
                    "data/gjamTime_data/veg_1984-1990_cf_full.tif",
                    "data/gjamTime_data/veg_1984-1990_hb_full.tif",
                    "data/gjamTime_data/veg_1984-1990_lc_full.tif")
wobs_1990 <- rast(files_wobs1990)
writeRaster(wobs_1990, filename = file.path(path_analysis_data_rast, "wobs_1990.tif"))

# wobs_2020 (sh, cf, hb, lc) in this order!
files_wobs2020 <- c("data/gjamTime_data/veg_2015-2020_sh_full.tif",
                  "data/gjamTime_data/veg_2015-2020_cf_full.tif",
                  "data/gjamTime_data/veg_2015-2020_hb_full.tif",
                  "data/gjamTime_data/veg_2015-2020_lc_full.tif")
wobs_2020 <- rast(files_wobs2020)
writeRaster(wobs_2020, filename = file.path(path_analysis_data_rast, "wobs_2020.tif"))

## get x vector to predict current and future steadystates
#' select rasters for respective variables used in the order of rho() from gjam.
#' currently for lat lon as predictors not available 

## start from x_1990 in data/analysis
# #load known data
# x_1990 <- rast("data/analysis/rasters/x_1990.tif")
# x_1990[["wvol"]] <- rast("data/gjamTime_data/soil_const_scwd_full.tif")
# x_names <- names(x_1990)
# x_names[which(x_names == "wvol")] <- "scwd"
# names(x_1990) <- x_names
# path_ref_list_fullname <- paste0(path_norm_list, name_norm_list)
# ref_list <- readRDS(path_ref_list_fullname)
# x_1990[["scwd"]] <- (x_1990[["scwd"]]-ref_list$scwd$mean)/ref_list$scwd$sd
# x_1990 <- WriteAndLoad(x_1990, "x_1990", path = path_analysis_data_rast, datatype = "FLT4S")


## start from scratch
# x_1990
filePred1990 <- c("data/gjamTime_data/topo_const_elev_full.tif",
                  "data/gjamTime_data/topo_const_slope_full.tif",
                  "data/gjamTime_data/topo_const_cosasp_full.tif",
                  "data/gjamTime_data/topo_const_tpi_full.tif",
                  "data/gjamTime_data/soil_const_scwd_full.tif",
                  "data/gjamTime_data/clim_1984-1990_tasw_full.tif",
                  "data/gjamTime_data/clim_1984-1990_tass_full.tif",
                  "data/gjamTime_data/clim_1984-1990_prw_full.tif",
                  "data/gjamTime_data/clim_1984-1990_prs_full.tif")

# load and normalize variable rasters
x_1990 <- rast(filePred1990)
names(x_1990) <- colnames(rhoMu)[2:10]
x_1990 <- normalizeRaster(x_1990)

# load intercept layer
intercept_layer <- rast(x_1990, nlyrs=1)
names(intercept_layer) <- colnames(rhoMu)[1]
values(intercept_layer) <- 1

# unite and write
x_1990 <- c(intercept_layer, x_1990)
names(x_1990) <- colnames(rhoMu)
writeRaster(x_1990, filename = file.path(path_analysis_data_rast, "x_1990.tif"),
            overwrite = TRUE)

## calculate products
# add product layer
for(pair in colnames(rhoMu)[11:20]){
  vars <- unlist(strsplit(pair, ":"))
  x_1990[[pair]] <- x_1990[[vars[1]]]*x_1990[[vars[2]]]
}
# save
x_1990 <- WriteAndLoad(x_1990, "x_1990", path = path_analysis_data_rast, datatype = "FLT4S")


## start from x_2020 in data/analysis
# #load known data
# x_2020 <- rast("data/analysis/rasters/x_2020.tif")
# x_2020[["wvol"]] <- rast("data/gjamTime_data/soil_const_scwd_full.tif")
# x_names <- names(x_2020)
# x_names[which(x_names == "wvol")] <- "scwd"
# names(x_2020) <- x_names
# path_ref_list_fullname <- paste0(path_norm_list, name_norm_list)
# ref_list <- readRDS(path_ref_list_fullname)
# x_2020[["scwd"]] <- (x_2020[["scwd"]]-ref_list$scwd$mean)/ref_list$scwd$sd
# x_2020 <- WriteAndLoad(x_2020, "x_2020", path = path_analysis_data_rast, datatype = "FLT4S")


## start from scratch
# x_2020
filePred2020 <- c("data/gjamTime_data/topo_const_elev_full.tif",
                  "data/gjamTime_data/topo_const_slope_full.tif",
                  "data/gjamTime_data/topo_const_cosasp_full.tif",
                  "data/gjamTime_data/topo_const_tpi_full.tif",
                  "data/gjamTime_data/soil_const_scwd_full.tif",
                  "data/gjamTime_data/clim_2015-2020_tasw_full.tif",
                  "data/gjamTime_data/clim_2015-2020_tass_full.tif",
                  "data/gjamTime_data/clim_2015-2020_prw_full.tif",
                  "data/gjamTime_data/clim_2015-2020_prs_full.tif")

# load and normalize variable rasters
x_2020 <- rast(filePred2020)
names(x_2020) <- colnames(rhoMu)[-1]
x_2020 <- normalizeRaster(x_2020)
# load intercept layer
intercept_layer <- rast(x_2020, nlyrs=1)
names(intercept_layer) <- colnames(rhoMu)[1]
values(intercept_layer) <- 1
# unite and write
x_2020 <- c(intercept_layer, x_2020)
writeRaster(x_2020, filename = file.path(path_analysis_data_rast, "x_2020.tif"),
            overwrite = TRUE)

## calculate products
# add product layer
for(pair in colnames(rhoMu)[11:20]){
  vars <- unlist(strsplit(pair, ":"))
  x_2020[[pair]] <- x_2020[[vars[1]]]*x_2020[[vars[2]]]
}
x_2020 <- WriteAndLoad(x_2020, "x_2020", path = path_analysis_data_rast, datatype = "FLT4S")


## start from x_2100 in data/analysis
# #load known data
# x_2100 <- rast("data/analysis/rasters/x_2100.tif")
# x_2100[["wvol"]] <- rast("data/gjamTime_data/soil_const_scwd_full.tif")
# x_names <- names(x_2100)
# x_names[which(x_names == "wvol")] <- "scwd"
# names(x_2100) <- x_names
# path_ref_list_fullname <- paste0(path_norm_list, name_norm_list)
# ref_list <- readRDS(path_ref_list_fullname)
# x_2100[["scwd"]] <- (x_2100[["scwd"]]-ref_list$scwd$mean)/ref_list$scwd$sd
# x_2100 <- WriteAndLoad(x_2100, "x_2100", path = path_analysis_data_rast, datatype = "FLT4S")


# x_2100
## start from scratch
filePred2100 <- c("data/gjamTime_data/topo_const_elev_full.tif",
                  "data/gjamTime_data/topo_const_slope_full.tif",
                  "data/gjamTime_data/topo_const_cosasp_full.tif",
                  "data/gjamTime_data/topo_const_tpi_full.tif",
                  "data/gjamTime_data/soil_const_wvol_full.tif",
                  "data/gjamTime_data/clim_2071-2100_tasw_full.tif",
                  "data/gjamTime_data/clim_2071-2100_tass_full.tif",
                  "data/gjamTime_data/clim_2071-2100_prw_full.tif",
                  "data/gjamTime_data/clim_2071-2100_prs_full.tif")

# load and normalize variable rasters
x_2100 <- rast(filePred2100)
names(x_2100) <- colnames(rhoMu)[-1]
x_2100 <- normalizeRaster(x_2100)
# load intercept layer
intercept_layer <- rast(x_2100, nlyrs=1)
names(intercept_layer) <- colnames(rhoMu)[1]
values(intercept_layer) <- 1
# unite and write
x_2100 <- c(intercept_layer, x_2100)
writeRaster(x_2100, filename = file.path(path_analysis_data_rast, "x_2100.tif"),
            overwrite = TRUE)


## calculate products
# add product layer
for(pair in colnames(rhoMu)[11:20]){
  vars <- unlist(strsplit(pair, ":"))
  x_2100[[pair]] <- x_2100[[vars[1]]]*x_2100[[vars[2]]]
}
x_2100 <- writeRaster(x_2100, filename = file.path(path_analysis_data_rast, "x_2100.tif"), overwrite = TRUE, datatype = "FLT4S")


# create dummylayer with 0 in mastermask and NA outside
zero_layer <- !rast("data/Masks/master_mask.tif")
zero_layer <- mask(zero_layer, zero_layer, maskvalue=TRUE, updatevalue=NA)
zero_layer <- WriteAndLoad(zero_layer, "zero_layer", path_analysis_data_rast)
