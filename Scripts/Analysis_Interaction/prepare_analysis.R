#' This Scirpt prepares the matrices (the parameters from gjam) and rasters
#' (the predictors, thought of the vectors in the model)


#set up environment:
library(here)
setwd(here::here())
source("Scripts/Analysis/analysisHfunctions.R")


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

#load known data
x_1990 <- rast("data/analysis/rasters/x_1990.tif")
x_1990[["wvol"]] <- rast("data/gjamTime_data/soil_const_scwd_full.tif")
x_names <- names(x_1990)
x_names[which(x_names == "wvol")] <- "scwd"
names(x_1990) <- x_names
path_ref_list_fullname <- paste0(path_norm_list, name_norm_list)
ref_list <- readRDS(path_ref_list_fullname)
x_1990[["scwd"]] <- (x_1990[["scwd"]]-ref_list$scwd$mean)/ref_list$scwd$sd

# load and normalize variable rasters
x_1990 <- rast(filePred1990)
names(x_1990) <- colnames(rhoMu)[2:10]
x_1990 <- normalizeRaster(x_1990)

# load intercept layer
intercept_layer <- rast(x_1990, nlyrs=1)
names(intercept_layer) <- colnames(rhoMu)[1]
values(intercept_layer) <- 1

# add product layer
for(pair in colnames(rhoMu)[11:20]){
  vars <- unlist(strsplit(pair, ":"))
  x_1990[[pair]] <- x_1990[[vars[1]]]*df[[vars[2]]]
}
# unite and write
x_1990 <- c(intercept_layer, x_1990)
names(x_1990) <- colnames(rhoMu)
writeRaster(x_1990, filename = file.path(path_analysis_data_rast, "x_1990.tif"),
            overwrite = TRUE)
# to make x_1990 for analysis inter
x_1990 <- rast("data/analysis/rasters/x_1990.tif")
x_1990 <- subset(x_1990, c(1:5, 7:10)) #except "wvol"
x_1990[["scwd"]] <- rast("data/gjamTime_data/soil_const_scwd_full.tif")
path_ref_list_fullname <- paste0(path_norm_list, name_norm_list)
ref_list <- readRDS(path_ref_list_fullname)

x_1990[["scwd"]] <- x_1990[["scwd"]]- 


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


# x_2100
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

# create dummylayer with 0 in mastermask and NA outside
zero_layer <- !rast("data/Masks/master_mask.tif")
zero_layer <- mask(zero_layer, zero_layer, maskvalue=TRUE, updatevalue=NA)
zero_layer <- WriteAndLoad(zero_layer, "zero_layer", path_analysis_data_rast)
