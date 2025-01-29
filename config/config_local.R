#' set paths here relative to root directory (where .Rproj sits)

## data ####
# gjamTime
path_data <- "data"

path_gjamTime_in <- "data/gjamTime/in"
path_gjamTime_tmp <- "data/gjamTime/tmp"
path_gjamTime_out <- "data/gjamTime/out"

name_gjamTime_output <- "output.Rdata"

# masks
#' the master mask serves as template for the target CRS, extent, resolution etc.
#' it serves as template for the study region. 
#' The values are only 0 (FALSE) or 1 (TRUE). Pixels with 0 (FALSE) are excluded
#' from the model fitting. This might be because they are outside of the study
#' area, or represents unusable data (such as water, infrastructure, ...). The 
#' mastermask is the main filter for what pixels are considered in the study and
#' what are excluded
path_masks <- "data/Masks"
name_master_mask <- "master_mask.tif"

## scripts ####
path_gjamTime_validVariables <- "config/gjamTime_validVariables.R"

## analysis ####
# paths
path_analysis <- "analysis"

# files


