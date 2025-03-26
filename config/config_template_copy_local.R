#' set paths here relative to root directory (where .Rproj sits)

## data ####
# gjamTime
path_data <- "data"

path_gjamTime_in <- "data/gjamTime/in"
path_gjamTime_tmp <- "data/gjamTime/tmp"
path_gjamTime_out <- "data/gjamTime/out"

# masks
#' the master mask serves as template for the target CRS, extent, resolution etc.
#' it serves as template for the study region. 
#' The values are only 0 (FALSE) or 1 (TRUE). Pixels with 0 (FALSE) are excluded
#' from the model fitting. This might be because they are outside of the study
#' area, or represents unusable data (such as water, infrastructure, ...). The 
#' mastermask is the main filter for what pixels are considered in the study and
#' what are excluded
path_masks <- "data/masks"
name_master_mask <- "master_mask.tif"

## scripts ####
path_gjamTime_validVariables <- "config/gjamTime_validVariables.R"

## analysis ####
path_analysis <- "analysis"
path_analysis_norm_predictors <- "analysis/.library_normalized_predictors"
path_analysis_tmp <- "analysis/.tmp"

## make dir if not exist ####
dirs <- c(
  path_data,
  path_gjamTime_in,
  path_gjamTime_tmp,
  path_gjamTime_out,
  path_masks,
  path_analysis,
  path_analysis_norm_predictors,
  path_analysis_tmp
)
for(dir in dirs){
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
}