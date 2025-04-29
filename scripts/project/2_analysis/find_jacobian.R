library(here)
# Set working directory only if not already set
setwd(here::here())

source("config/config_local.R")
source("scripts/core/2_analysis/jacobian_geospatial.R")

arg <- "probe1_base_nf"
# arg <- "test_higherorder1_alphaNeg1"

## call jacobian_geospatial()
jacobian <- jacobian_geospatial(argument = arg, n_chunks = 100, regular = TRUE,
                                inverse = FALSE,
                                output_mask = file.path(path_masks, "wildfire_mask_1978-2014.tif"))
