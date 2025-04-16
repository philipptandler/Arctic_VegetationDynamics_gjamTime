library(here)
# Set working directory only if not already set
setwd(here::here())

source("config/config_local.R")
source("scripts/core/2_analysis/jacobian_geospatial.R")

arg <- "probe1_base"

## call fixpt_geospatial()
jacobian <- jacobian_geospatial(argument = arg, n_chunks = 100, regular = TRUE, w="w_obs",
                                inverse = FALSE)
