library(here)
# Set working directory only if not already set
setwd(here::here())

source("config/config_local.R")
source("scripts/core/2_analysis/jacobian_geospatial.R")

arg <- "test_higherorder1_alphaNeg1"

## call fixpt_geospatial()
jacobian <- jacobian_geospatial(argument = arg, n_chunks = 2, regular = TRUE,
                                inverse = TRUE)
