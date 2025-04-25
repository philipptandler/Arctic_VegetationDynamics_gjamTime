#' choose a representative model name. This name will be used for creating output 
#' folders in path_gjam_out and in path_analysis
#' The only strictly required entry is yvars (the response variable)

name <- "probe1_base_nf_shrt"

# continue <- "e4ca783bd375"

# use the same names as in the list "var_list" in config/set_gjam_variables.R
yvars <- list(
  vegetation = c("sh", "cf", "hb", "lc")
)

# use the same names as in the list "var_list"  in config/set_gjam_variables.R
#' y, x can be added predictor variables for longitude and latitude, assuming
#' the y direction of the raster equals north-south and the x direction equals
#' west-east
#' Also, interaction variables can be specified. Either between two variables
#' (e.g. elev:slope) or as higher order terms (e.g. tass2)
#' ! Important, all interaction variables must exist as single variables too

xvars <- list(
  topography = c("elev", "slope", "cosasp", "tpi"),
  y = FALSE, # to get latitude
  x = FALSE, # to get longitude
  climate = c("prs", "prw", "tass", "tasw"),
  soil = c("scwd"),
  interaction =  c("elev:slope", "elev:cosasp", "elev:tpi", "slope:tpi", 
                   "elev:tass", "tass:prs", "tass:prw",
                   "scwd:elev", "scwd:prs", "scwd:prw") 
)

#' what time periods are considered for this model fit 
times <- c("1984-1990",
           "1991-1996",
           "1997-2002",
           "2003-2008",
           "2009-2014")

# what version of the data is used
version <- "full"

# if a specific area within the study area is selected,
# if TRUE, specify what mask should be used to subset in path_masks

#' Use a subsample within the study area to fit the model, highly recommended! 
#' Either sub sample the points on a regular grid or random
#' if mode = "regular", 'parameter' sets distance of grid
#' if mode = "random", parameter sets sample size
subsample <- list(
  doSubsample = TRUE, #recommended TRUE: FALSE might crash due to memory overflow
  mode = "random",
  # if 'regular' distance between grid points, if 'random' number of sub samples
  size = 10e3,
  seed = 1 # sets a seed for the first run, will change for further sub samples
)

subset <- list(
  doSubset = TRUE, #if TRUE, set mask for subset
  mask = "wildfire_mask_1978-2014.tif"
)


#' what model to choose, see gjamTime vignette
#' termB = movement
#' termR = density independent growth
#' termA = density dependent growth
model <- list(
  termB = FALSE,
  termR = TRUE,
  termA = TRUE
)

#' here set the list of priors for beta, rho, alpha.
#' For beta and rho, set intercept=list(lo=..., hi=...),
#' For alpha set either a value (for all interactions)
#' or a matrix of S x S, where S = the number of variables in yvars above

# set how many iterations MCMC runs, and how many are burned
modelRunntime <- list(
  ng = 1200,
  burnin = 600
)




