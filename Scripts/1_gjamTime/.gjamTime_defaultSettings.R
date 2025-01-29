.default_call <- function(){
  ## these are the default parameters
  def_name <- "gjamTime_model"
  
  def_yvars <- FALSE 
  def_xvars <- FALSE
  def_periods <- FALSE
  def_version <- FALSE
  
  def_subset <- FALSE
  def_subsample <- FALSE
  
  # model
  def_model <- list(
    termB = FALSE,
    termR = TRUE,
    termA = TRUE
  )
  
  ## priors
  def_betaPrior <- list(
    intercept = list(
      lo = -100,
      hi = 100
    ),
    variables = list(
      lo = -100,
      hi = 100
    )
  )
  def_rhoPrior <- list(
    intercept = list(
      lo = -2,
      hi = 2
    ),
    variables = list(
      lo = -100,
      hi = 100
    )
  )
  def_alphaPrior <- list(
    alphaSign = -1
  )

  def_priorList <- list(
    beta = def_betaPrior,
    rho = def_rhoPrior,
    alpha = def_alphaPrior
  )
  # model
  def_modelRunntime <- list(
    ng = 100,
    burnin = 50
  )
  
  default <- list(
    name = def_name,
    yvars = def_yvars,
    xvars = def_xvars,
    periods = def_periods,
    version = def_version,
    subset = def_subset,
    subsample = def_subsample,
    model = def_model,
    priorList = def_priorList,
    modelRunntime = def_modelRunntime
  )
  return(default)
}

# depends on function in .gjamTime_Hfunctions
.default_vars <- function(){
  default_vars <- list()
  valid_vars <- .receive_validVariables()
  
  for(name in names(valid_vars$variables)){
    default_vars[[name]] = FALSE
  }
  default_vars$x <- FALSE
  default_vars$y <- FALSE
  default_vars$interaction <- FALSE
  return(default_vars)
}

.default_subset <- function(){
  subset <- list(
    doSubset <- TRUE,
    mask <- NULL
  )
  return(subset)
}

.default_subsample <- function(){
  subsample <- list(
    doSubsample <- TRUE,
    mode <- "random",
    parameter <- 100, 
    seed <- 1
  )
  return(subsample)
}