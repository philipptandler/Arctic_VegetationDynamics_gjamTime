.default_call <- function(){
  ## these are the default parameters
  def_name <- "gjamTime_model"
  def_continue <- FALSE
  def_outfolderBase <- FALSE
  def_outfolderSub <- FALSE
  def_task_id <- 1
  
  def_yvars <- FALSE 
  def_xvars <- FALSE
  def_times <- FALSE
  def_version <- .receive_validVariables()$versions[1]
  
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

  def_priorSettings <- list(
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
    continue = def_continue,
    outfolderBase = def_outfolderBase,
    outfolderSub = def_outfolderSub,
    task_id = def_task_id,
    yvars = def_yvars,
    xvars = def_xvars,
    times = def_times,
    version = def_version,
    subset = def_subset,
    subsample = def_subsample,
    model = def_model,
    priorSettings = def_priorSettings,
    modelRunntime = def_modelRunntime
  )
  default
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
  default_vars$times <- FALSE
  default_vars$version <- FALSE
  default_vars
}

.default_subsample <- function(){
  subsample <- list(
    doSubsample = TRUE,
    mode = "random",
    size = 100, 
    seed = 1
  )
  subsample
}

.default_output_size <- function(){
  list(
    saveOutput = 1e4,
    saveCallRDS = 1e4,
    saveCalltxt = 1e2
  )
}
