## sourcing general scripts ####
source("scripts/core/2_analysis/.analysis_Hfunctions.R")

fixpt <- function(rho, alpha, x, ...){
  
  wstar <- .fixpt(beta = NULL, rho = rho, alpha = alpha, x = x,
                  ...)
  
  wstar
}