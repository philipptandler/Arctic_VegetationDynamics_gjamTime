## sourcing general scripts ####
source("scripts/core/2_analysis/.analysis_Hfunctions.R")

jacobian <- function(rho, alpha, x, w_star, ...){
  
  jacobian <- .fixpt(rho=rho, alpha=alpha, x=x, w_star=w_star, ...)
  
  jacobian
}