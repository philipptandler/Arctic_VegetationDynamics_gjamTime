## sourcing general scripts ####
source("scripts/core/2_analysis/.analysis_Hfunctions.R")

eigen <- function(jacobian, ...){
  
  lamda <- .eigen(jacobian=jacobian, ...)
  
  lamda
}