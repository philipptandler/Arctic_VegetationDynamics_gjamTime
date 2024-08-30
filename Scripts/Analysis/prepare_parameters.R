#' This Scirpt prepares the ydata and xdata and fits the gjamTime function
#' I run the 'crop' version with subset = FALSE on my laptop (32 GB RAM) without
#' and problems. 
#' The 'full' version I run on the computing cluster Euler at ETH Zürich with
#' 128 GB memory.Beware, the 'full' version with subset = FASLE overflows the
#' memory even on the comuptin cluster 


#set up environment:
library(here)
setwd(here::here())
source("Scripts/Analysis/analysisHfunctions.R")

# loads gjamOutputs
estimates_all <- load_estimates_gjam(folderPattern = "gjam_official_full_subs100_[0-9]{4}",
                                     directory = path_gjamTime_outStorge,
                                     save = TRUE)

alphaMu <- estimates_all$alphaMu
alphaSe <- estimates_all$alphaSe
rhoMu <- estimates_all$rhoMu
rhoSe <- estimates_all$rhoSe

cat("\nalphaMu\n")
print(alphaMu)
cat("\nalphaSe\n")
print(alphaSe)
cat("\nrhoMu\n")
print(rhoMu)
cat("\nrhoSe\n")
print(rhoSe)
