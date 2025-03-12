
#set up environment:
library(here)
setwd(here::here())
source("Scripts/Analysis/analysisHfunctions.R")

alphaMu <- readRDS(file.path(path_analysis_saveParameters, ".alphaMu.rds"))
rhoMu <- readRDS(file.path(path_analysis_saveParameters, ".rhoMu.rds"))
alphaSe <- readRDS(file.path(path_analysis_saveParameters, ".alphaSe.rds"))
rhoSe <- readRDS(file.path(path_analysis_saveParameters, ".rhoSe.rds"))

names_species <- c("shrub", "conifer", "herbaceous", "lichen")

rownames(alphaMu) <- names_species
rownames(alphaSe) <- names_species

copy_matrix <- function(M, names = NULL){
  df <- as.data.frame(M)
  if(!is.null(names)){colnames(df) <- names_species}
  print(df)
  write.table(df, "clipboard", sep = "\t", row.names = FALSE, col.names = TRUE)
}

copy_matrix(Im(eigen(alphaMu)$values))


