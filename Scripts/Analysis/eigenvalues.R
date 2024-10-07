# this Script calculates the eigenvalues for each cell

#set up environment:
library(here)
setwd(here::here())
useScratchifTerminal <- TRUE 
source("Scripts/Analysis/analysisHfunctions.R")


## General SETUP ####
time <- 1
chunk_size <- 160
chunkprossessing <- TRUE

## system Arguments
if(length(sysArgs) > 0){time <- as.integer(sysArgs[1])}
if(time < 0 || time > 2){stop("invalid argument: time")}
if(length(sysArgs) > 1){
  chunk_size <- as.integer(sysArgs[2])
  chunkprossessing = TRUE
}
if(chunk_size < 1){stop("invalid argument: chunk_size")}
n_chunks <- ceiling(X_DIM_RASTER/chunk_size)


# periods
periods_list <- list(
  "1" = "1990",
  "2" = "2020"
)
period_char <- periods_list[[time]]

## names
name_jacobian <-paste0("jacobian_", period_char)
name_lamda <- paste0("lamda_all_", period_char)
name_lamdaDominant <- paste0("lamda_dominant_", period_char)
name_lamdaHarmonic <- paste0("lamda_harmonic_", period_char)
name_tauHarmonic <- paste0("tau_harmonic_", period_char)


## load rasters
J <- rast(file.path(path_analysis_data_rast,
                    paste0(name_jacobian,".tif")))

## process chunks

cat("Calculating eigenvalues for", period_char, ":\n")
for(chunk in 1:n_chunks){
  cat("process", chunk, "of", n_chunks, "chunks: ")
  # subset boundaries
  # xmin <- (chunk-1)*chunk_size+1
  # xmax <- min(chunk*chunk_size, X_DIM_RASTER)
  # ymin <- 1
  # ymax <- Y_DIM_RASTER
  ## to test
  xmin <- 4623
  xmax <- 4683
  ymin <- 7001+chunk*100
  ymax <- 7100+chunk*100
  cat("subset...")
  J_subs <- J[ymin:ymax, xmin:xmax, drop = F]
  
  # eigenvalues
  cat(", calculate eigen values...")
  lamda_subs <- app(J_subs, compute_eigenvalues)
  mask_lamdaPos <- (lamda_subs[[1]]<0 &
                    lamda_subs[[2]]<0 &
                    lamda_subs[[3]]<0 &
                    lamda_subs[[4]]<0)
  lamda_subs <- mask(lamda_subs, mask_lamdaPos, maskvalues=0, updatevalue=NA)
  names(lamda_subs) <- c("lamda_1","lamda_2","lamda_3","lamda_4")
  writeRaster(lamda_subs,
              file.path(path_analysis_chunkprocesses,
                        paste0(name_lamda, "-", chunk,".tif")),
              overwrite = TRUE,
              datatype = "FLT4S")
  
  ## filter out eigenvalues >= 0
  #' we can do this because we can proof that our Jacobian is stable (proof in Clenet 2023 MathBio)
  # dominant eigenvalue
  cat(", lamda dominant...")
  lamda_dominant_subs <- max(lamda_subs)
  names(lamda_dominant_subs) <- c("lamda_dominant")
  writeRaster(lamda_dominant_subs,
              file.path(path_analysis_chunkprocesses,
                        paste0(name_lamdaDominant, "-", chunk,".tif")),
              overwrite = TRUE,
              datatype = "FLT4S")
  # harmonic reactiontime tau_harmonic = 1/lamda_harmonic = mean(tau_i) = mean(1/lamda_i)
  cat(" and tau harmonic...")
  tau_i_subs <- 1/lamda_subs
  tau_mean_subs <- mean(tau_i_subs)
  # harmonic lamda
  lamda_harmonic_subs <- 1/tau_mean_subs
  names(lamda_harmonic_subs) <- c("lamda_harmonic")
  writeRaster(lamda_harmonic_subs,
              file.path(path_analysis_chunkprocesses,
                        paste0(name_lamdaHarmonic, "-", chunk,".tif")),
              overwrite = TRUE,
              datatype = "FLT4S")
  # harmonic tau
  tau_harmonic_subs <- log(-tau_mean_subs)
  names(tau_harmonic_subs) <- c("tau_harmonic")
  writeRaster(tau_harmonic_subs,
              file.path(path_analysis_chunkprocesses,
                        paste0(name_tauHarmonic, "-", chunk,".tif")),
              overwrite = TRUE,
              datatype = "FLT4S")
  cat(" done.\n")
}

## merge all files produced ####
## merge and write Jacobian
cat("merging chunks eigenvalues \n")
lamda <- mergeAndWrite(name_lamda, save = TRUE, datatype = "FLT4S")
cat("merging chunks lamda Dominant \n")
lamda_dominant <- mergeAndWrite(name_lamdaDominant, save = TRUE, datatype = "FLT4S")
cat("merging chunks lamda Harmonic \n")
lamda_harmonic <- mergeAndWrite(name_lamdaHarmonic, save = TRUE, datatype = "FLT4S")
cat("merging chunks tau Harmonic \n")
tau_harmonic <- mergeAndWrite(name_tauHarmonic, save = TRUE, datatype = "FLT4S")

cat(" => merged eigenvalues for", period_char,":))\n\n\n\n\n\n")

