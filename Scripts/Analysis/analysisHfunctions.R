#' This Script holds helper functions to process and analyse the outputs of 
#' gjamTime.R

## load output gjamTime ####

load_estimates_gjam <- function(folderPattern, directory=NULL, save=TRUE){
  
  directories <- list.dirs(file.path(directory),
                           recursive = FALSE, full.names = TRUE)
  matching_dirs <- directories[grepl(folderPattern, basename(directories))]
  
  #loads all outfiles independently
  samples_alphaMu <- list()
  samples_alphaSe <- list()
  samples_rhoMu <- list()
  samples_rhoSe <- list()

  # Loop over each matching directory
  for (dir in matching_dirs) {
    # Construct the path to the output.Rdata file
    rdata_path <- file.path(dir, "output.Rdata")
    
    # Check if the file exists
    if (file.exists(rdata_path)) {
      # Load the Rdata file
      load(rdata_path) #loads output_short list
      
      samples_alphaMu[[length(samples_alphaMu)+1]] <- output_short$alphaMu
      samples_alphaSe[[length(samples_alphaSe)+1]] <- output_short$alphaSe
      samples_rhoMu[[length(samples_rhoMu)+1]] <- output_short$rhoMu
      samples_rhoSe[[length(samples_rhoSe)+1]] <- output_short$rhoSe

    } else {
      warning(paste("File not found:", rdata_path))
    }
  }
  
  parameters_alpha <- estimate_parameters(samples_alphaMu, samples_alphaSe)
  parameters_rho <- estimate_parameters(samples_rhoMu, samples_rhoSe)
  
  # Initialize an empty list to store the somevalue outputs
  outputlist <- list(
    alphaMu = parameters_alpha$mean,
    alphaSe = parameters_alpha$sd,
    rhoMu = parameters_rho$mean,
    rhoSe = parameters_rho$sd
  )
  
  #save
  if(save){
    for(estimate in names(outputlist)){
      est <- outputlist[[estimate]]
      saveRDS(est, file = file.path(path_analysis, paste0(".",estimate, ".rds")))
    }
    cat("saved estimates under", paste0(path_analysis, "/<estimate>.rds\n"))
  }
  
  # Return the list of somevalue
  return(outputlist)
}


## estimate parameters ####
#' returns estimated mean and sd for X~Norm(mu, sd) according to samples X_i
# mu_list and sd_list as non empty list, same length, identical dimensions of matrix
estimate_parameters <- function(mu_list, sd_list){
  n_row <- nrow(mu_list[[1]])
  n_col <- ncol(mu_list[[1]])
  
  # Initialize matrices to store the numerator and denominator
  weighted_mean <- matrix(0, nrow=n_row, ncol=n_col)
  inverse_variance <- matrix(0, nrow=n_row, ncol=n_col)
  
  # Calculate the numerator and denominator for each element in the matrices
  for (i in 1:length(mu_list)) {
    weighted_mean <- weighted_mean + mu_list[[i]] / (sd_list[[i]]^2)
    inverse_variance <- inverse_variance + 1 / (sd_list[[i]]^2)
  }
  
  # Calculate the weighted mean matrix
  mu_matrix <- weighted_mean / inverse_variance
  
  # Calculate the standard error matrix
  sd_matrix <- sqrt(1 / inverse_variance)
  
  paramater_list <- list(
    mean = mu_matrix,
    sd = sd_matrix
  )
  return(paramater_list)
}


## variable definitions ####
## paths
path_gjamTime_outStorge <- "data/gjamTime_outStorage"
path_analysis <- "Scripts/Analysis"

