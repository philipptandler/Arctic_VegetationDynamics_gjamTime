
#set up environment:
library(here)
setwd(here::here())
source("Scripts/Analysis/analysisHfunctions.R")



# Define the function for the Monte Carlo simulation
simulate_eigenvalues <- function(alphaMu, alphaSe, num_simulations = 1000) {
  # Store the max real part of eigenvalues for each simulation
  max_real_eigenvalues <- numeric(num_simulations)
  
  for (i in seq_len(num_simulations)) {
    # Generate a perturbed matrix by sampling from normal distribution
    perturbed_matrix <- matrix(rnorm(n = length(alphaMu),
                                     mean = alphaMu,
                                     sd = alphaSe),
                               nrow = nrow(alphaMu),
                               ncol = ncol(alphaMu))
    
    # Calculate the eigenvalues of the perturbed matrix
    eigenvalues <- eigen(perturbed_matrix)$values
    
    # Get the maximum real part of the eigenvalues
    max_real_eigenvalues[i] <- max(Re(eigenvalues))
  }
  
  return(max_real_eigenvalues)
}

set.seed(123)  # For reproducibility

alphaMu <- readRDS(file.path(path_analysis_saveParameters, ".alphaMu.rds"))
alphaSe <- readRDS(file.path(path_analysis_saveParameters, ".alphaSe.rds"))

# Run the Monte Carlo simulation
num_simulations <- 1e6
max_real_parts <- simulate_eigenvalues(alphaMu, alphaSe, num_simulations)

# Calculate the probability of having a non-negative real eigenvalue
prob_non_negative <- sum(max_real_parts >= 0) / num_simulations
cat("Estimated probability of a non-negative real eigenvalue:", prob_non_negative, "\n")
