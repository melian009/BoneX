## Supportive functions Lognormal distribution

#____________________________________________________________________________#

# KL Divergence between LogNorm(mu1, sigma1) and LogNorm(mu2, sigma2)
kl_lognorm <- function(mu1, sigma1, mu2, sigma2) {
  
  DKL <- 0.5 * (log(abs(sigma2/sigma1)) + 
                  (sigma1^2 + (mu1 - mu2)^2)/(2*sigma2^2) - 1/2)
  
  return(DKL)
}

## Difference between the means 
mean_lognorm <- function(mu, sigma){
  ex <- exp(mu + (sigma^2)/2)
  return(ex)
}

diff_mean_lognorm <- function(mu1, sigma1, mu2, sigma2){
  res_diff <- mean_lognorm(mu1,sigma1) - mean_lognorm(mu2,sigma2)
  return(res_diff)
}

#____________________________________________________________________________#

## Create the Cost/Benefit matrix given an adjacency matrix 
# Lognormal distribution
create_CB_lognorm <- function(Aij, is.bipartite=FALSE, mu=0, sigma=1){
  #if it's not binary, make it binary
  matrix_A <- Aij
  matrix_A <- as.matrix((matrix_A > 0) + 0)
  # If network is bipartite, convert to adjacency
  if(is.bipartite){
    matrix_A <- cbind(rbind(Aij, matrix(0,ncol(Aij),ncol(Aij))), 
                      rbind(matrix(0,nrow(Aij),nrow(Aij)), t(Aij)))
  }
  # number of samples and values
  nsamples <- sum(matrix_A)
  values <- rlnorm(nsamples, meanlog = mu, sdlog = sigma)
  
  # Make the cost/benefit matrix
  CB_mt <- matrix_A
  CB_mt[CB_mt>0] <- values
  return(CB_mt)
}

#____________________________________________________________________________#
## Running the boolean model
# Input: Number of species in each category of a bipartite system
#        Expected connectance of the system
#        Alpha - environmental cost
#        Statistical distribution of benefits, costs and physiological cost
boolean_model <- function(Ni, Nj, connectance, alpha=0.0001,
                          muC = 0, sigmaC = 1, # pars for costs
                          muB = 0, sigmaB = 1, # pars for benefits
                          muCp = 0, sigmaCp = 1){ # pars for physio costs
  # Building the matrix of interactions
  A <- build_random_graph(Ni, Nj, connectance)
  
  # Everyone starts present
  all_present <- rep(1, Ni+Nj)
  names(all_present) <- rownames(A)
  
  community <- as_tibble(t(all_present))
  
  #Creating benefits and costs under Beta distribution
  B <- create_CB_lognorm(A, mu=muB, sigma=sigmaB)
  C <- create_CB_lognorm(A, mu=muC, sigma=sigmaC)
  
  # Adding a small physiological cost
  costs_p <- c(create_CB_lognorm(c(1:(Ni+Nj)), mu=muCp, sigma=sigmaCp)*0.1)
  
  #costs_p <- c(create_CB_matrix(c(1:(Ni+Nj)), distribution = dist_Cp,...)*0.1)
  names(costs_p) <- rownames(A)
  
  fitness <- define_fitness(C, costs_p, B, alpha)
  
  changing <- TRUE
  while(changing>0){
    
    # These are the species that should be there in the next step
    presence <- (fitness>0)*1
    extinct_sp <- names(presence[presence==0])
    
    # Check if it's the same
    changing <- sum(tail(community, 1) != presence)
    
    # Update A by adding zero to extinct species
    newA <- A
    newA[extinct_sp,] <- 0
    newA[,extinct_sp] <- 0
    
    # Update costs_p by zeroing the costs
    new_Cp <- costs_p
    new_Cp[extinct_sp] <- 0
    
    # Update matrix of costs and benefits
    newC <- C
    newC[extinct_sp,] <- 0
    newC[,extinct_sp] <- 0
    
    newB <- B
    newB[extinct_sp,] <- 0
    newB[,extinct_sp] <- 0
    
    community <- community %>% rbind(presence)
    
    # Define new fitness
    fitness <- define_fitness(newC, new_Cp, newB, alpha)  
  }
  
  res <- list(community = community, 
              A = A, B = B, C=C, 
              costs_p = costs_p)
  
  return(res)
  
}
