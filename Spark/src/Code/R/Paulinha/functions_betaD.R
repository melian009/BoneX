## Supportive functions - Beta distribution

#____________________________________________________________________________#
# Ref: Nawa V, Nadarajah S. Exact Expressions for Kullback-Leibler Divergence 
# for Univariate Distributions. Entropy (Basel). 2024 Nov 7;26(11):959. 
# doi: 10.3390/e26110959. PMID: 39593904; PMCID: PMC11592849.

# KL Divergence between Beta(a1, b1) and Beta(a2, b2)
kl_beta <- function(a1, b1, a2, b2) {
  # Log ratio of Beta functions
  log_beta_ratio <- lbeta(a2, b2) - lbeta(a1, b1)
  
  # Digamma terms
  term1 <- (a1 - a2) * psigamma(a1, 0)
  term2 <- (b1 - b2) * psigamma(b1, 0)
  term3 <- (a2 - a1 + b2 - b1) * psigamma(a1 + b1, 0)
  
  return(log_beta_ratio + term1 + term2 + term3)
}

## Difference between the means 
mean_beta <- function(a1, b1){
  ex <- a1/(a1+b1)
  return(ex)
}

diff_mean_beta <- function(a1, b1, a2, b2){
  res_diff <- mean_beta(a1,b1) - mean_beta(a2,b2)
  return(res_diff)
}

#____________________________________________________________________________#
## Create the Cost/Benefit matrix given an adjacency matrix 
# Beta distribution
create_CB_beta <- function(Aij, is.bipartite=FALSE, shape1, shape2,...){
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
  values <- rbeta(nsamples, shape1=shape1, shape2=shape2)
  
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
#        Parameters of beta distribution for benefits, costs and 
#         physiological cost
boolean_model <- function(Ni, Nj, connectance, alpha=0.0001,
                          shape1C = 5, shape2C = 5, # Shape pars for costs
                          shape1B = 5, shape2B = 5, # Shape pars for benefits
                          shape1Cp = 5, shape2Cp = 5){ # Shape for physio costs
  # Building the matrix of interactions
  A <- build_random_graph(Ni, Nj, connectance)
  
  # Everyone starts present
  all_present <- rep(1, Ni+Nj)
  names(all_present) <- rownames(A)
  
  community <- as_tibble(t(all_present))
  
  #Creating benefits and costs under Beta distribution
  B <- create_CB_beta(A, shape1=shape1B, shape2=shape2B)
  C <- create_CB_beta(A, shape1=shape1C, shape2=shape2C)
  
  # Adding a small physiological cost
  costs_p <- c(create_CB_beta(c(1:(Ni+Nj)), shape1=shape1Cp, shape2=shape2Cp)*0.1)

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
