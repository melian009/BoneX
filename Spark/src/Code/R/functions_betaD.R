## Supportive and main function

# Create a unit vector
unit_vector <- function(x) {x/sum(sqrt(x^2))}

# Function to build a random graph
build_random_graph <- function(nodes_i, nodes_j, connectance, return_adjacency=TRUE){
  all_connected <- FALSE
  while(!all_connected){
    # creating the matrix with expected connectance
    A <- (matrix(runif(nodes_i * nodes_j), nodes_i, nodes_j) <= connectance)
    all_connected <- all(rowSums(A) > 0) & all(colSums(A) > 0)
    # Sampling interactions
  }
  A <- A*1
  
  if(return_adjacency){
    A <- cbind(rbind(A, matrix(0,nodes_j,nodes_j)), 
               rbind(matrix(0,nodes_i,nodes_i), t(A)))
    rownames(A) <- paste0("sp", 1:(nodes_i+nodes_j))
    colnames(A) <- paste0("sp", c((nodes_i+1):(nodes_j+nodes_i), 1:nodes_i))
  }
  
  return(A)
}


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

# Fixed Environmental effect
define_alpha <- function(zi){
  theta <- runif(1)
  alpha <- abs(there - zi)
  return(alpha)
}

# Estimate the biotic and abiotic fitness components separately  given a
# matrix of benefits, costs (biotic) and a vector of physiological costs
define_fitness <- function(eco_costs, physio_costs, tot_benefits, alpha){
  fitness <- rowSums(tot_benefits - eco_costs) - alpha*physio_costs
  return(fitness)
}

# Get ratio between costs and benefits
get_fitness_ratio <- function(eco_costs, physio_costs, tot_benefits){
  fitness_ratio <- rowSums(tot_benefits - eco_costs) - physio_costs
  return(fitness_ratio)
}

# Random Ecosystem Service Matrix
# A given proportion of species contribute to each service
# Species contribution is drawn from a uniform distribution and
# normalized to sum to one 
create_ES_random_matrix <- function(n_sp, n_services, prop_contributing_sp){
  ES <- matrix(0, nrow = n_services, ncol = n_sp)
  for (i in 1:n_services) {
    # select species to contribute to the service
    selected_sp <- sample(n_sp, ceiling(prop_contributing_sp*n_sp))
    # define the contribution -- sums to one
    contribution <- unit_vector(runif(ceiling(prop_contributing_sp*n_sp)))
    # Assign contribution to each species in the matrix
    ES[i,selected_sp] <- contribution
  }
  return(ES)
}


## Running the boolean model
# Input: Number of species in each category of a bipartite system
#        Expected connectance of the system
#        Alpha - environmental cost
#        Statistical distribution of benefits, costs and physiological cost
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


## Estimating how costs and benefits change over time 
# Input of the function is the output of boolean_model
estimate_CB_overtime <- function(model_output){
  
  B <- model_output$B
  C <- model_output$C
  cp <- model_output$costs_p
  sp_present <- model_output$community
  sp_degree_initial <- apply(model_output$B>0, 1, sum)
  sp_names <- rownames(C)
  
  res <- tibble()
  
  for (i in 1:nrow(sp_present)) {
    my_sp <- sp_present[i,] %>% select(where(~sum(.) ==1)) %>% names()
    
    sp_names <- sp_names[sp_names %in% my_sp]
    sp_degree_initial <- sp_degree_initial[my_sp]
    # Update costs_p by zeroing the costs
    new_Cp <- cp[my_sp]
    
    # Update matrix of costs and benefits
    newC <- C[my_sp, my_sp]
    
    newB <- B[my_sp, my_sp]
    
    if(dim(newB)[1] == 0 & dim(newB)[2] == 0) sp_degree <- rep(0, ncol(newB))
    else sp_degree <- apply(newB>0, 1, sum)
    
    # Estimate CB ratio
    CB_ratio <- get_fitness_ratio(newC, new_Cp, newB)  
    
    tmp <- tibble(time_step = i,
                  sp_id = sp_names,
                  sp_k_initial = sp_degree_initial,
                  sp_k_curr = sp_degree,
                  ratio = CB_ratio)
    
    res <- rbind(res, tmp)
    
  }
  return(res)
}
