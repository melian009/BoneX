## Supportive and main function


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


## Create the Cost/Benefit matrix given an adjacency matrix with choice of 
  # statistical distribution defining the values
create_CB_matrix <- function(Aij, is.bipartite=FALSE, distribution="lognormal", ...){
  #if it's not binary, make it binary
  matrix_A <- Aij
  matrix_A <- as.matrix((matrix_A > 0) + 0)
  # If network is bipartite, convert to adjacency
  if(is.bipartite){
    matrix_A <- cbind(rbind(Aij, matrix(0,ncol(Aij),ncol(Aij))), 
               rbind(matrix(0,nrow(Aij),nrow(Aij)), t(Aij)))
  }
  # number of samples
  nsamples <- sum(matrix_A)
  
  if (distribution == "normal") {
    values <- rnorm(nsamples, ...)
  }
  if (distribution == "beta") {
    values <- rbeta(nsamples, ...)
  }
  if (distribution == "lognormal") {
    values <- rlnorm(nsamples, ...)
  }
  if (distribution == "skewnormal") { # Used in Campbell
    values <- rsn(nsamples, ...)
  }
  if (distribution == "uniform") {
    values <- runif(nsamples, ...)
  }
  # Make the cost/benefit matrix
  CB_mt <- matrix_A
  CB_mt[CB_mt>0] <- values
  return(CB_mt)
}

# Environmental effect - can be a single number of a fluctuating function
define_alpha <- function(){
  
}

# Estimate the biotic and abiotic fitness components separatelu  given a
  # matrix of benefits, costs (biotic) and a vector of physiological costs
define_fitness <- function(eco_costs, physio_costs, tot_benefits){
  fitness <- rowSums(tot_benefits - eco_costs) - physio_costs
  return(fitness)
}

# Get ratio between costs and benefits
get_fitness_ratio <- function(eco_costs, physio_costs, tot_benefits){
  fitness_ratio <- rowSums(tot_benefits - eco_costs) - physio_costs
  return(fitness_ratio)
}


## Running the model
 # Input: Number of species in each category of a bipartite system
 #        Expected connectance of the system
 #        Statistical distribution of benefits, costs and physiological cost
boolean_model <- function(Ni, Nj, connectance, 
                          dist_B = "lognormal", dist_C = "lognormal", 
                          dist_Cp = "lognormal",...){
  # Building the matrix of interactions
  A <- build_random_graph(Ni, Nj, connectance)
  
  # Everyone starts present
  all_present <- rep(1, Ni+Nj)
  names(all_present) <- rownames(A)
  
  community <- as_tibble(t(all_present))
  
  #Creating costs and benefits
  C <- create_CB_matrix(A, distribution = dist_C,...)
  B <- create_CB_matrix(A, distribution = dist_B,...)
  
  # Adding a small physiological cost
  costs_p <- c(create_CB_matrix(c(1:(Ni+Nj)), distribution = dist_Cp,...)*0.1)
  names(costs_p) <- rownames(A)
  
  fitness <- define_fitness(C, costs_p, B)
  
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
    fitness <- define_fitness(newC, new_Cp, newB)  
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


