## Main function

# Create a unit vector -- necessary for ES matrix
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

# Get ratio between costs and benefits
get_fitness_ratio <- function(eco_costs, physio_costs, tot_benefits){
  fitness_ratio <- rowSums(tot_benefits - eco_costs) - physio_costs
  return(fitness_ratio)
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
    
    if(is_empty(my_sp)){
      print("all sp went extinct")
      break
    }
    
    sp_names <- sp_names[sp_names %in% my_sp]
    sp_degree_initial <- sp_degree_initial[my_sp]
    # Update costs_p by zeroing the costs
    new_Cp <- cp[my_sp]
    
    # Update matrix of costs and benefits
    newC <- as.matrix(C[my_sp, my_sp])
    
    newB <- as.matrix(B[my_sp, my_sp])
    
    if(dim(newB)[1] == 0 & dim(newB)[2] == 0) { # no sp is left
      previous_sp <- sp_present[c(i-1),] %>% 
        select(where(~sum(.) ==1)) %>% names()
      newB <- as.matrix(B[previous_sp, previous_sp])
      newC <- as.matrix(C[previous_sp, previous_sp])
      new_Cp <- cp[previous_sp]
      sp_degree <- rep(0, 1)
    } else {sp_degree <- apply(newB>0, 1, sum)}
    
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
