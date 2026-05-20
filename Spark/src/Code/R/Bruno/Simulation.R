simulation <- function(A, B_vec, Ce_vec, Cp_vec, zi, theta){
  
  n <- nrow(A)
  state <- rep(1, n)
  alpha <- alpha_fun(theta, zi)
  physio_all <- Cf(alpha, Cp_vec)
  
  state_history <- matrix(state, nrow = 1, ncol = n)
  colnames(state_history) <- paste0("sp", 1:n)
  
  prop_species_history <- numeric()
  prop_interactions_history <- numeric()
  degree_history <- matrix(0, nrow = 1, ncol = n)  # grau inicial
  colnames(degree_history) <- paste0("sp", 1:n)
  
  prop_species_history[1] <- mean(state)
  A_active <- A * (state %o% state)
  prop_interactions_history[1] <- sum(A_active) / sum(A)
  degree_history[1, ] <- rowSums(A_active)
  
  t <- 2
  repeat {
    active_species <- which(state == 1)
    if (length(active_species) == 0) break
    
    B_mat <- matrix(0, n, n)
    C_mat <- matrix(0, n, n)
    
    for(i in active_species){
      partners <- which(A[i, ] == 1 & state == 1)
      for(j in partners){
        B_mat[i, j] <- B_vec[j]   # effect of j over i
        B_mat[j, i] <- B_vec[i]   # effect of i over j
        C_mat[i, j] <- Ce_vec[j]
        C_mat[j, i] <- Ce_vec[i]
      }
    }
    
    
    delta <- B_mat - C_mat
    outcome <- rowSums(delta)
    nb <- outcome - physio_all[t, ]
    new_state <- as.numeric(nb > 0)
    
    if (identical(new_state, state)) break
    
    state <- new_state
    state_history <- rbind(state_history, state)
    
    prop_species_history[t] <- mean(state)
    A_active <- A * (state %o% state)
    prop_interactions_history[t] <- sum(A_active) / sum(A)
    degree_history <- rbind(degree_history, rowSums(A_active))
    
    t <- t + 1
  }
  
  A_active <- A * (state %o% state)
  prop_active_species <- mean(state)
  prop_remaining_interactions <- sum(A_active) / sum(A)
  
  return(list(
    state_history = state_history,
    prop_species_history = prop_species_history,
    prop_interactions_history = prop_interactions_history,
    initial_degree = degree_history[1,],
    degree_history = degree_history,
    final_state = state,
    prop_active_species = prop_active_species,
    prop_remaining_interactions = prop_remaining_interactions
  ))
}




#----------------------
# Simulation example
set.seed(123)
n <- nrow(modular)
B_vec <- rbeta(n,.5,.5)
Ce_vec <- rbeta(n,.5,.5)
Cp_vec <- rbeta(n,.5,.5)*0.2 
zi <- runif(n, 1, 10)
theta <- environment(A_min = 1, A_max = 10, w_min = 1, w_max = 5, t_max = 100)

  

resultado <- simulation(modular, B_vec, Ce_vec, Cp_vec, zi, theta)
resultado



