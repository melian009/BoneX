simulation <- function(A, B_vec, Ce_vec, Cp_vec, zi, theta, n_steps = 100){
  
  n <- nrow(A)               # número de espécies originais
  t_max <- length(theta)      # tempo total
  state <- rep(1, n)          # estado inicial das espécies
  alpha <- alpha_fun(theta, zi)
  physio_all <- Cf(alpha, Cp_vec)
  
  max_steps <- min(n_steps, t_max)
  state_history <- matrix(NA, nrow = max_steps, ncol = n)
  colnames(state_history) <- paste0("sp", 1:n)
  state_history[1, ] <- state
  
  last_step = 1 
  
  for (t in 2:max_steps){
    
    active_species <- which(state == 1)
    
    # Inicializar B_mat e C_mat quadradas
    B_mat <- matrix(0, n, n)
    C_mat <- matrix(0, n, n)
    
    if(length(active_species) > 0){
      for (i in active_species){
        partners <- which(A[i, ] == 1 & state == 1)
        if(length(partners) > 0){
          B_mat[i, partners] <- B_vec[partners]
          C_mat[i, partners] <- Ce_vec[partners]
simulation <- function(A, B_vec, Ce_vec, Cp_vec, zi, theta, n_steps = 100){
  
  n <- nrow(A)               # número de espécies originais
  t_max <- length(theta)      # tempo total
  state <- rep(1, n)          # estado inicial das espécies
  alpha <- alpha_fun(theta, zi)
  physio_all <- Cf(alpha, Cp_vec)
  
  max_steps <- min(n_steps, t_max)
  state_history <- matrix(NA, nrow = max_steps, ncol = n)
  colnames(state_history) <- paste0("sp", 1:n)
  state_history[1, ] <- state
  
  last_step = 1 
  
  for (t in 2:max_steps){
    
    active_species <- which(state == 1)
    
    # Inicializar B_mat e C_mat quadradas
    B_mat <- matrix(0, n, n)
    C_mat <- matrix(0, n, n)
    
    if(length(active_species) > 0){
      for (i in active_species){
        partners <- which(A[i, ] == 1 & state == 1)
        if(length(partners) > 0){
          B_mat[i, partners] <- B_vec[partners]
          C_mat[i, partners] <- Ce_vec[partners]
        }
      }
      
      delta <- B_mat - C_mat
      outcome <- rowSums(delta)
      
      nb <- outcome - physio_all[t, ]  # apenas espécies originais
      new_state <- as.numeric(nb > 0)
      state_history[t, ] <- new_state
      last_step = t
      
      if(identical(new_state, state)) break
      state <- new_state
    }
  }
  
  
  # Matriz de interações ativas finais
  A_active <- A * (state %o% state)
  
  prop_active_species <- mean(state)
  prop_remaining_interactions <- sum(A_active) / sum(A)
  
  
  
  return(list(
    state_history = state_history[1:t, , drop = FALSE],
    final_state = state,
    prop_active_species = prop_active_species,
    prop_remaining_interactions = prop_remaining_interactions
  ))
}




#----------------------
# Simulation example
set.seed(123)
n <- nrow(modular)
B_vec <- rbeta(n,5,5)
Ce_vec <- rbeta(n,5,5)
Cp_vec <- rbeta(n,5,5)*0.2 
zi <- runif(n, 1, 10)
theta <- environment(1, 10, 1, 5, t_max = 100)

  

resultado <- simulation(modular, B_vec, Ce_vec, Cp_vec, zi, theta, n_steps = 100)
resultado
  