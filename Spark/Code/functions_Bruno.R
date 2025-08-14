library(tidyverse)

#----------------------
# Inputing or creating network
network <- function(N, conectancia) {
  A <- matrix(rbinom(N*N, 1, conectancia), nrow = N, ncol = N)
  diag(A) <- 0
  return(A)
}

#----------------------
# Environmental condition (all times t values)
environment <- function(A_min, A_max, w_min, w_max, t_max = 100){
  t <- seq(1, t_max, by = 1)
  A <- runif(1, A_min, A_max)
  w <- runif(1, w_min, w_max)
  amplitude <- A / 2
  frequency <- (1 + sin(2 * pi * w * t))
  theta <- amplitude * frequency
  return(theta)
}
#----------------------
# Alpha (theta - zi)
alpha_fun <- function(theta, zi) {
  t_max <- length(theta)
  n <- length(zi)
  result <- matrix(NA, nrow = t_max, ncol = n)
  for (time in 1:t_max) {
    result[time, ] <- (theta[time] - zi)
  }
  colnames(result) <- paste0("sp", 1:n)
  rownames(result) <- paste0("t", 1:t_max)
  return(abs(result))
}

#----------------------
# Physiological cost (Cf is just to distinguich from Cp - fisiologico / physilogical )
Cf <- function(alpha, Cp){
  t_max = nrow(alpha)
  n <- length(Cp)
  physio <- matrix(NA, nrow = t_max, ncol = n)
  for (time in 1:t_max) {
    physio[time, ] <- (alpha[time, ] * Cp)
  }
  colnames(physio) <- paste0("sp", 1:n)
  rownames(physio) <- paste0("t", 1:t_max)
  return(physio)
}

#----------------------
# Simulating dinamics
simulation <- function(A, B_vec, Ce_vec, Cp_vec, zi, theta, n_steps = 100){
  
  n <- nrow(A)
  t_max <- length(theta)
  
  # Alpha and physiological costs pre-calculated for all times t
  alpha <- alpha_fun(theta, zi)
  physio_all <- Cf(alpha, Cp_vec)
  
  # Inicial state: all species present
  state <- rep(1, n) 
  
  # Matrix to store history of states (states changes trhough time)
  state_history <- matrix(NA, nrow = min(n_steps, t_max), ncol = n)
  colnames(state_history) <- paste0("sp", 1:n)
  
  for (t in 1:min(n_steps, t_max)) {
    
    # Filtering A Matriz for just active species (1) 
    active_species <- which(state == 1)
    
    # Actualizating costs and benefit matrices (C_mat & B_mat) considering just existing interactions  with active species 
    B_mat <- matrix(0, n, n)
    C_mat <- matrix(0, n, n)
    for (i in active_species) {
      partners <- which(A[i, ] == 1 & state == 1)
      B_mat[i, partners] <- B_vec[partners]
      C_mat[i, partners] <- Ce_vec[partners]
    }
    
    # Especialization: benefits saturation
    sum_b <- rowSums(B_mat)
    B_max <- max(B_vec)
    B_final <- ifelse(sum_b > B_max, B_max, sum_b)
    
    # Calculating netbenefits NB: benefits - (ecological costs + ´hysiological costs)
    C_total <- rowSums(C_mat) + physio_all[t, ]
    nb <- B_final - C_total
    
    # State actualization: species turns active if NB > 0
    new_state <- ifelse(nb > 0, 1, 0)
    
    # **Store the hystory of species states on the matrix**
    state_history[t, ] <- new_state
    
    # Stop itaration condition: stable state when species stop to change state
    if (all(new_state == state)) break
    
    state <- new_state
  }
  
  # FInal active interactions 
  A_active <- A * (state %o% state) #outer product of states in A_ij 
  
  # Final Metrics
  prop_active_species <- sum(state) / n
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
A <- network(10, 0.3)
diag(A) <- 0
n <- nrow(A)
B_vec <- rlnorm(n)
Ce_vec <- rlnorm(n)
Cp_vec <- rlnorm(n) * 0.1
zi <- rnorm(n, 0, 1)
theta <- environment(0.01, 0.1, 0.01, 0.1, t_max = 100)

resultado <- simulation(A, B_vec, Ce_vec, Cp_vec, zi, theta, n_steps = 100)
resultado
