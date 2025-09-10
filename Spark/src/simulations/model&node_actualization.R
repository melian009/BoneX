# Simulating dinamics
simulation <- function(A, B_vec, Ce_vec, Cp_vec, zi, theta, n_steps = 100){
  
  n <- nrow(A)
  t_max <- length(theta)

  # square matrix  
  group_1 <- nrow(A)
  group_2 <- ncol(A)
  rownames(A_raw) <- paste("1", 1:group_1, sep = "")
  colnames(A_raw) <- paste("2", 1:group_2, sep = "")
  m1 <- cbind(matrix(0, group_1, group_1), A)
  m2 <- cbind(t(A), matrix(0, group_2, group_2))
  A <- rbind(m1, m2)
  diag(A) <- 0
  
  # Alpha and physiological costs pre-calculated for all times t
  alpha <- alpha_fun(theta, zi)
  physio_all <- Cf(alpha, Cp_vec)
  
  # Inicial state: all species present
  state <- rep(1, n) 
  
  # Matrix to store history of states (states changes trhough time)
  state_history <- matrix(NA, nrow = min(n_steps, t_max), ncol = n)
  colnames(state_history) <- paste0("sp", 1:n)
  
  state_history[1, ] <- state
  for (t in 2:min(n_steps, t_max)) {
    
    # Filtering A Matriz for just active species (1) 
    active_species <- which(state == 1)
    
    # Actualizating costs and benefit matrices (C_mat & B_mat) considering just existing interactions with active species 
    B_mat <- matrix(0, n, n)
    C_mat <- matrix(0, n, n)
    for (i in active_species) {
      partners <- which(A[i, ] == 1 & state == 1)
      B_mat[i, partners] <- B_vec[partners]
      C_mat[i, partners] <- Ce_vec[partners]
    }

    b1 <- nrow(B_mat)
    b2 <- ncol(B_mat)
    m1 <- cbind(matrix(0, b1, b1), B_mat)
    m2 <- cbind(t(B_mat), matrix(0, b2, b2))
    B_mat <- rbind(m1, m2)
    diag(B_mat) <- 0

    c1 <- nrow(B_mat)
    c2 <- ncol(B_mat)
    m1 <- cbind(matrix(0, c1, c1), C_mat)
    m2 <- cbind(t(C_mat), matrix(0, c2, c2))
    C_mat <- rbind(m1, m2)
    diag(C_mat) <- 0
    
    
    # Especialization: benefits saturation
    #sum_b <- rowSums(B_mat)
    #B_max <- max(B_vec)
    #B_final <- ifelse(sum_b > B_max, B_max, sum_b)
    
    delta <- B_mat - C_mat
    outcome <- rowSums(delta)
    
    # Calculating netbenefits NB: benefits - (ecological costs + physiological costs)
    nb <- outcome - physio_all[t, ]
   
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
A <- network(50, 0.3)
diag(A) <- 0
n <- nrow(A)
B_vec <- rbeta(n,5,5)
Ce_vec <- rbeta(n,5,5)
Cp_vec <- rbeta(n,5,5)*0.2 
zi <- runif(n, 1, 10)
theta <- environment(1, 10, 1, 5, t_max = 100)



resultado <- simulation(A, B_vec, Ce_vec, Cp_vec, zi, theta, n_steps = 100)
resultado
