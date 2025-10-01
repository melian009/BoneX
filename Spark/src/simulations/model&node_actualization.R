simulation <- function(A, B_vec, Ce_vec, Cp_vec, zi, theta){
  
  n <- nrow(A)               # número de espécies originais
  state <- rep(1, n)         # estado inicial das espécies
  alpha <- alpha_fun(theta, zi)
  physio_all <- Cf(alpha, Cp_vec)
  
  state_history <- matrix(state, nrow = 1, ncol = n)  # inicializar com a primeira linha
  colnames(state_history) <- paste0("sp", 1:n)
  
  t <- 2
  repeat {
    active_species <- which(state == 1)
    
    if(length(active_species) == 0) break  # se não há espécies ativas, parar
    
    # Inicializar B_mat e C_mat quadradas
    B_mat <- matrix(0, n, n)
    C_mat <- matrix(0, n, n)
    
    for(i in active_species){
      partners <- which(A[i, ] == 1 & state == 1)
      if(length(partners) > 0){
        B_mat[i, partners] <- B_vec[partners]
        C_mat[i, partners] <- Ce_vec[partners]
      }
    }
    
    delta <- B_mat - C_mat
    outcome <- rowSums(delta)
    
    # Apenas espécies originais
    nb <- outcome - physio_all[t, ]
    new_state <- as.numeric(nb > 0)
    
    # Checa se mudou; se não mudou, para
    if(identical(new_state, state)) break
    
    state <- new_state
    state_history <- rbind(state_history, state)  # adiciona nova linha dinamicamente
    t <- t + 1
  }
  
  # Matriz de interações ativas finais
  A_active <- A * (state %o% state)
  
  prop_active_species <- mean(state)
  prop_remaining_interactions <- sum(A_active) / sum(A)
  
  return(list(
    state_history = state_history,
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

  

resultado <- simulation(modular, B_vec, Ce_vec, Cp_vec, zi, theta)
resultado
  