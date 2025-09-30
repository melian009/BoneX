# Simulating dinamics
simulation <- function(A, B_vec, Ce_vec, Cp_vec, zi, theta, n_steps = 100){
  
  # Dimensões originais (número de espécies)
  n <- nrow(A)  # número de espécies originais
  group_1 <- nrow(A)
  group_2 <- ncol(A)
  t_max <- length(theta)
  
  # Transformar A em matriz quadrada para cálculos
  rownames(A) <- paste("1", 1:group_1, sep = "")
  colnames(A) <- paste("2", 1:group_2, sep = "")
  A_square <- rbind(
    cbind(matrix(0, group_1, group_1), A),
    cbind(t(A), matrix(0, group_2, group_2))
  )
  diag(A_square) <- 0
  
  # Dimensão da matriz quadrada
  n_total <- nrow(A_square)
  
  # Alpha and physiological costs (baseados nas espécies originais)
  alpha <- alpha_fun(theta, zi)
  physio_all <- Cf(alpha, Cp_vec)
  
  # Estado inicial: todas as espécies originais presentes
  state <- rep(1, n)  # apenas para as espécies originais
  
  # Criar estado expandido para a matriz quadrada (duplicar estado)
  state_expanded <- c(state, state[1:group_2])
  
  # Matrix to store history of states (apenas espécies originais)
  max_steps <- min(n_steps, t_max)
  state_history <- matrix(NA, nrow = max_steps, ncol = n)
  colnames(state_history) <- paste0("sp", 1:n)
  
  state_history[1, ] <- state
  
  for (t in 2:max_steps) {
    
    # Atualizar estado expandido
    state_expanded <- c(state, state[1:group_2])
    
    # Filtering matrix for just active species
    active_species <- which(state_expanded == 1)
    
    # Initialize benefit and cost matrices (tamanho da matriz quadrada)
    B_mat <- matrix(0, n_total, n_total)
    C_mat <- matrix(0, n_total, n_total)
    
    if(length(active_species) > 0) {
      for (i in active_species) {
        partners <- which(A_square[i, ] == 1 & state_expanded == 1)
        if(length(partners) > 0) {
          # Mapear benefits/costs das espécies originais
          if(i <= n) {  # se é uma espécie original
            B_mat[i, partners] <- B_vec[pmin(partners, n)]  # usar valores das espécies originais
            C_mat[i, partners] <- Ce_vec[pmin(partners, n)]
          } else {  # se é uma espécie "duplicada" 
            original_i <- i - n  # mapear para espécie original
            B_mat[i, partners] <- B_vec[pmin(partners, n)]
            C_mat[i, partners] <- Ce_vec[pmin(partners, n)]
          }
        }
      }
      
      # Calcular delta usando a matriz quadrada completa
      delta <- B_mat - C_mat
      outcome <- rowSums(delta)
      
      # Calculating netbenefits apenas para as espécies originais
      nb <- outcome[1:n] - physio_all[t, ]  # pegar apenas os primeiros n valores
      
      # State actualization: species turns active if NB > 0
      new_state <- as.numeric(nb > 0) 
      
      # Store the history of species states
      state_history[t, ] <- new_state
      
      # Stop iteration condition
      if (identical(new_state, state)) break
      
      state <- new_state
    }
  }
  
  # Final active interactions (usando estado das espécies originais)
  final_state_expanded <- c(state, state[1:group_2])
  A_active <- A_square * (final_state_expanded %o% final_state_expanded)
  
  # Final Metrics
  prop_active_species <- mean(state)
  prop_remaining_interactions <- sum(A_active) / sum(A_square)
  
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
A <- star_net(20)
A <- rnet(20)
diag(A) <- 0
n <- nrow(A)
B_vec <- rbeta(n,5,5)
Ce_vec <- rbeta(n,5,5)
Cp_vec <- rbeta(n,5,5)*0.2 
zi <- runif(n, 1, 10)
theta <- environment(1, 10, 1, 5, t_max = 100)



resultado <- simulation(A, B_vec, Ce_vec, Cp_vec, zi, theta, n_steps = 100)
resultado
