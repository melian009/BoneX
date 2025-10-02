#Ecosystem services function

ecosystem <- function(object, ES_matrix) {
  # Species History states
  state_history <- object$state_history
  
  # Garantir que linhas = time steps, colunas = espécies
  state_history <- as.matrix(state_history)
  rownames(state_history) <- NULL
  n_time <- nrow(state_history)
  
  # Garantir que ES_matrix está em forma de matriz
  ES_matrix <- as.matrix(ES_matrix)
  
  # Calcular serviços em cada time step
  services_history <- state_history %*% ES_matrix
  
  # Nomear linhas e colunas
  rownames(services_history) <- paste0("t", 1:n_time)
  colnames(services_history) <- colnames(ES_matrix)
  
  # Selecionar estado inicial e final
  E_initial <- services_history[1, ]
  E_final   <- services_history[n_time, ]
  
  # Diferença de serviços entre início e fim
  delta_E <- E_initial - E_final
  
  # Retornar lista organizada
  return(list(
    services_history = services_history,
    E_initial = E_initial,
    E_final = E_final,
    delta_E = delta_E
  ))
}



#--------------------------------------------


# Calcular serviços ecossistêmicos
services_dinamics <- ecosystem(resultado, services) #caling the above functions
services_dinamics
# Looking at the changes of ecosystem services along the time
print(services_dinamics$services_history)

# the diference at the first to the last time steps
print(services_dinamics$delta_E)

library(ggplot2)
