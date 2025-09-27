#Ecosystem services function

ecosystem <- function(object, P_matrix) {
  # Species History states
  state_history <- object$state_history #bringing the simulation time steps from the iteration of costs-benefits
  #this one has t in rows and species status in colums
  n <- nrow(state_history)
  
  # Calculate services in each time t: multiply the transpose P by state_history
  services_history <- state_history %*% t(P_matrix) #P needs the ecosystem services on the rows
  
  # Selecting the first and last time steps
  E_initial <- services_history[1, ]
  E_final   <- services_history[n, ]
  
  # Diference of ecosystem services between initial and final at time steps
  delta_E <- E_initial - E_final
  
  return(list(
    services_history = services_history, # All time steps
    E_initial = E_initial,
    E_final = E_final,
    delta_E = delta_E
  ))
}


#--------------------------------------------

#Implementation exemple
set.seed(123)
#exemple matrix of the size of the iterations time steps and species number in the main model
E_matrix <- matrix(runif(5*50, 0, 1), nrow = 5)
# Calcular serviços ecossistêmicos
services <- ecosystem(resultado, E_matrix) #caling the above functions

# Looking at the changes of ecosystem services along the time
print(services$services_history)

# the diference at the first to the last time steps
print(services$delta_E)

