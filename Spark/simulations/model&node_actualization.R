
#creating the model
model <- function(A, alpha, n_steps = 100) {
  n <- nrow(A) #the number of rows
  B_vec <- runif(n, 0.5, 1) #benefit vector applied to all species (all species provide benefits to its partners)
  Ce_vec <- runif(n, 0.1, 0.5) #ecological costs vector for all species (all i species pay an ecological cost)
  Cf_vec <- runif(n, 0.05, 0.2) * alpha #physiological costs payed by every species multiplied by the alpha (the factor odf increase or decreasing costs)
#now to facilitate the calculations, we have two diferent matrices 
  B_mat <- matrix(0, n, n) #the benefits matrices
  C_mat <- matrix(0, n, n) #the costs matrices
  
  #now we creat two matrices B and C matrices to input the costs of benefits of each interaction
  for (i in 1:n) {#for each i
    for (j in 1:n) {#and for each j
      if (A[i, j] == 1) {#if there is an interaction betwee than (1)
        B_mat[i, j] <- B_vec[j] #the benefit value is set in the benefit matrix
        C_mat[i, j] <- Ce_vec[i] #the ecological costs value payed to interact with each partner  are set in the costs matrix
      }
    }
  }
  
  return(list(B_mat = B_mat, C_mat = C_mat, Cf_vec = Cf_vec))
}
  
#the Boolean actualization  

#bollean function
actualize_state <- function(actual_state) {
  n <- nrow(A)  
  nb <- numeric(n) #net benefit (NB) of interaction pairs
  
  for (i in 1:n) { #for each species i
     partners <- which(A[i, ] == 1) #identify their interaction partners [A_ij for all j]
     total_benefit <- sum(B_mat[i, partners] * actual_state[partners]) #sum the benefits provided by all active partners 
     total_cost <- sum(C_mat[i, partners] * estado_atual[partners]) + Cf_vec[i] #sum the the costs to interact with all active partners and the phisiological costs 
     nb[i] <- total_benefit - total_cost
  }
  #new nodes state {1,0}
  new_state <- ifelse(nb > 0, 1, 0)
  return(new_state)
}
  
#iterate
simulation <- function(A, alpha, n_steps = 100){
  matrices <- model(A, alpha)
  B_mat <- matrices$B_mat
  C_mat <- matrices$C_mat
  Cf_vec <- matrices$Cf_vec
  
  state <- rep(1, nrow(A)) #all species starts as present in network (1)
  
  for (t in n_steps) {
    new_state <- actualize_state(A, state, B_mat, C_mat, Cf_vec)
    if (all(new_state == state)) break 
    state <- new_state
      
  }
  # Calculate the functional interactions matrix (between active species)
  A_active <- A * (state %o% state) #using the outer product to create a n x n matrix with the values in each position is the product of the states of the nodes interacting in A
  #here, we only obtain an 1 value (maintenence of interaction) if the state of two interacting nodes are 1 (active)
  #Therefore, if one of the species are 0 (extinct) the interaction will disappear.



  #proportion of active species
  prop_active_species <- (sum(state) / n)
  
  #proportion of remainig interactions
  total_inicial_interactions <- sum(A)
  active_interactions <- sum(A_active)
  prop_remaining_interactions <- active_interactions / total_inicial_interactions
  
  
 return(list(
  prop_active_species,
  prop_remaining_interactions
 ))
  
}
