## Main functions for the structured case

## Build a graph with a core (all connected) and peripheral structure
## core nodes are nodes_ci (number of rows) and node_cj (number of columns)
build_corep_graph <- function(nodes_ci, nodes_cj, nodes_pi, nodes_pj,
                              expected_connectance, return_adjacency=TRUE){
  
  nodes_i <- nodes_ci+nodes_pi
  nodes_j <- nodes_cj+nodes_pj
  
  # Check for minimal connectance given core
  ct_min <- min(c(1/nodes_ci, 1/nodes_cj))
  
  connec_pc <- expected_connectance
  
  # Value needs to be at least this minimum
  if(expected_connectance<=ct_min){
    cat("connectance needs to be greater than:", ct_min, 
               "\n setting it to:", ct_min+0.01, "\n")
    connec_pc <- ct_min+0.01
  }
  
  # Build the network
   # 1. Core species (all connected)
   A_core <- matrix(1, nodes_ci, nodes_cj)
   # 2. The first periphery (periphery sp. randomly connected to core)
   all_p_connected <- FALSE
   i <- 1
   while(!all_p_connected){
    B_perif_col <- (matrix(runif(nodes_ci*nodes_pj), 
                           nodes_ci, nodes_pj) <= connec_pc)
    all_p_connected <- all(colSums(B_perif_col)>0)
    i <- i+1
    if(i>10000){
      connec_pc <- connec_pc+0.01
      i <- 1
      cat("\n set connectance cols:", connec_pc)
    }
   }
   B_perif_col <- B_perif_col*1
   
   
   # 3. The second periphery (periphery sp. randomly connected to core)
   connec_pc <- expected_connectance
   
   all_p_connected <- FALSE
   i <- 1
   while(!all_p_connected){
     B_perif_row <- (matrix(runif(nodes_cj*nodes_pi), 
                            nodes_pi, nodes_cj) <= connec_pc)
     all_p_connected <- all(rowSums(B_perif_row)>0)
     i <- i+1
     if(i>10000){
       connec_pc <- connec_pc+0.01
       i <- 1
       cat("\n set connectance rows:", connec_pc, "\n")
     }
   }
   B_perif_row <- B_perif_row*1
   
   
   
   # 4. Assemble the parts
   A <- matrix(0, nrow = nodes_ci+nodes_pi, ncol = nodes_cj+nodes_pj)
   A[1:nodes_ci, 1:nodes_cj] <- A_core
   A[(nodes_ci+1):(nodes_ci+nodes_pi), 1:nodes_cj] <- B_perif_row
   A[1:nodes_ci, (nodes_cj+1):(nodes_cj+nodes_pj)] <- B_perif_col
  
   rownames(A) <- c(paste0("spi_c", 1:nodes_ci), paste0("spi_p", 1:nodes_pi))
   colnames(A) <- c(paste0("spj_c", 1:nodes_cj), paste0("spj_p", 1:nodes_pj))
   
  # To return the adjacency (square) matrix 
   if(return_adjacency){
     A <- cbind(rbind(matrix(0,nodes_i,nodes_i), t(A)),
                rbind(A, matrix(0,nodes_j,nodes_j)))
     rownames(A) <- c(paste0("spi_c", 1:nodes_ci), paste0("spi_p", 1:nodes_pi), 
                      paste0("spj_c", 1:nodes_cj), paste0("spj_p", 1:nodes_pj))
     colnames(A) <- c(paste0("spi_c", 1:nodes_ci), paste0("spi_p", 1:nodes_pi), 
                      paste0("spj_c", 1:nodes_cj), paste0("spj_p", 1:nodes_pj))
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


# Get ratio between costs and benefits
get_net_benefit <- function(eco_costs, physio_costs, tot_benefits){
  fitness_benefit <- rowSums(tot_benefits - eco_costs) - physio_costs
  return(fitness_benefit)
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
    CB_net <- get_net_benefit(newC, new_Cp, newB)  
    
    tmp <- tibble(time_step = i,
                  sp_id = sp_names,
                  sp_k_initial = sp_degree_initial,
                  sp_k_curr = sp_degree,
                  net_benefit = CB_net)
    
    res <- rbind(res, tmp)
    
  }
  return(res)
}
