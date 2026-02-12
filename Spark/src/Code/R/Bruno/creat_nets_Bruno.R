# =============================================================================
# MASTER FUNCTION: CREAT INTERACTIONS NETWORKS
# =============================================================================

interaction_networks = function(sp_n,
                                type = "random",
                                connectance = 0.3, center = 1,
                                n_modules = 3,
                                internal_connectance = 0.8, 
                                external_connectance = 0.05,
                                nested_degree = 0.8,
                                nested_min_connectance = 0.1,
                                nested_max_connectance = 0.9){
  A = switch (type,
              "random" = {
                
                A = matrix(rbinom(n = sp_n^2, size = 1,prob = connectance),
                           nrow = sp_n, ncol = sp_n)
                diag(A) = 0
                A
              },
              
              "star" = {
                
                A = matrix(0, nrow = sp_n, ncol = sp_n)
                #connect species to central node
                A[center, -center] = 1 #center -> others
                A[-center, center] = 1 #others -> center
                
                diag(A) = 0
                A
              },
              
              "modular" = {
                A = matrix(0, nrow = sp_n, ncol = sp_n)
                
                #dividing in modules size
                module_size = floor(sp_n / n_modules)
                modules = rep(1:n_modules, each = module_size)
                if (length(modules) < sp_n) {
                  modules <- c(modules, rep(n_modules, sp_n - length(modules))) #if we have species without modules, integrate than to the last module
                }
                
                #creat conennections
                for (i in 1:sp_n) {
                  for(j in 1:sp_n){
                    if(i != j) {
                      if(modules[i] == modules[j]){
                        if (runif(1) < internal_connectance) A[i,j] = 1
                      } else { 
                        if (runif(1) < external_connectance) A[i,j] = 1}
                    }
                  }
                }
                attr(A, "modules") = modules
                attr(A, "type") = "modular"
                A
              },
              
              "path" = {
                A = matrix(0, nrow = sp_n, ncol = sp_n)
                
                #creating sequencial connections
                for (i in 1:(sp_n - 1)) {
                  A[i, i + 1] = 1
                  A[1 + 1, i] = 1
                }
                A
              },
              
              "nested" = {
                A <- matrix(0, nrow = sp_n, ncol = sp_n)
                
                for(i in 1:sp_n) {
                  # Conectivity probability reduces with species index
                  base_prob_i = nested_max_connectance - 
                    ((i - 1) / sp_n) * (nested_max_connectance - nested_min_connectance) * nested_degree
                  
                  for(j in 1:sp_n) {
                    if(i != j) {
                      base_prob_j = nested_max_connectance - 
                        ((j - 1) / sp_n) * (nested_max_connectance - nested_min_connectance) * nested_degree
                      
                      # Probabilidade combinada (quanto menor o índice, mais conectado)
                      conect_prob = base_prob_i * base_prob_j
                      
                      if(runif(1) < conect_prob) {
                        A[i, j] = 1
                      }
                    }
                  }
                }
                
                attr(A, "nested_degree") = nested_degree
                A
              }
              
  )
  
  rownames(A) = paste0("sp", 1:sp_n)
  colnames(A) = paste0("sp", 1:sp_n)
  attr(A, "type") = type
  
  return(A)
}


#==============================================================================
#Examples

set.seed(123)


random = interaction_networks(sp_n = 20,type = "random")
star = interaction_networks(sp_n = 5, type = "star")
modular = interaction_networks(sp_n = 20,type = "modular")
nested = interaction_networks(sp_n = 20,type = "nested")
path = interaction_networks(sp_n = 20, type = "path")
