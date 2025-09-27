# =============================================================================
# FUNCTIONS TO CREAT DIFFERENT NETWROK STRUCTURES
# =============================================================================

# 1.1 Random Networks

rnet = function(sp_n, connectance = 0.3) {
  A = matrix(rbinom(n = sp_n^2, size = 1,prob = connectance),
             nrow = sp_n, ncol = sp_n)
  diag(A) = 0
  rownames(A) = paste0("sp", 1:sp_n)
  colnames(A) = paste0("sp", 1:sp_n)
  return(A)
            
}

# 1.2 Star net

star_net = function(sp_n, center = 1){
  A = matrix(0, nrow = sp_n, ncol = sp_n)
  #connect species to central node
  A[center, -center] = 1 #center -> others
  A[-center, center] = 1 #others -> center
  
  diag(A) = 0
  rownames(A) = paste0("sp", 1:sp_n)
  colnames(A) = paste0("sp", 1:sp_n)
  return(A)
}


# 1.3 Modular net

modular_net = function(sp_n, n_modules = 3, internal_connectance = 0.8, external_connectance = 0.05){
  
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
    
  rownames(A) = paste0("sp", 1:sp_n)
  colnames(A) = paste0("sp", 1:sp_n)
  
  #add modules information
  attr(A, "modules") = modules
  attr(A, "type") = "modular"
  return(A)
}

#==================================================
random = rnet(20)
random  

star = star_net(20)
star

modular = modular_net(20)
modular
