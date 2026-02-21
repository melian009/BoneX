library(tidyverse)

#===============================================================================
# Inputing or creating network
network <- function(N, conectancia) {
  A <- matrix(rbinom(N*N, 1, conectancia), nrow = N, ncol = N)
  diag(A) <- 0
  return(A)
}


#===============================================================================
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

#===============================================================================
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


#===============================================================================
# Physiological cost (Cf is just to distinguish from Cp - fisiologico / physiological )
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


#===============================================================================

# =============================================================================
# AJUSTE FINAL: CRIANDO A FUNÇÃO QUE O GRID PROCURA
# =============================================================================

identify_core_periphery_bipartite <- function(B, threshold_percentile = 0.5) {
  
  # 1. Identifica Core/Periphery para as PLANTAS (Linhas de B)
  # Usamos a sua lógica original aplicada às linhas
  deg_p <- rowSums(B > 0)
  thresh_p <- quantile(deg_p, probs = threshold_percentile)
  class_p <- ifelse(deg_p >= thresh_p, "core", "periphery")
  
  # 2. Identifica Core/Periphery para os POLINIZADORES (Colunas de B)
  # Usamos a sua lógica original aplicada às colunas
  deg_a <- colSums(B > 0)
  thresh_a <- quantile(deg_a, probs = threshold_percentile)
  class_a <- ifelse(deg_a >= thresh_a, "core", "periphery")
  
  # 3. Organiza os índices para a REDE QUADRADA (que terá N_plantas + N_pols)
  # Isso é vital porque o Grid trabalha com a rede 25x25 depois
  n_p <- nrow(B)
  core_sp_total <- c(which(class_p == "core"), which(class_a == "core") + n_p)
  peri_sp_total <- c(which(class_p == "periphery"), which(class_a == "periphery") + n_p)
  
  # Retorna a lista EXATA que os Passos 8 e 9 do seu Grid esperam ler
  return(list(
    degree = c(deg_p, deg_a),
    core_species = core_sp_total,
    periphery_species = peri_sp_total,
    
    # Informações detalhadas por grupo (para o Passo 9)
    degree_group1 = deg_p,
    classification_group1 = class_p,
    core_group1 = which(class_p == "core"),
    periphery_group1 = which(class_p == "periphery"),
    
    degree_group2 = deg_a,
    classification_group2 = class_a,
    core_group2 = which(class_a == "core"),
    periphery_group2 = which(class_a == "periphery")
  ))
}


