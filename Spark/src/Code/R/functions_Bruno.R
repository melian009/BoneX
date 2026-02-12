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
# FUNÇÃO: IDENTIFICAR ESPÉCIES CENTRAIS E PERIFÉRICAS
# =============================================================================
# Esta função calcula o grau (número de conexões) de cada espécie na rede
# e classifica elas como centrais ou periféricas

identify_core_periphery <- function(A, threshold_percentile = 0.5) {
  # A = matriz de adjacência (rede de interações)
  # threshold_percentile = percentil para separar central de periférica
  #   - 0.5 = mediana (50% superior são centrais)
  #   - 0.7 = 70% superior são centrais (mais restritivo)
  
  # Calcular o grau de cada espécie (quantas conexões ela tem)
  # rowSums conta quantos 1s existem em cada linha
  # colSums conta quantos 1s existem em cada coluna
  # Somamos os dois porque a rede pode ser direcionada
  degree <- rowSums(A) + colSums(A)
  
  # Nomear o vetor com os nomes das espécies
  names(degree) <- rownames(A)
  
  # Calcular o limiar (threshold) baseado no percentil escolhido
  # quantile() encontra o valor que separa os dados no percentil desejado
  threshold_value <- quantile(degree, probs = threshold_percentile)
  
  # Classificar cada espécie
  # Se o grau >= threshold, é CENTRAL, senão é PERIFÉRICA
  classification <- ifelse(degree >= threshold_value, "core", "periphery")
  
  # Retornar uma lista com:
  # - degree: vetor com o grau de cada espécie
  # - classification: vetor com a classificação (core/periphery)
  # - threshold: valor do limiar usado
  # - core_species: índices das espécies centrais
  # - periphery_species: índices das espécies periféricas
  return(list(
    degree = degree,
    classification = classification,
    threshold = threshold_value,
    core_species = which(classification == "core"),
    periphery_species = which(classification == "periphery")
  ))
}

# -----------------------------------------------------------------------------
# EXEMPLO DE USO:
# -----------------------------------------------------------------------------
# Criar uma rede modular de exemplo
set.seed(123)
test_network <- interaction_networks(sp_n = 20, type = "modular")

# Identificar espécies centrais e periféricas
core_periphery <- identify_core_periphery(test_network, threshold_percentile = 0.5)

# Ver os resultados:
print(core_periphery$degree)           # Grau de cada espécie
print(core_periphery$classification)   # Classificação de cada uma
print(core_periphery$core_species)     # Quais são as centrais (índices)
print(core_periphery$periphery_species) # Quais são as periféricas (índices)

