# =============================================================================
# FUNÇÃO: IDENTIFICAR CORE E PERIFERIA EM REDE BIPARTIDA
# =============================================================================
#
# DESCRIÇÃO:
#   Classifica espécies como "core" ou "periphery" com base no grau (número
#   de interações) na rede bipartida original. O critério é aplicado
#   SEPARADAMENTE para cada grupo (ex: plantas e animais/polinizadores),
#   garantindo que ambos os lados da rede estejam representados no core.
#
# INPUT:
#   B                   — matriz bipartida (plantas x animais), binária ou ponderada
#   threshold_percentile — percentil de corte (default = 0.5 → top 50% = core)
#                          ex: 0.75 → apenas o top 25% é core (mais restrito)
#
# OUTPUT: lista com os seguintes elementos
#   $core_species        — índices das espécies core na matriz QUADRADA
#   $periphery_species   — índices das espécies periféricas na matriz QUADRADA
#   $core_group1         — índices core do grupo 1 (plantas) na matriz quadrada
#   $periphery_group1    — índices periféricos do grupo 1
#   $core_group2         — índices core do grupo 2 (animais) na matriz quadrada
#   $periphery_group2    — índices periféricos do grupo 2
#   $degree              — vetor de grau de todas as espécies (na matriz quadrada)
#   $degree_group1       — grau das espécies do grupo 1
#   $degree_group2       — grau das espécies do grupo 2
#   $classification      — vetor nomeado: "core" ou "periphery" para cada espécie
#   $n_group1            — número de espécies no grupo 1
#   $n_group2            — número de espécies no grupo 2
#   $threshold_used      — percentil usado como corte
#
# NOTA SOBRE ÍNDICES:
#   A rede quadrada é construída como:
#     [ 0       B   ]   linhas 1:n_group1       → grupo 1 (plantas)
#     [ t(B)    0   ]   linhas (n_group1+1):n   → grupo 2 (animais)
#   Os índices retornados já refletem essa numeração quadrada.
#
# =============================================================================

identify_core_periphery_bipartite <- function(B,
                                               threshold_percentile = 0.5) {
  
  # ---------------------------------------------------------------------------
  # 1. Validações básicas
  # ---------------------------------------------------------------------------
  if (!is.matrix(B)) B <- as.matrix(B)
  
  n_group1 <- nrow(B)   # plantas (linhas da bipartida)
  n_group2 <- ncol(B)   # animais (colunas da bipartida)
  n_total  <- n_group1 + n_group2
  
  if (n_group1 == 0 || n_group2 == 0) {
    stop("A matriz bipartida não pode ter dimensão zero em nenhum grupo.")
  }
  if (threshold_percentile <= 0 || threshold_percentile >= 1) {
    stop("threshold_percentile deve estar entre 0 e 1 (exclusivo).")
  }
  
  # ---------------------------------------------------------------------------
  # 2. Calcular grau de cada espécie na rede bipartida
  #    Grupo 1 (plantas):  grau = número de colunas não-zero em cada linha
  #    Grupo 2 (animais):  grau = número de linhas não-zero em cada coluna
  # ---------------------------------------------------------------------------
  
  # Binarizar para o cálculo de grau (funciona mesmo em redes ponderadas)
  B_bin <- B
  B_bin[B_bin > 0] <- 1
  
  degree_group1 <- rowSums(B_bin)   # grau das plantas
  degree_group2 <- colSums(B_bin)   # grau dos animais
  
  # Grau na ordem da matriz quadrada: [plantas | animais]
  degree_all <- c(degree_group1, degree_group2)
  names(degree_all) <- c(
    paste0("P", seq_len(n_group1)),
    paste0("A", seq_len(n_group2))
  )
  
  # ---------------------------------------------------------------------------
  # 3. Definir limiar de corte — separado por grupo
  #    Espécies com grau >= limiar são "core"
  # ---------------------------------------------------------------------------
  
  cutoff_group1 <- quantile(degree_group1, probs = threshold_percentile)
  cutoff_group2 <- quantile(degree_group2, probs = threshold_percentile)
  
  # ---------------------------------------------------------------------------
  # 4. Classificar espécies
  # ---------------------------------------------------------------------------
  
  # Índices LOCAIS dentro de cada grupo
  core_local_g1      <- which(degree_group1 >= cutoff_group1)
  periphery_local_g1 <- which(degree_group1 <  cutoff_group1)
  
  core_local_g2      <- which(degree_group2 >= cutoff_group2)
  periphery_local_g2 <- which(degree_group2 <  cutoff_group2)
  
  # Converter para índices na matriz QUADRADA
  # Grupo 1 ocupa posições 1:n_group1
  # Grupo 2 ocupa posições (n_group1+1):(n_group1+n_group2)
  core_g1_square      <- core_local_g1
  periphery_g1_square <- periphery_local_g1
  
  core_g2_square      <- core_local_g2      + n_group1
  periphery_g2_square <- periphery_local_g2 + n_group1
  
  # Índices globais (todos os core e todos os periféricos)
  core_all      <- c(core_g1_square,      core_g2_square)
  periphery_all <- c(periphery_g1_square, periphery_g2_square)
  
  # Vetor de classificação nomeado
  classification <- rep("periphery", n_total)
  classification[core_all] <- "core"
  names(classification) <- names(degree_all)
  
  # ---------------------------------------------------------------------------
  # 5. Retornar resultados
  # ---------------------------------------------------------------------------
  
  return(list(
    # Índices na matriz quadrada
    core_species      = core_all,
    periphery_species = periphery_all,
    core_group1       = core_g1_square,
    periphery_group1  = periphery_g1_square,
    core_group2       = core_g2_square,
    periphery_group2  = periphery_g2_square,
    
    # Graus
    degree       = degree_all,
    degree_group1 = degree_group1,
    degree_group2 = degree_group2,
    
    # Classificação e metadados
    classification    = classification,
    n_group1          = n_group1,
    n_group2          = n_group2,
    threshold_used    = threshold_percentile,
    cutoff_group1     = cutoff_group1,
    cutoff_group2     = cutoff_group2
  ))
}


# =============================================================================
# EXEMPLO DE USO
# =============================================================================

# # Rede bipartida simulada (10 plantas x 15 animais)
# set.seed(42)
# B <- matrix(rbinom(10 * 15, 1, 0.3), nrow = 10, ncol = 15)
# rownames(B) <- paste0("P", 1:10)
# colnames(B) <- paste0("A", 1:15)
#
# cp <- identify_core_periphery_bipartite(B, threshold_percentile = 0.5)
#
# cat("Core (índices quadrados):", cp$core_species, "\n")
# cat("Periféricos (índices quadrados):", cp$periphery_species, "\n")
# cat("Core plantas:", cp$core_group1, "\n")
# cat("Core animais:", cp$core_group2, "\n")
# cat("Grau médio core:", mean(cp$degree[cp$core_species]), "\n")
# cat("Grau médio periféricos:", mean(cp$degree[cp$periphery_species]), "\n")
# cat("Classificação:\n")
# print(cp$classification)
