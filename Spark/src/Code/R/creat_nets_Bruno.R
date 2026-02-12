# =============================================================================
# MASTER FUNCTION: CREAT INTERACTIONS NETWORKS
# =============================================================================
# =============================================================================
# FUNÇÃO: CRIAR REDE MUTUALISTA BIPARTIDA (FORMA CORRETA)
# =============================================================================

mutualistic_network_bipartite <- function(
    n_group1,  # Número de espécies do grupo 1 (ex: plantas)
    n_group2,  # Número de espécies do grupo 2 (ex: polinizadores)
    type = "random",
    connectance = 0.3,
    n_modules = 3,
    internal_connectance = 0.8,
    external_connectance = 0.05,
    nested_degree = 0.8,
    nested_min_connectance = 0.1,
    nested_max_connectance = 0.9) {
  
  # Criar matriz bipartida: n_group1 linhas × n_group2 colunas
  B <- switch(type,
              
              # ============================================================
              # RANDOM: Conexões aleatórias
              # ============================================================
              "random" = {
                B <- matrix(
                  rbinom(n = n_group1 * n_group2, size = 1, prob = connectance),
                  nrow = n_group1, 
                  ncol = n_group2
                )
                B
              },
              
              # ============================================================
              # MODULAR: Grupos tendem a interagir dentro do módulo
              # ============================================================
              "modular" = {
                B <- matrix(0, nrow = n_group1, ncol = n_group2)
                
                # Dividir grupo 1 em módulos
                module_size1 <- floor(n_group1 / n_modules)
                modules1 <- rep(1:n_modules, each = module_size1)
                if (length(modules1) < n_group1) {
                  modules1 <- c(modules1, rep(n_modules, n_group1 - length(modules1)))
                }
                
                # Dividir grupo 2 em módulos
                module_size2 <- floor(n_group2 / n_modules)
                modules2 <- rep(1:n_modules, each = module_size2)
                if (length(modules2) < n_group2) {
                  modules2 <- c(modules2, rep(n_modules, n_group2 - length(modules2)))
                }
                
                # Criar conexões
                for (i in 1:n_group1) {
                  for (j in 1:n_group2) {
                    # Mesma módulo = alta conectância
                    if (modules1[i] == modules2[j]) {
                      if (runif(1) < internal_connectance) {
                        B[i, j] <- 1
                      }
                    } else {
                      # Módulos diferentes = baixa conectância
                      if (runif(1) < external_connectance) {
                        B[i, j] <- 1
                      }
                    }
                  }
                }
                
                attr(B, "modules_group1") <- modules1
                attr(B, "modules_group2") <- modules2
                B
              },
              
              # ============================================================
              # NESTED: Generalistas interagem com todos, especialistas com poucos
              # ============================================================
              "nested" = {
                B <- matrix(0, nrow = n_group1, ncol = n_group2)
                
                for (i in 1:n_group1) {
                  # Espécies com índice baixo = generalistas (muitas conexões)
                  base_prob_i <- nested_max_connectance - 
                    ((i - 1) / n_group1) * (nested_max_connectance - nested_min_connectance) * nested_degree
                  
                  for (j in 1:n_group2) {
                    base_prob_j <- nested_max_connectance - 
                      ((j - 1) / n_group2) * (nested_max_connectance - nested_min_connectance) * nested_degree
                    
                    # Probabilidade combinada
                    connect_prob <- base_prob_i * base_prob_j
                    
                    if (runif(1) < connect_prob) {
                      B[i, j] <- 1
                    }
                  }
                }
                
                attr(B, "nested_degree") <- nested_degree
                B
              },
              
              # ============================================================
              # STAR: Uma espécie central conecta com todas do outro grupo
              # ============================================================
              "star" = {
                B <- matrix(0, nrow = n_group1, ncol = n_group2)
                
                # Primeira espécie do grupo 1 conecta com todas do grupo 2
                B[1, ] <- 1
                
                # Outras espécies têm poucas conexões aleatórias
                for (i in 2:n_group1) {
                  n_connections <- rbinom(1, n_group2, 0.1)
                  if (n_connections > 0) {
                    connections <- sample(1:n_group2, n_connections)
                    B[i, connections] <- 1
                  }
                }
                B
              }
  )
  
  # Nomear linhas e colunas
  rownames(B) <- paste0("P", 1:n_group1)  # P = plants
  colnames(B) <- paste0("A", 1:n_group2)  # A = animals
  
  # Adicionar atributos
  attr(B, "type") <- type
  attr(B, "n_group1") <- n_group1
  attr(B, "n_group2") <- n_group2
  attr(B, "is_bipartite") <- TRUE
  
  return(B)
}

# =============================================================================
# EXEMPLOS:
# =============================================================================

set.seed(123)

# Rede random: 10 plantas × 15 polinizadores
random_bip <- mutualistic_network_bipartite(
  n_group1 = 10, 
  n_group2 = 15, 
  type = "random",
  connectance = 0.3
)

print(random_bip)
print(dim(random_bip))  # 10 × 15

# Rede modular
modular_bip <- mutualistic_network_bipartite(
  n_group1 = 12, 
  n_group2 = 18, 
  type = "modular",
  n_modules = 3,
  internal_connectance = 0.8,
  external_connectance = 0.05
)

print(modular_bip)

# Rede nested
nested_bip <- mutualistic_network_bipartite(
  n_group1 = 10, 
  n_group2 = 15, 
  type = "nested",
  nested_degree = 0.8
)

print(nested_bip)



# =============================================================================
# FUNÇÃO: CONVERTER REDE BIPARTIDA EM QUADRADA (UNIPARTIDA)
# =============================================================================

bipartite_to_square <- function(B) {
  # B = matriz bipartida (n_group1 × n_group2)
  
  # Pegar dimensões
  n1 <- nrow(B)  # Número de espécies grupo 1
  n2 <- ncol(B)  # Número de espécies grupo 2
  n_total <- n1 + n2
  
  # Criar matriz quadrada vazia
  A <- matrix(0, nrow = n_total, ncol = n_total)
  
  # Preencher bloco superior direito com a matriz bipartida
  # (grupo 1 → grupo 2)
  A[1:n1, (n1+1):n_total] <- B
  
  # Preencher bloco inferior esquerdo (simetria)
  # (grupo 2 → grupo 1)
  A[(n1+1):n_total, 1:n1] <- t(B)
  
  # Nomear linhas e colunas
  row_names <- c(rownames(B), colnames(B))
  colnames(A) <- row_names
  rownames(A) <- row_names
  
  # Adicionar atributos
  attr(A, "type") <- attr(B, "type")
  attr(A, "is_bipartite_converted") <- TRUE
  attr(A, "n_group1") <- n1
  attr(A, "n_group2") <- n2
  attr(A, "original_bipartite") <- B
  
  return(A)
}

# =============================================================================
# EXEMPLO DE CONVERSÃO:
# =============================================================================

# Criar rede bipartida
set.seed(123)
bip <- mutualistic_network_bipartite(n_group1 = 5, n_group2 = 7, type = "random")

print("REDE BIPARTIDA (5 × 7):")
print(bip)

# Converter para quadrada
square <- bipartite_to_square(bip)

print("REDE QUADRADA (12 × 12):")
print(square)

# Verificar simetria
print(paste("É simétrica?", isSymmetric(square)))

# Visualizar estrutura
print("Estrutura da matriz quadrada:")
print("- Bloco superior esquerdo (5×5): zeros (plantas não interagem com plantas)")
print("- Bloco superior direito (5×7): interações plantas → polinizadores")
print("- Bloco inferior esquerdo (7×5): interações polinizadores → plantas (espelho)")
print("- Bloco inferior direito (7×7): zeros (polinizadores não interagem com polinizadores)")



