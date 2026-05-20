# =============================================================================
# FUNÇÃO: CALCULAR MÉTRICAS ESTRUTURAIS DE REDES BIPARTIDAS REAIS
# =============================================================================
#
# DESCRIÇÃO:
#   Recebe uma matriz bipartida binária e calcula as principais métricas
#   estruturais usadas como variáveis preditoras no modelo:
#     - Riqueza (plantas, animais, total)
#     - Número de interações
#     - Conectância
#     - Grau médio (plantas, animais, total)
#     - Aninhamento: NODF (via pacote bipartite)
#     - Modularidade: Q (via pacote bipartite)
#
# INPUT:
#   B           — matriz bipartida binária (plantas × animais)
#   nome        — nome da rede (string), usado para identificação no output
#   calc_nodf   — calcular NODF? (default TRUE; pode ser FALSE para redes grandes)
#   calc_mod    — calcular modularidade Q? (default TRUE; mais lento)
#
# OUTPUT:
#   tibble com 1 linha e todas as métricas como colunas
#
# DEPENDÊNCIAS:
#   install.packages("bipartite")
# =============================================================================

library(bipartite)
library(dplyr)

calcular_metricas_rede <- function(B,
                                   nome = "rede",
                                   calc_nodf = TRUE,
                                   calc_mod  = TRUE) {
  
  # ---------------------------------------------------------------------------
  # 1. Validações e binarização
  # ---------------------------------------------------------------------------
  if (!is.matrix(B)) B <- as.matrix(B)
  B[B > 0] <- 1
  B[is.na(B)] <- 0
  
  n_plantas <- nrow(B)
  n_animais <- ncol(B)
  n_total   <- n_plantas + n_animais
  
  if (n_plantas == 0 || n_animais == 0) {
    warning(paste("Rede", nome, "tem dimensão zero — retornando NAs."))
    return(tibble(
      nome_rede = nome, n_plantas = n_plantas, n_animais = n_animais,
      n_total = n_total, n_interacoes = NA, conectancia = NA,
      grau_medio_plantas = NA, grau_medio_animais = NA, grau_medio_total = NA,
      NODF = NA, Q = NA
    ))
  }
  
  # ---------------------------------------------------------------------------
  # 2. Métricas básicas
  # ---------------------------------------------------------------------------
  n_interacoes       <- sum(B)
  conectancia        <- n_interacoes / (n_plantas * n_animais)
  grau_medio_plantas <- mean(rowSums(B))
  grau_medio_animais <- mean(colSums(B))
  grau_medio_total   <- mean(c(rowSums(B), colSums(B)))
  
  # ---------------------------------------------------------------------------
  # 3. Aninhamento — NODF
  # ---------------------------------------------------------------------------
  NODF_val <- NA
  if (calc_nodf) {
    tryCatch({
      nodf_result <- bipartite::networklevel(B, index = "NODF")
      NODF_val    <- as.numeric(nodf_result["NODF"])
    }, error = function(e) {
      warning(paste("NODF falhou para", nome, ":", e$message))
    })
  }
  
  # ---------------------------------------------------------------------------
  # 4. Modularidade — Q
  # ---------------------------------------------------------------------------
  Q_val <- NA
  if (calc_mod) {
    tryCatch({
      # computeModules usa o algoritmo de Dormann & Strauss (2014)
      mod_result <- bipartite::computeModules(B)
      Q_val      <- mod_result@likelihood   # Q = índice de modularidade
    }, error = function(e) {
      warning(paste("Modularidade falhou para", nome, ":", e$message))
    })
  }
  
  # ---------------------------------------------------------------------------
  # 5. Retornar tibble com 1 linha
  # ---------------------------------------------------------------------------
  tibble(
    nome_rede          = nome,
    n_plantas          = n_plantas,
    n_animais          = n_animais,
    n_total            = n_total,
    n_interacoes       = n_interacoes,
    conectancia        = conectancia,
    grau_medio_plantas = grau_medio_plantas,
    grau_medio_animais = grau_medio_animais,
    grau_medio_total   = grau_medio_total,
    NODF               = NODF_val,
    Q                  = Q_val
  )
}


# =============================================================================
# FUNÇÃO AUXILIAR: CALCULAR MÉTRICAS PARA TODAS AS REDES DE UMA VEZ
# =============================================================================
#
# Recebe uma lista nomeada de matrizes bipartidas e retorna um dataframe
# com as métricas de todas as redes empilhadas.
#
# INPUT:
#   lista_redes — lista nomeada: list(nome1 = matriz1, nome2 = matriz2, ...)
#   calc_nodf   — calcular NODF? (default TRUE)
#   calc_mod    — calcular Q? (default TRUE)
#   verbose     — imprimir progresso? (default TRUE)
#
# OUTPUT:
#   dataframe com todas as redes e suas métricas

calcular_metricas_todas <- function(lista_redes,
                                    calc_nodf = TRUE,
                                    calc_mod  = TRUE,
                                    verbose   = TRUE) {
  
  nomes  <- names(lista_redes)
  n_redes <- length(lista_redes)
  
  if (is.null(nomes)) {
    nomes <- paste0("rede_", seq_len(n_redes))
  }
  
  resultados <- vector("list", n_redes)
  
  for (i in seq_len(n_redes)) {
    
    if (verbose) {
      cat(sprintf("  [%d/%d] Calculando métricas: %s\n", i, n_redes, nomes[i]))
    }
    
    resultados[[i]] <- tryCatch(
      calcular_metricas_rede(
        B        = lista_redes[[i]],
        nome     = nomes[i],
        calc_nodf = calc_nodf,
        calc_mod  = calc_mod
      ),
      error = function(e) {
        warning(paste("Erro em", nomes[i], ":", e$message))
        tibble(nome_rede = nomes[i], n_plantas = NA, n_animais = NA,
               n_total = NA, n_interacoes = NA, conectancia = NA,
               grau_medio_plantas = NA, grau_medio_animais = NA,
               grau_medio_total = NA, NODF = NA, Q = NA)
      }
    )
  }
  
  bind_rows(resultados)
}


# =============================================================================
# EXEMPLO DE USO
# =============================================================================

# # Rede simulada para teste
# set.seed(42)
# B_teste <- matrix(rbinom(10 * 15, 1, 0.3), nrow = 10, ncol = 15)
# rownames(B_teste) <- paste0("P", 1:10)
# colnames(B_teste) <- paste0("A", 1:15)
#
# metricas <- calcular_metricas_rede(B_teste, nome = "rede_teste")
# print(metricas)
#
# # Para múltiplas redes de uma vez:
# lista <- list(rede_a = B_teste, rede_b = B_teste[1:5, 1:8])
# todas_metricas <- calcular_metricas_todas(lista)
# print(todas_metricas)
