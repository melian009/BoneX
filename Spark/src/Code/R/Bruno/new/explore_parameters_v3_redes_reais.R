# =============================================================================
# FUNÇÃO: LOOP PRINCIPAL — REDES REAIS
# =============================================================================
#
# DESCRIÇÃO:
#   Versão adaptada do explore_parameters_v3() para usar redes REAIS
#   obtidas da literatura (em vez de redes simuladas). Para cada rede real,
#   varia o tipo de provedor de serviços (core / periphery / random)
#   e roda n_replicates simulações do modelo Booleano.
#
# INPUTS:
#   lista_redes          — lista nomeada de matrizes BIPARTIDAS binárias
#                          ex: list(rede1 = matriz1, rede2 = matriz2, ...)
#   metricas_redes       — dataframe com métricas pré-calculadas de cada rede
#                          (output de calcular_metricas_todas())
#                          deve ter coluna 'nome_rede' como chave
#   service_providers_options — vetor com tipos de provedor a variar
#                               default: c("core", "periphery", "random")
#   n_services           — número de serviços ecossistêmicos (default 5)
#   threshold_percentile — percentil de corte core/periphery (default 0.5)
#   B_shape1, B_shape2   — parâmetros beta para benefício (default 0.5, 0.5)
#   Ce_shape1, Ce_shape2 — parâmetros beta para custo ecológico
#   Cp_shape1, Cp_shape2 — parâmetros beta para custo fisiológico
#   Cp_multiplier        — multiplicador do custo fisiológico (default 0)
#   A_min, A_max         — amplitude ambiental (default 0 = sem variação)
#   w_min, w_max         — frequência ambiental (default 0)
#   t_max                — número máximo de passos de tempo (default 10000)
#   zi_min, zi_max       — intervalo do ótimo ambiental zi (default 1, 10)
#   connectance_ES       — conectância da rede de serviços (default 0.3)
#   distribution_ES      — distribuição dos pesos de serviços (default "lognormal")
#   n_replicates         — número de replicatas por combinação (default 5)
#   seed                 — semente base para reprodutibilidade (default 123)
#   verbose              — imprimir progresso? (default TRUE)
#
# OUTPUT:
#   dataframe com uma linha por simulação e colunas:
#     - identificação: sim_id, nome_rede, service_providers, replicate
#     - métricas da rede: conectância, NODF, Q, graus, n_especies...
#     - core/periphery: n_core, n_periphery, persistência por grupo
#     - dinâmica: persistence_species, persistence_interactions, convergência
#     - serviços: services_initial, services_final, loss, riqueza_servicos
#     - históricos (listas): prop_species_history, prop_interactions_history
#
# DEPENDÊNCIAS (carregar antes de rodar):
#   source("functions_Bruno.R")
#   source("model_node_actualization.R")
#   source("eco_services_iteraction.R")
#   source("identify_core_periphery.R")
#   source("ecosystem_services_network_v2.R")
# =============================================================================

library(dplyr)

explore_parameters_v3_redes_reais <- function(
    
    # Redes reais
    lista_redes,
    metricas_redes           = NULL,
    
    # Grid de variação
    service_providers_options = c("core", "periphery", "random"),
    
    # Serviços ecossistêmicos
    n_services               = 5,
    connectance_ES           = 0.3,
    distribution_ES          = "lognormal",
    
    # Core/periphery
    threshold_percentile     = 0.5,
    
    # Parâmetros das espécies
    B_shape1                 = 0.5,
    B_shape2                 = 0.5,
    Ce_shape1                = 0.5,
    Ce_shape2                = 0.5,
    Cp_shape1                = 0,
    Cp_shape2                = 0,
    Cp_multiplier            = 0,
    
    # Ambiente
    A_min                    = 0,
    A_max                    = 0,
    w_min                    = 0,
    w_max                    = 0,
    t_max                    = 10000,
    zi_min                   = 1,
    zi_max                   = 10,
    
    # Replicatas
    n_replicates             = 5,
    seed                     = 123,
    verbose                  = TRUE
) {
  
  # ---------------------------------------------------------------------------
  # Validações
  # ---------------------------------------------------------------------------
  if (is.null(names(lista_redes))) {
    names(lista_redes) <- paste0("rede_", seq_along(lista_redes))
  }
  nomes_redes <- names(lista_redes)
  n_redes     <- length(lista_redes)
  
  # ---------------------------------------------------------------------------
  # Criar grid: rede × provedor × replicata
  # ---------------------------------------------------------------------------
  params <- expand.grid(
    nome_rede    = nomes_redes,
    service_prov = service_providers_options,
    replicate    = seq_len(n_replicates),
    stringsAsFactors = FALSE
  )
  n_total <- nrow(params)
  
  if (verbose) {
    cat("\n===========================================================\n")
    cat("MODELO BOOLEANO — REDES MUTUALISTAS REAIS\n")
    cat("===========================================================\n\n")
    cat(sprintf("Redes carregadas: %d\n", n_redes))
    cat(sprintf("Provedores de serviços: %s\n",
                paste(service_providers_options, collapse = ", ")))
    cat(sprintf("Replicatas por combinação: %d\n", n_replicates))
    cat(sprintf("Total de simulações: %d\n\n", n_total))
    cat("Iniciando...\n\n")
  }
  
  # ---------------------------------------------------------------------------
  # Loop principal
  # ---------------------------------------------------------------------------
  results_list <- vector("list", n_total)
  
  for (i in seq_len(n_total)) {
    
    if (verbose && (i %% 10 == 0 || i == n_total)) {
      cat(sprintf("  [%d/%d] %.1f%% — %s / %s / rep %d\n",
                  i, n_total, (i / n_total) * 100,
                  params$nome_rede[i], params$service_prov[i], params$replicate[i]))
    }
    
    set.seed(seed + i)
    p <- params[i, ]
    
    # -------------------------------------------------------------------------
    # PASSO 1: Recuperar rede bipartida
    # -------------------------------------------------------------------------
    B <- lista_redes[[p$nome_rede]]
    if (!is.matrix(B)) B <- as.matrix(B)
    B[B > 0] <- 1
    
    n_group1      <- nrow(B)   # plantas
    n_group2      <- ncol(B)   # animais
    n_species_total <- n_group1 + n_group2
    
    # -------------------------------------------------------------------------
    # PASSO 2: Converter bipartida → quadrada
    # -------------------------------------------------------------------------
    m1         <- cbind(matrix(0, n_group1, n_group1), B)
    m2         <- cbind(t(B), matrix(0, n_group2, n_group2))
    square_net <- rbind(m1, m2)
    diag(square_net) <- 0
    
    # -------------------------------------------------------------------------
    # PASSO 3: Identificar core/periphery
    # -------------------------------------------------------------------------
    cp_info <- identify_core_periphery_bipartite(
      B                    = B,
      threshold_percentile = threshold_percentile
    )
    
    # -------------------------------------------------------------------------
    # PASSO 4: Criar rede de serviços ecossistêmicos
    # -------------------------------------------------------------------------
    serv_net <- ecosystem_services_network_v2(
      sp_n                 = n_species_total,
      services_n           = n_services,
      mutualistic_network  = square_net,
      service_providers    = as.character(p$service_prov),
      threshold_percentile = threshold_percentile,
      connectance          = connectance_ES,
      distribution         = distribution_ES
    )
    
    # -------------------------------------------------------------------------
    # PASSO 5: Ambiente
    # -------------------------------------------------------------------------
    theta <- environment(A_min, A_max, w_min, w_max, t_max = t_max)
    
    # -------------------------------------------------------------------------
    # PASSO 6: Parâmetros das espécies
    # -------------------------------------------------------------------------
    B_vec  <- rbeta(n_species_total, B_shape1,  B_shape2)
    Ce_vec <- rbeta(n_species_total, Ce_shape1, Ce_shape2)
    
    # Cp pode ser zero (Cp_shape1 = Cp_shape2 = 0 → rbeta não funciona)
    if (Cp_multiplier == 0 || (Cp_shape1 == 0 && Cp_shape2 == 0)) {
      Cp_vec <- rep(0, n_species_total)
    } else {
      Cp_vec <- rbeta(n_species_total, Cp_shape1, Cp_shape2) * Cp_multiplier
    }
    
    zi <- runif(n_species_total, zi_min, zi_max)
    
    # -------------------------------------------------------------------------
    # PASSO 7: Simulação Booleana
    # -------------------------------------------------------------------------
    dynamics <- simulation(square_net, B_vec, Ce_vec, Cp_vec, zi, theta)
    
    # -------------------------------------------------------------------------
    # PASSO 8: Serviços ecossistêmicos
    # -------------------------------------------------------------------------
    services     <- ecosystem(dynamics, serv_net)
    e_final_vec  <- services$E_final
    riqueza_final <- sum(e_final_vec > 0.0001)
    
    # -------------------------------------------------------------------------
    # PASSO 9: Métricas de core/periphery pós-simulação
    # -------------------------------------------------------------------------
    final_state         <- dynamics$final_state
    n_core_total        <- length(cp_info$core_species)
    n_periphery_total   <- length(cp_info$periphery_species)
    n_core_group1       <- length(cp_info$core_group1)
    n_periphery_group1  <- length(cp_info$periphery_group1)
    n_core_group2       <- length(cp_info$core_group2)
    n_periphery_group2  <- length(cp_info$periphery_group2)
    core_survived       <- sum(final_state[cp_info$core_species])
    periphery_survived  <- sum(final_state[cp_info$periphery_species])
    core_persistence    <- core_survived / max(n_core_total, 1)
    periphery_persistence <- periphery_survived / max(n_periphery_total, 1)
    mean_degree_core    <- mean(cp_info$degree[cp_info$core_species])
    mean_degree_periphery <- mean(cp_info$degree[cp_info$periphery_species])
    
    # -------------------------------------------------------------------------
    # PASSO 10: Métricas da rede (do dataframe pré-calculado ou básicas)
    # -------------------------------------------------------------------------
    if (!is.null(metricas_redes) && p$nome_rede %in% metricas_redes$nome_rede) {
      m <- metricas_redes[metricas_redes$nome_rede == p$nome_rede, ]
      NODF_val    <- m$NODF
      Q_val       <- m$Q
      conn_val    <- m$conectancia
      n_int_val   <- m$n_interacoes
    } else {
      # Fallback: métricas básicas sem NODF/Q
      conn_val  <- sum(B) / (n_group1 * n_group2)
      n_int_val <- sum(B)
      NODF_val  <- NA
      Q_val     <- NA
    }
    
    # -------------------------------------------------------------------------
    # PASSO 11: Armazenar resultado
    # -------------------------------------------------------------------------
    temp_df <- data.frame(
      # Identificação
      sim_id           = i,
      nome_rede        = p$nome_rede,
      service_providers = as.character(p$service_prov),
      replicate        = p$replicate,
      
      # Estrutura da rede
      n_group1         = n_group1,
      n_group2         = n_group2,
      n_species_total  = n_species_total,
      n_interacoes     = n_int_val,
      conectancia      = conn_val,
      NODF             = NODF_val,
      Q                = Q_val,
      
      # Core/periphery
      n_core_total         = n_core_total,
      n_periphery_total    = n_periphery_total,
      n_core_group1        = n_core_group1,
      n_periphery_group1   = n_periphery_group1,
      n_core_group2        = n_core_group2,
      n_periphery_group2   = n_periphery_group2,
      mean_degree_core     = mean_degree_core,
      mean_degree_periphery = mean_degree_periphery,
      
      # Parâmetros do modelo
      A_min = A_min, A_max = A_max,
      w_min = w_min, w_max = w_max,
      Cp_multiplier = Cp_multiplier,
      threshold_percentile = threshold_percentile,
      
      # Dinâmica das espécies
      persistence_species      = dynamics$prop_active_species,
      persistence_interactions = dynamics$prop_remaining_interactions,
      n_species_final          = sum(dynamics$final_state),
      time_to_convergence      = nrow(dynamics$state_history),
      mean_degree_initial      = mean(dynamics$initial_degree),
      
      # Persistência por grupo
      core_persistence      = core_persistence,
      periphery_persistence = periphery_persistence,
      core_survived         = core_survived,
      periphery_survived    = periphery_survived,
      
      # Serviços ecossistêmicos
      services_initial       = sum(services$E_initial),
      services_final         = sum(services$E_final),
      services_loss          = sum(services$delta_E),
      services_loss_relative = sum(services$delta_E) / max(sum(services$E_initial), 1e-10),
      riqueza_servicos_final = riqueza_final,
      
      # Históricos (listas)
      prop_species_history      = I(list(dynamics$prop_species_history)),
      prop_interactions_history = I(list(dynamics$prop_interactions_history)),
      services_history          = I(list(rowSums(services$services_history))),
      
      stringsAsFactors = FALSE
    )
    
    # Colunas individuais de serviço (serv_1 ... serv_n)
    serv_cols <- as.data.frame(t(e_final_vec))
    colnames(serv_cols) <- paste0("serv_", seq_len(n_services))
    
    results_list[[i]] <- cbind(temp_df, serv_cols)
  }
  
  # ---------------------------------------------------------------------------
  # Combinar resultados
  # ---------------------------------------------------------------------------
  results_df <- bind_rows(results_list)
  
  if (verbose) {
    cat("\n===========================================================\n")
    cat(sprintf("CONCLUÍDO! %d simulações finalizadas.\n", nrow(results_df)))
    cat("===========================================================\n\n")
  }
  
  return(results_df)
}
