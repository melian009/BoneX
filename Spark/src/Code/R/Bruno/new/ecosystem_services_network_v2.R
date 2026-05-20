# =============================================================================
# FUNÇÃO: REDE DE SERVIÇOS ECOSSISTÊMICOS - VERSÃO 2
# =============================================================================
#
# DESCRIÇÃO:
#   Cria a matriz de serviços ecossistêmicos (espécies × serviços), onde cada
#   célula representa a intensidade com que uma espécie contribui para um
#   serviço. A novidade em relação à versão original é o argumento
#   `service_providers`, que controla QUAIS espécies podem prover serviços:
#
#     "core"      → apenas espécies core (alto grau) proveem serviços
#     "periphery" → apenas espécies periféricas (baixo grau) proveem serviços
#     "random"    → qualquer espécie pode prover (sem restrição por posição)
#
# INPUTS:
#   sp_n                — número total de espécies (tamanho da rede quadrada)
#   services_n          — número de serviços ecossistêmicos
#   mutualistic_network — matriz QUADRADA da rede mutualista (sp_n × sp_n)
#   service_providers   — "core", "periphery" ou "random"
#   threshold_percentile— percentil de corte para core/periphery (default 0.5)
#   connectance         — conectância da rede de serviços (default 0.3)
#   distribution        — distribuição dos pesos: "uniform", "lognormal", "beta", "normal"
#   intensity_min       — valor mínimo de intensidade (default 0.1)
#   intensity_max       — valor máximo de intensidade (default 1.0)
#   meanlog / sdlog     — parâmetros da lognormal (default -0.5 / 0.5)
#   shape1_beta / shape2_beta — parâmetros da beta (default 2 / 5)
#
# OUTPUT:
#   Matriz (sp_n × services_n) com intensidades de contribuição.
#   Espécies fora do grupo de provedores têm linha = 0.
#   Atributos: "service_providers", "n_providers", "provider_indices"
#
# =============================================================================

ecosystem_services_network_v2 <- function(
    sp_n,
    services_n,
    mutualistic_network,
    service_providers      = "random",
    threshold_percentile   = 0.5,
    connectance            = 0.3,
    distribution           = "lognormal",
    intensity_min          = 0.1,
    intensity_max          = 1.0,
    meanlog                = -0.5,
    sdlog                  = 0.5,
    shape1_beta            = 2,
    shape2_beta            = 5,
    mean_normal            = 0.5,
    sd_normal              = 0.15
) {
  
  # ---------------------------------------------------------------------------
  # 1. Validações
  # ---------------------------------------------------------------------------
  if (!service_providers %in% c("core", "periphery", "random")) {
    stop("service_providers deve ser 'core', 'periphery' ou 'random'.")
  }
  if (!is.matrix(mutualistic_network)) mutualistic_network <- as.matrix(mutualistic_network)
  if (nrow(mutualistic_network) != sp_n || ncol(mutualistic_network) != sp_n) {
    stop("mutualistic_network deve ser uma matriz quadrada sp_n × sp_n.")
  }
  
  # ---------------------------------------------------------------------------
  # 2. Identificar quais espécies podem prover serviços
  # ---------------------------------------------------------------------------
  
  if (service_providers == "random") {
    
    # Todas as espécies são candidatas
    provider_indices <- 1:sp_n
    
  } else {
    
    # Calcular grau de cada espécie na rede quadrada
    # (soma da linha + soma da coluna, dividido por 2 para não duplicar)
    degree_sq <- (rowSums(mutualistic_network > 0) + colSums(mutualistic_network > 0)) / 2
    
    cutoff <- quantile(degree_sq, probs = threshold_percentile)
    
    if (service_providers == "core") {
      provider_indices <- which(degree_sq >= cutoff)
    } else {  # "periphery"
      provider_indices <- which(degree_sq < cutoff)
    }
    
    # Garantia: se nenhuma espécie passou pelo filtro, avisa e usa todas
    if (length(provider_indices) == 0) {
      warning(sprintf(
        "Nenhuma espécie classificada como '%s' com threshold=%.2f. Usando todas.",
        service_providers, threshold_percentile
      ))
      provider_indices <- 1:sp_n
    }
  }
  
  n_providers <- length(provider_indices)
  
  # ---------------------------------------------------------------------------
  # 3. Função auxiliar: gerar valores de intensidade
  # ---------------------------------------------------------------------------
  gerar_intensidade <- function(n) {
    switch(distribution,
      "uniform"   = runif(n, intensity_min, intensity_max),
      "lognormal" = {
        vals <- rlnorm(n, meanlog, sdlog)
        if (max(vals) > min(vals)) {
          (vals - min(vals)) / (max(vals) - min(vals)) *
            (intensity_max - intensity_min) + intensity_min
        } else {
          rep(intensity_min, n)
        }
      },
      "beta"   = rbeta(n, shape1_beta, shape2_beta) *
                   (intensity_max - intensity_min) + intensity_min,
      "normal" = pmax(pmin(
                   rnorm(n, mean_normal, sd_normal),
                   intensity_max), intensity_min),
      stop(paste("Distribuição desconhecida:", distribution))
    )
  }
  
  # ---------------------------------------------------------------------------
  # 4. Construir a matriz de serviços
  #    Apenas as linhas dos provedores recebem valores > 0
  # ---------------------------------------------------------------------------
  ES <- matrix(0, nrow = sp_n, ncol = services_n)
  
  for (i in provider_indices) {
    for (j in 1:services_n) {
      if (runif(1) < connectance) {
        ES[i, j] <- gerar_intensidade(1)
      }
    }
  }
  
  # Garantir que cada serviço tenha pelo menos 1 provedor
  # (evita serviços com coluna zerada que causam divisão por zero depois)
  for (j in 1:services_n) {
    if (sum(ES[, j]) == 0) {
      i_chosen <- sample(provider_indices, 1)
      ES[i_chosen, j] <- gerar_intensidade(1)
    }
  }
  
  # ---------------------------------------------------------------------------
  # 5. Nomear e anotar atributos
  # ---------------------------------------------------------------------------
  rownames(ES) <- paste0("sp", 1:sp_n)
  colnames(ES) <- paste0("service", 1:services_n)
  
  attr(ES, "service_providers")   <- service_providers
  attr(ES, "n_providers")         <- n_providers
  attr(ES, "provider_indices")    <- provider_indices
  attr(ES, "threshold_percentile")<- threshold_percentile
  attr(ES, "distribution")        <- distribution
  attr(ES, "connectance_target")  <- connectance
  attr(ES, "connectance_actual")  <- sum(ES > 0) / (sp_n * services_n)
  
  return(ES)
}


# =============================================================================
# EXEMPLO DE USO
# =============================================================================

# # Rede quadrada simulada (25 espécies)
# set.seed(42)
# sq_net <- matrix(rbinom(25*25, 1, 0.2), 25, 25)
# diag(sq_net) <- 0
#
# # Cenário 1: só espécies core proveem serviços
# ES_core <- ecosystem_services_network_v2(
#   sp_n = 25, services_n = 5,
#   mutualistic_network = sq_net,
#   service_providers = "core",
#   threshold_percentile = 0.5,
#   distribution = "lognormal"
# )
#
# # Cenário 2: só periféricas
# ES_periph <- ecosystem_services_network_v2(
#   sp_n = 25, services_n = 5,
#   mutualistic_network = sq_net,
#   service_providers = "periphery",
#   threshold_percentile = 0.5,
#   distribution = "lognormal"
# )
#
# # Cenário 3: aleatório
# ES_rand <- ecosystem_services_network_v2(
#   sp_n = 25, services_n = 5,
#   mutualistic_network = sq_net,
#   service_providers = "random",
#   distribution = "lognormal"
# )
#
# # Verificar quais espécies proveem em cada cenário
# cat("Provedores core:      ", attr(ES_core,   "provider_indices"), "\n")
# cat("Provedores periféricos:", attr(ES_periph, "provider_indices"), "\n")
# cat("N provedores core:    ", attr(ES_core,   "n_providers"), "\n")
# cat("N provedores periféricos:", attr(ES_periph, "n_providers"), "\n")
