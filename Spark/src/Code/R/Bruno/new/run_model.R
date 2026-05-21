# =============================================================================
# SCRIPT MASTER — MODELO BOOLEANO COM REDES MUTUALISTAS REAIS
# =============================================================================
#
# ORDEM DE EXECUÇÃO:
#   1. Carrega todas as funções
#   2. Lê as redes reais do disco
#   3. Calcula métricas estruturais (NODF, Q, conectância...)
#   4. Roda o modelo Booleano com grid de parâmetros
#   5. Salva os resultados
#
# ANTES DE RODAR:
#   - Defina `caminho_redes` com o caminho para a pasta das suas redes
#   - As redes devem ser arquivos .csv: linhas = plantas, colunas = animais
#   - Instale os pacotes necessários (ver seção 0)
#   - Todos os .R devem estar na mesma pasta que este script
# =============================================================================


# =============================================================================
# 0. PACOTES
# =============================================================================
library(dplyr)
library(readr)
library(bipartite)   # para NODF e modularidade


# =============================================================================
# 1. CARREGAR TODAS AS FUNÇÕES
# =============================================================================

source("functions_Bruno.R")                    # environment(), alpha_fun(), Cf()
source("model_node_actualization.R")           # simulation()
source("eco_services_iteraction.R")            # ecosystem()
source("identify_core_periphery.R")            # identify_core_periphery_bipartite()
source("ecosystem_services_network_v2.R")      # ecosystem_services_network_v2()
source("calcular_metricas_rede.R")             # calcular_metricas_rede(), calcular_metricas_todas()
source("explore_parameters_v3_redes_reais.R")  # explore_parameters_v3_redes_reais()


# =============================================================================
# 2. LER REDES REAIS DO DISCO
# =============================================================================

# --- DEFINA AQUI O CAMINHO PARA SUA PASTA DE REDES ---
caminho_redes <- "caminho/para/sua/pasta/redes"   # <-- ALTERE AQUI

# Listar todos os CSVs recursivamente (percorre subpastas por tipo de mutualismo)
arquivos_csv <- list.files(
  path      = caminho_redes,
  pattern   = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

cat(sprintf("Arquivos encontrados: %d\n", length(arquivos_csv)))

# Função de leitura: lê CSV, binariza, retorna matriz bipartida
importar_rede_bipartida <- function(caminho) {
  linhas   <- readLines(caminho)
  dados    <- lapply(linhas, function(x) as.numeric(strsplit(x, ",")[[1]]))
  ncol_max <- max(sapply(dados, length))
  dados    <- lapply(dados, function(x) c(x, rep(0, ncol_max - length(x))))
  A_raw    <- do.call(rbind, dados)
  A_raw[is.na(A_raw)] <- 0
  A_raw[A_raw > 0]    <- 1
  rownames(A_raw) <- paste0("P", seq_len(nrow(A_raw)))
  colnames(A_raw) <- paste0("A", seq_len(ncol(A_raw)))
  return(A_raw)  # bipartida — quadratização feita no loop
}

# Ler todas as redes
lista_redes_raw <- lapply(arquivos_csv, function(arq) {
  tryCatch(
    importar_rede_bipartida(arq),
    error = function(e) {
      message(paste("Erro ao ler:", basename(arq), "—", e$message))
      NULL
    }
  )
})

# Nomear pela pasta/arquivo (ex: "polinizacao/M_PL_001")
nomes_redes <- tools::file_path_sans_ext(
  file.path(
    basename(dirname(arquivos_csv)),
    basename(arquivos_csv)
  )
)
names(lista_redes_raw) <- nomes_redes

# Remover redes que falharam na leitura
lista_redes_raw <- Filter(Negate(is.null), lista_redes_raw)
cat(sprintf("Redes carregadas com sucesso: %d\n", length(lista_redes_raw)))

# Distribuição de tamanhos
tamanhos <- sapply(lista_redes_raw, function(B) nrow(B) + ncol(B))
cat(sprintf("Tamanho mínimo: %d espécies\n", min(tamanhos)))
cat(sprintf("Tamanho máximo: %d espécies\n", max(tamanhos)))
cat(sprintf("Tamanho médio:  %.1f espécies\n", mean(tamanhos)))


# =============================================================================
# 3. CALCULAR MÉTRICAS ESTRUTURAIS
# =============================================================================
# NODF e Q são lentos para redes grandes.
# Se já calculou antes, carregue direto: metricas_redes <- readRDS("metricas_redes.rds")

cat("\nCalculando métricas estruturais...\n")

metricas_redes <- calcular_metricas_todas(
  lista_redes = lista_redes_raw,
  calc_nodf   = TRUE,
  calc_mod    = TRUE,
  verbose     = TRUE
)

# Adicionar tipo de mutualismo a partir do nome da subpasta
metricas_redes <- metricas_redes %>%
  mutate(
    tipo_mutualismo = basename(dirname(
      file.path(caminho_redes, gsub("/.*", "", nome_rede))
    ))
  )

# Salvar métricas (não precisa recalcular nas próximas sessões)
saveRDS(metricas_redes, "metricas_redes.rds")
write.csv(metricas_redes, "metricas_redes.csv", row.names = FALSE)
cat("Métricas salvas em metricas_redes.rds e metricas_redes.csv\n\n")
print(metricas_redes)


# =============================================================================
# 4. GRID DE PARÂMETROS
# =============================================================================
# Defina aqui os valores que quer variar.
# Cada combinação vira um cenário independente — salvo em arquivo separado.
#
# Parâmetros disponíveis para variar:
#   Cp_multiplier        — custo fisiológico (0 = desativado)
#   threshold_percentile — corte core/periphery (0.25 = top 25% é core)
#   connectance_ES       — conectância da rede de serviços
#   n_replicates         — replicatas por combinação
# =============================================================================

grid_params <- expand.grid(
  Cp_multiplier        = c(0, 0.5, 1.0),
  threshold_percentile = c(0.25, 0.50, 0.75),
  stringsAsFactors     = FALSE
)

cat(sprintf("Total de cenários no grid: %d\n", nrow(grid_params)))
cat(sprintf("Simulações por cenário: %d redes × 3 provedores × 50 replicatas = %d\n",
            length(lista_redes_raw),
            length(lista_redes_raw) * 3 * 50))
cat(sprintf("Total geral de simulações: %d\n\n",
            nrow(grid_params) * length(lista_redes_raw) * 3 * 50))


# =============================================================================
# 5. RODAR O MODELO BOOLEANO — LOOP SOBRE O GRID
# =============================================================================

lista_resultados <- vector("list", nrow(grid_params))

for (g in seq_len(nrow(grid_params))) {

  cp_val  <- grid_params$Cp_multiplier[g]
  thr_val <- grid_params$threshold_percentile[g]

  nome_arquivo <- sprintf("resultados_Cp%.2f_thr%.2f.rds", cp_val, thr_val)

  # Pular cenário se já foi salvo (permite retomar de onde parou)
  if (file.exists(nome_arquivo)) {
    cat(sprintf("\n--- Cenário %d/%d já existe, carregando: %s ---\n",
                g, nrow(grid_params), nome_arquivo))
    lista_resultados[[g]] <- readRDS(nome_arquivo)
    next
  }

  cat(sprintf("\n--- Cenário %d/%d | Cp=%.2f | threshold=%.2f ---\n",
              g, nrow(grid_params), cp_val, thr_val))

  resultado_g <- explore_parameters_v3_redes_reais(
    lista_redes               = lista_redes_raw,
    metricas_redes            = metricas_redes,
    service_providers_options = c("core", "periphery", "random"),
    n_services                = 5,
    connectance_ES            = 0.3,
    distribution_ES           = "lognormal",
    threshold_percentile      = thr_val,
    B_shape1                  = 0.5,
    B_shape2                  = 0.5,
    Ce_shape1                 = 0.5,
    Ce_shape2                 = 0.5,
    Cp_multiplier             = cp_val,
    A_min                     = 0,
    A_max                     = 0,
    w_min                     = 0,
    w_max                     = 0,
    t_max                     = 10000,
    zi_min                    = 1,
    zi_max                    = 10,
    n_replicates              = 50,
    seed                      = 123,
    verbose                   = TRUE
  )

  # Adicionar colunas identificadoras do cenário
  resultado_g$cenario_id          <- g
  resultado_g$param_Cp_multiplier <- cp_val
  resultado_g$param_threshold     <- thr_val

  # Checkpoint — salva imediatamente ao terminar cada cenário
  saveRDS(resultado_g, nome_arquivo)
  cat(sprintf("Cenário %d salvo em: %s\n", g, nome_arquivo))

  lista_resultados[[g]] <- resultado_g
}


# =============================================================================
# 6. CONSOLIDAR E SALVAR RESULTADO FINAL
# =============================================================================

resultados <- bind_rows(lista_resultados)

# Completo com históricos (só abre no R)
saveRDS(resultados, "resultados_modelo_completo.rds")

# Simplificado sem históricos (abre no Excel também)
resultados_csv <- resultados %>%
  select(-prop_species_history, -prop_interactions_history, -services_history)

write.csv(resultados_csv, "resultados_modelo_completo.csv", row.names = FALSE)

cat("\nResultados consolidados salvos em:\n")
cat("  resultados_modelo_completo.rds  (completo, com históricos)\n")
cat("  resultados_modelo_completo.csv  (simplificado, sem históricos)\n")


# =============================================================================
# 7. CHECAGEM RÁPIDA DOS RESULTADOS
# =============================================================================

cat("\n--- Resumo por provedor de serviços ---\n")
print(
  resultados %>%
    group_by(service_providers) %>%
    summarise(
      n_sims                 = n(),
      media_persist_especies = mean(persistence_species, na.rm = TRUE),
      media_persist_interac  = mean(persistence_interactions, na.rm = TRUE),
      media_services_loss    = mean(services_loss_relative, na.rm = TRUE),
      media_riqueza_serv     = mean(riqueza_servicos_final, na.rm = TRUE),
      .groups = "drop"
    )
)

cat("\n--- Resumo por cenário (Cp × threshold) ---\n")
print(
  resultados %>%
    group_by(param_Cp_multiplier, param_threshold, service_providers) %>%
    summarise(
      media_persist_especies = mean(persistence_species, na.rm = TRUE),
      media_services_loss    = mean(services_loss_relative, na.rm = TRUE),
      media_riqueza_serv     = mean(riqueza_servicos_final, na.rm = TRUE),
      .groups = "drop"
    )
)
