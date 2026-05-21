# =============================================================================
# SCRIPT MASTER — MODELO BOOLEANO COM REDES MUTUALISTAS REAIS
# =============================================================================
#
# ORDEM DE EXECUÇÃO:
#   1. Carrega todas as funções
#   2. Lê as redes reais do disco
#   3. Calcula métricas estruturais (NODF, Q, conectância...)
#   4. Define grid de parâmetros
#   5. Roda o modelo Booleano sobre o grid
#   6. Consolida e salva resultados
#   7. Checagem rápida
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
  path       = caminho_redes,
  pattern    = "\\.csv$",
  recursive  = TRUE,
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
# Se já calculou antes, carregue direto:
#   metricas_redes <- readRDS("metricas_redes.rds")

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
#
# COMO USAR:
#   - Para manter um parâmetro fixo: deixe só um valor no vetor
#     ex: Cp_multiplier = c(0)        → fixo em 0, não varia
#   - Para variar um parâmetro: coloque múltiplos valores
#     ex: Cp_multiplier = c(0, 0.5, 1.0) → 3 níveis
#
# ATENÇÃO AO TAMANHO DO GRID:
#   O número total de cenários é o PRODUTO de todos os vetores com >1 valor.
#   Antes de rodar, verifique o total impresso no console.
#
# PARÂMETROS DO MODELO:
#
#   [BENEFÍCIO ECOLÓGICO — B]
#     B_shape1, B_shape2: parâmetros da distribuição Beta de onde B é sorteado
#     Média da Beta = shape1 / (shape1 + shape2)
#     B alto (shape1 >> shape2): espécies ganham muito de cada parceiro
#     ex: shape1=2, shape2=0.5 → média ~0.8 (benefício alto)
#         shape1=0.5, shape2=2 → média ~0.2 (benefício baixo)
#
#   [CUSTO ECOLÓGICO — Ce]
#     Ce_shape1, Ce_shape2: parâmetros da Beta de Ce
#     Ce alto: interagir custa caro fisiologicamente
#     ex: shape1=0.5, shape2=0.5 → distribuição U (muito variável)
#         shape1=1,   shape2=1   → uniforme
#
#   [CUSTO FISIOLÓGICO — Cp]
#     Cp_multiplier: escala o custo fisiológico (0 = desativado)
#     Cp_shape1, Cp_shape2: forma da distribuição Beta de Cp
#     Só ativo se Cp_multiplier > 0
#
#   [AMBIENTE]
#     A_min, A_max: amplitude da flutuação ambiental θ(t)
#       A=0 → ambiente estático
#       A>0 → ambiente oscila entre -A e +A
#     w_min, w_max: frequência da oscilação ambiental
#       w=0 → sem oscilação
#       w>0 → oscilação periódica
#     zi_min, zi_max: intervalo do ótimo ambiental de cada espécie
#       define o quão diferente é o zi de cada espécie
#
#   [REDE DE SERVIÇOS]
#     connectance_ES: fração de células não-zero na ES matrix
#     threshold_percentile: corte para core/periphery (0.5 = top 50% é core)
#
#   [SIMULAÇÃO]
#     n_replicates: replicatas por combinação (mín recomendado: 30)
#     t_max: passos máximos de tempo por simulação
# =============================================================================

grid_params <- expand.grid(

  # --- Número de serviços ecossistêmicos ---
  # Fixo: manter um único valor
  # Sensibilidade: c(5, 10, 15)
  n_services = c(5),        # variar ex: c(5, 10, 15)

  # --- Benefício ecológico (B) ---
  B_shape1 = c(0.5),        # variar ex: c(0.5, 1, 2)
  B_shape2 = c(0.5),        # variar ex: c(0.5, 1, 2)

  # --- Custo ecológico (Ce) ---
  Ce_shape1 = c(0.5),       # variar ex: c(0.5, 1, 2)
  Ce_shape2 = c(0.5),       # variar ex: c(0.5, 1, 2)

  # --- Custo fisiológico (Cp) ---
  Cp_multiplier = c(0),     # variar ex: c(0, 0.5, 1.0)
  Cp_shape1     = c(0.5),   # só importa se Cp_multiplier > 0
  Cp_shape2     = c(0.5),   # só importa se Cp_multiplier > 0

  # --- Ambiente ---
  A_min = c(0),             # variar ex: c(0, 0.5, 1.0)
  A_max = c(0),             # variar ex: c(0, 0.5, 1.0)
  w_min = c(0),             # variar ex: c(0, 0.1, 0.5)
  w_max = c(0),             # variar ex: c(0, 0.1, 0.5)
  zi_min = c(1),            # variar ex: c(1, 5)
  zi_max = c(10),           # variar ex: c(10, 50)

  # --- Rede de serviços ---
  connectance_ES       = c(0.3),   # variar ex: c(0.1, 0.3, 0.5)
  threshold_percentile = c(0.5),   # variar ex: c(0.25, 0.50, 0.75)

  # --- Simulação ---
  n_replicates = c(50),     # aumentar para mais robustez estatística
  t_max        = c(10000),  # raramente precisa mudar

  stringsAsFactors = FALSE
)

# Informações sobre o grid
n_cenarios   <- nrow(grid_params)
n_redes      <- length(lista_redes_raw)
n_provedores <- 3  # core, periphery, random
n_rep        <- unique(grid_params$n_replicates)

cat(sprintf("\n=== GRID DE PARÂMETROS ===\n"))
cat(sprintf("Cenários no grid:          %d\n", n_cenarios))
cat(sprintf("Redes por cenário:         %d\n", n_redes))
cat(sprintf("Provedores por rede:       %d (core / periphery / random)\n", n_provedores))
cat(sprintf("Replicatas por combinação: %d\n", n_rep))
cat(sprintf("Total de simulações:       %d\n\n",
            n_cenarios * n_redes * n_provedores * n_rep))


# =============================================================================
# 5. RODAR O MODELO BOOLEANO — LOOP SOBRE O GRID
# =============================================================================

lista_resultados <- vector("list", n_cenarios)

for (g in seq_len(n_cenarios)) {

  p <- grid_params[g, ]

  # Nome do arquivo de checkpoint baseado nos parâmetros que variam
  nome_arquivo <- sprintf(
    "resultados_cenario_%03d_Cp%.2f_A%.2f_thr%.2f.rds",
    g, p$Cp_multiplier, p$A_max, p$threshold_percentile
  )

  # Pular se já foi calculado (permite retomar de onde parou)
  if (file.exists(nome_arquivo)) {
    cat(sprintf("\n--- Cenário %d/%d já existe, carregando: %s ---\n",
                g, n_cenarios, nome_arquivo))
    lista_resultados[[g]] <- readRDS(nome_arquivo)
    next
  }

  cat(sprintf(
    "\n--- Cenário %d/%d | B=(%.1f,%.1f) | Ce=(%.1f,%.1f) | Cp=%.2f | A=(%.2f,%.2f) | thr=%.2f ---\n",
    g, n_cenarios,
    p$B_shape1, p$B_shape2,
    p$Ce_shape1, p$Ce_shape2,
    p$Cp_multiplier,
    p$A_min, p$A_max,
    p$threshold_percentile
  ))

  resultado_g <- explore_parameters_v3_redes_reais(
    lista_redes               = lista_redes_raw,
    metricas_redes            = metricas_redes,
    service_providers_options = c("core", "periphery", "random"),
    n_services                = p$n_services,
    connectance_ES            = p$connectance_ES,
    distribution_ES           = "lognormal",
    threshold_percentile      = p$threshold_percentile,
    B_shape1                  = p$B_shape1,
    B_shape2                  = p$B_shape2,
    Ce_shape1                 = p$Ce_shape1,
    Ce_shape2                 = p$Ce_shape2,
    Cp_shape1                 = p$Cp_shape1,
    Cp_shape2                 = p$Cp_shape2,
    Cp_multiplier             = p$Cp_multiplier,
    A_min                     = p$A_min,
    A_max                     = p$A_max,
    w_min                     = p$w_min,
    w_max                     = p$w_max,
    t_max                     = p$t_max,
    zi_min                    = p$zi_min,
    zi_max                    = p$zi_max,
    n_replicates              = p$n_replicates,
    seed                      = 123,
    verbose                   = TRUE
  )

  # Adicionar todas as colunas do grid como identificadores do cenário
  resultado_g$cenario_id           <- g
  resultado_g$param_n_services     <- p$n_services
  resultado_g$param_B_shape1       <- p$B_shape1
  resultado_g$param_B_shape2       <- p$B_shape2
  resultado_g$param_Ce_shape1      <- p$Ce_shape1
  resultado_g$param_Ce_shape2      <- p$Ce_shape2
  resultado_g$param_Cp_multiplier  <- p$Cp_multiplier
  resultado_g$param_Cp_shape1      <- p$Cp_shape1
  resultado_g$param_Cp_shape2      <- p$Cp_shape2
  resultado_g$param_A_min          <- p$A_min
  resultado_g$param_A_max          <- p$A_max
  resultado_g$param_w_min          <- p$w_min
  resultado_g$param_w_max          <- p$w_max
  resultado_g$param_zi_min         <- p$zi_min
  resultado_g$param_zi_max         <- p$zi_max
  resultado_g$param_connectance_ES <- p$connectance_ES
  resultado_g$param_threshold      <- p$threshold_percentile
  resultado_g$param_n_replicates   <- p$n_replicates
  resultado_g$param_t_max          <- p$t_max

  # Checkpoint — salva imediatamente ao terminar cada cenário
  saveRDS(resultado_g, nome_arquivo)
  cat(sprintf("Cenário %d/%d salvo em: %s\n", g, n_cenarios, nome_arquivo))

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

cat("\n--- Resumo por cenário ---\n")
print(
  resultados %>%
    group_by(cenario_id, param_Cp_multiplier, param_A_max,
             param_threshold, service_providers) %>%
    summarise(
      media_persist_especies = mean(persistence_species, na.rm = TRUE),
      media_services_loss    = mean(services_loss_relative, na.rm = TRUE),
      media_riqueza_serv     = mean(riqueza_servicos_final, na.rm = TRUE),
      .groups = "drop"
    )
)
