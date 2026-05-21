# =============================================================================
# SCRIPT MASTER — MODELO BOOLEANO COM REDES MUTUALISTAS REAIS
# =============================================================================
#
# ORDEM DE EXECUÇÃO:
#   1. Carrega todas as funções
#   2. Lê as redes reais do disco
#   3. Calcula métricas estruturais (NODF, Q, conectância...)
#   4. Roda o modelo Booleano variando provedor de serviços
#   5. Salva os resultados
#
# ANTES DE RODAR:
#   - Defina `caminho_redes` com o caminho para a pasta das suas redes
#   - As redes devem ser arquivos .csv no formato: linhas = plantas, colunas = animais
#   - Instale os pacotes necessários (ver seção de pacotes abaixo)
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

source("functions_Bruno.R")                        # environment(), alpha_fun(), Cf()
source("model&node_actualization.R")               # simulation()
source("eco_services_iteraction.R")                # ecosystem()
source("identify_core_periphery.R")                # identify_core_periphery_bipartite()
source("ecosystem_services_network_v2.R")          # ecosystem_services_network_v2()
source("calcular_metricas_rede.R")                 # calcular_metricas_rede(), calcular_metricas_todas()
source("explore_parameters_v3_redes_reais.R")      # explore_parameters_v3_redes_reais()


# =============================================================================
# 2. LER REDES REAIS DO DISCO
# =============================================================================

# --- DEFINA AQUI O CAMINHO PARA SUA PASTA DE REDES ---
caminho_redes <- "C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Empirical/P_AF_bin"   # <-- ALTERE AQUI

# Listar todos os CSVs recursivamente (percorre subpastas por tipo de mutualismo)
arquivos_csv <- list.files(
  path       = caminho_redes,
  pattern    = "\\.csv$",
  recursive  = TRUE,
  full.names = TRUE
)

cat(sprintf("Arquivos encontrados: %d\n", length(arquivos_csv)))

# Função de leitura já existente (de Mutualistic_networks.R)
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
  return(A_raw)   # retorna bipartida (NÃO quadrada aqui — quadratização é feita no loop)
}

# Ler todas as redes como matrizes bipartidas
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

# (Opcional) Ver distribuição de tamanhos
tamanhos <- sapply(lista_redes_raw, function(B) nrow(B) + ncol(B))
cat(sprintf("Tamanho mínimo: %d espécies\n", min(tamanhos)))
cat(sprintf("Tamanho máximo: %d espécies\n", max(tamanhos)))
cat(sprintf("Tamanho médio:  %.1f espécies\n", mean(tamanhos)))


# =============================================================================
# 3. CALCULAR MÉTRICAS ESTRUTURAIS
# =============================================================================
# NODF e modularidade são lentos para redes grandes.
# Se quiser pular para redes grandes, use calc_nodf = FALSE / calc_mod = FALSE
# e calcule separadamente depois.

cat("\nCalculando métricas estruturais...\n")

metricas_redes <- calcular_metricas_todas(
  lista_redes  = lista_redes_raw,
  calc_nodf    = TRUE,
  calc_mod     = TRUE,
  verbose      = TRUE
)

# Adicionar tipo de mutualismo a partir do nome da pasta
metricas_redes <- metricas_redes %>%
  mutate(
    tipo_mutualismo = basename(dirname(
      file.path(caminho_redes, gsub("/.*", "", nome_rede))
    ))
  )

# Salvar métricas
saveRDS(metricas_redes, "metricas_redes.rds")
write.csv(metricas_redes, "metricas_redes.csv", row.names = FALSE)
cat("Métricas salvas em metricas_redes.rds e metricas_redes.csv\n")

print(metricas_redes)


# =============================================================================
# 4. RODAR O MODELO BOOLEANO
# =============================================================================

cat("\nIniciando simulações do modelo Booleano...\n")

resultados <- explore_parameters_v3_redes_reais(
  
  # Redes reais
  lista_redes           = lista_redes_raw,
  metricas_redes        = metricas_redes,
  
  # Variação principal
  service_providers_options = c("core", "periphery", "random"),
  
  # Serviços ecossistêmicos
  n_services            = 5,
  connectance_ES        = 0.3,
  distribution_ES       = "lognormal",
  
  # Core/periphery
  threshold_percentile  = 0.5,
  
  # Parâmetros das espécies (sem custo fisiológico por padrão)
  B_shape1              = 0.5,
  B_shape2              = 0.5,
  Ce_shape1             = 0.5,
  Ce_shape2             = 0.5,
  Cp_multiplier         = 0,     # mudar para > 0 para ativar custo fisiológico
  
  # Ambiente (estático por padrão)
  A_min                 = 0,
  A_max                 = 0,
  w_min                 = 0,
  w_max                 = 0,
  t_max                 = 10000,
  zi_min                = 1,
  zi_max                = 10,
  
  # Replicatas
  n_replicates          = 5,
  seed                  = 123,
  verbose               = TRUE
)


# =============================================================================
# 5. SALVAR RESULTADOS
# =============================================================================

# Salvar em RDS (preserva listas de históricos)
saveRDS(resultados, "resultados_modelo.rds")

# Salvar versão simplificada em CSV (sem as colunas de histórico)
resultados_csv <- resultados %>%
  select(-prop_species_history, -prop_interactions_history, -services_history)

write.csv(resultados_csv, "resultados_modelo.csv", row.names = FALSE)

cat("\nResultados salvos em:\n")
cat("  resultados_modelo.rds  (completo, com históricos)\n")
cat("  resultados_modelo.csv  (simplificado, sem históricos)\n")


# =============================================================================
# 6. CHECAGEM RÁPIDA DOS RESULTADOS
# =============================================================================

cat("\n--- Resumo dos resultados ---\n")
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
