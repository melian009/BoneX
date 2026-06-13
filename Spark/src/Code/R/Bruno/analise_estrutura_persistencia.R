# =============================================================================
# ANÁLISE: EFEITO DAS MÉTRICAS ESTRUTURAIS NA PERSISTÊNCIA DE ESPÉCIES
# =============================================================================
#
# Pergunta 1 — COMO cada métrica estrutural afeta a persistência?
#   -> Modelo multivariado único, coeficientes em z-score (direção, magnitude, p)
#
# Pergunta 2 — QUAL métrica é mais importante?
#   -> Ranking dos |coeficientes| padronizados do MESMO modelo multivariado
#
# Cenário fixado em param_nivel_BCe == "moderate"
# =============================================================================

library(lme4)
library(ggpubr)
library(tidyverse)
library(readr)
library(glmmTMB)
library(ggplot2)
library(openxlsx2)
library(ggeffects)
library(bbmle)
library(dplyr)
library(car)
library(broom.mixed)
library(purrr)
library(performance)
library(patchwork)

# -----------------------------------------------------------------------
# 1. FILTRAR CENÁRIO E TRATAR NAs
# -----------------------------------------------------------------------
resultados = readRDS("C:/Users/bruno/OneDrive/Documentos/posdoc/Suíça/projeto/resultados_modelo_completo.rds")

resultados <- resultados %>%
  mutate(
    param_nivel_BCe = dplyr::recode(param_nivel_BCe,
                                    "fraco"    = "low",
                                    "moderado" = "moderate",
                                    "forte"    = "high"
    ),
    tipo_mutualismo_label = dplyr::recode(sistema,
                                          "anemo_fish" = "anemo-fish",
                                          "ant_domacea" = "ant-domacea",
                                          "ant_NEF" = "Ant-EFN"
    )
  )

estruc <- resultados %>%
  filter(param_nivel_BCe == "moderate", param_Cp_multiplier == 0)

n_antes <- nrow(estruc)

estruc <- estruc %>%
  filter(!is.na(NODF), !is.na(Q), !is.na(conectancia),
         !is.na(n_species_total), !is.na(n_interacoes),
         !is.na(mean_degree_core), !is.na(mean_degree_periphery))

n_depois <- nrow(estruc)

cat("=== TRATAMENTO DE NAs ===\n")
cat(sprintf("Linhas antes:  %d\n", n_antes))
cat(sprintf("Linhas depois: %d\n", n_depois))
cat(sprintf("Excluídas:     %d (%.1f%%)\n\n", n_antes - n_depois,
            100 * (n_antes - n_depois) / n_antes))

# -----------------------------------------------------------------------
# 2. PADRONIZAR (Z-SCORE) OS PREDITORES ESTRUTURAIS
# -----------------------------------------------------------------------
estruc <- estruc %>%
  mutate(
    NODF_z     = as.numeric(scale(NODF)),
    Q_z        = as.numeric(scale(Q)),
    conect_z   = as.numeric(scale(conectancia)),
    size_z     = as.numeric(scale(n_species_total)),
    n_inter_z  = as.numeric(scale(n_interacoes)),
    deg_core_z = as.numeric(scale(mean_degree_core)),
    deg_peri_z = as.numeric(scale(mean_degree_periphery))
  )

# -----------------------------------------------------------------------
# 3. CHECAR MULTICOLINEARIDADE (Matriz de Correlação)
# -----------------------------------------------------------------------
cat("=== MATRIZ DE CORRELAÇÃO ENTRE PREDITORES (z-score) ===\n")
cor_matrix <- cor(estruc[, c("NODF_z","Q_z","conect_z","size_z",
                             "n_inter_z","deg_core_z","deg_peri_z")],
                  use = "complete.obs")
print(round(cor_matrix, 2))
cat("\n")

# -----------------------------------------------------------------------
# 4. AJUSTAR MODELO MULTIVARIADO FINAL (Mantendo todas as variáveis)
# -----------------------------------------------------------------------
mod_final <- glmmTMB(
  cbind(n_species_final, n_species_total - n_species_final) ~
    NODF_z + Q_z + conect_z + size_z + deg_core_z + deg_peri_z +
    (1 | nome_rede),
  data   = estruc,
  family = binomial
)

# Diagnóstico de Colinearidade para checagem (VIF e VIF Ajustado)
cat("=== DIAGNÓSTICO DE COLINEARIDADE (MODELO COMPLETO) ===\n")
print(check_collinearity(mod_final))
cat("\n")

# -----------------------------------------------------------------------
# 5. PERGUNTA 1 — COMO cada métrica afeta a persistência?
# -----------------------------------------------------------------------
cat("=== RESUMO DO MODELO FINAL ===\n")
print(summary(mod_final))

tabela_efeitos <- tidy(mod_final, effects = "fixed", conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ ""
    )
  ) %>%
  select(term, estimate, std.error, conf.low, conf.high, p.value, sig)

cat("\n=== PERGUNTA 1: EFEITO DE CADA MÉTRICA (em log-odds por DP) ===\n")
print(tabela_efeitos)

# -----------------------------------------------------------------------
# 6. PERGUNTA 2 — QUAL métrica é mais importante? (Ranking por |estimate|)
# -----------------------------------------------------------------------
tabela_ranking <- tabela_efeitos %>%
  mutate(abs_estimate = abs(estimate)) %>%
  arrange(desc(abs_estimate)) %>%
  select(term, estimate, abs_estimate, p.value, sig)

cat("\n=== PERGUNTA 2: RANKING DE IMPORTÂNCIA (|efeito| por DP) ===\n")
print(tabela_ranking)

# -----------------------------------------------------------------------
# 7. R² DO MODELO (Variação Total Explicada)
# -----------------------------------------------------------------------
cat("\n=== R² DO MODELO FINAL ===\n")
print(r2(mod_final))

# -----------------------------------------------------------------------
# 8. SALVAR RESULTADOS
# -----------------------------------------------------------------------
write_xlsx(tabela_efeitos,  
          "C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/tabela_efeitos_estrutura_cp_0.xlsx",  
          row.names = FALSE)
write_xlsx(tabela_ranking,  
          "C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/tabela_ranking_estrutura_cp_0.xlsx", 
          row.names = FALSE)
saveRDS(mod_final, 
        "C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/mod_estrutura_final_cp_0.rds")

cat("\nResultados salvos com sucesso!\n")



# -----------------------------------------------------------------------
# 8. FIGURAS
# -----------------------------------------------------------------------
# Extrair os coeficientes para legendar os gráficos automaticamente
df_coef <- tidy(mod_final, effects = "fixed") %>% 
  filter(term != "(Intercept)") %>%
  mutate(
    p_label = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ "ns"
    ),
    texto_grafico = paste0("beta = ", round(estimate, 2), " ", p_label)
  )
# Criar um vetor nomeado facilitado para resgatar os valores
texto <- setNames(df_coef$texto_grafico, df_coef$term)
# --- 1. EXTRAIR AS PREDIÇÕES CORRETAS DO MODELO MULTIVARIADO ---
# Usamos o mod_final! O ggeffects calcula o efeito de cada uma mantendo as outras na média.
# O [all] garante que ele explore toda a amplitude dos dados.

preds_size_meta <- ggpredict(mod_final, terms = "size_z [all]")
preds_cone_meta <- ggpredict(mod_final, terms = "conect_z [all]")
preds_nodf_meta <- ggpredict(mod_final, terms = "NODF_z [all]")
preds_modu_meta <- ggpredict(mod_final, terms = "Q_z [all]")
preds_core_meta <- ggpredict(mod_final, terms = "deg_core_z [all]")
preds_peri_meta <- ggpredict(mod_final, terms = "deg_peri_z [all]")

# Despadronização do Eixo X para escala real
preds_size <- as.data.frame(preds_size_meta) %>% mutate(x_real = x * sd(estruc$n_species_total) + mean(estruc$n_species_total))
preds_cone <- as.data.frame(preds_cone_meta) %>% mutate(x_real = x * sd(estruc$conectancia) + mean(estruc$conectancia))
preds_nodf <- as.data.frame(preds_nodf_meta) %>% mutate(x_real = x * sd(estruc$NODF) + mean(estruc$NODF))
preds_modu <- as.data.frame(preds_modu_meta) %>% mutate(x_real = x * sd(estruc$Q) + mean(estruc$Q))
preds_core <- as.data.frame(preds_core_meta) %>% mutate(x_real = x * sd(estruc$mean_degree_core) + mean(estruc$mean_degree_core))
preds_peri <- as.data.frame(preds_peri_meta) %>% mutate(x_real = x * sd(estruc$mean_degree_periphery) + mean(estruc$mean_degree_periphery))

# -----------------------------------------------------------------------
# 5. CONSTRUÇÃO DOS 7 GRÁFICOS INDIVIDUAIS
# -----------------------------------------------------------------------

# --- LINHA 1: Escala do Ecossistema ---
# -----------------------------------------------------------------------
# 3. CONFIGURAR OS GRÁFICOS (LEGENDA TOTALMENTE DESATIVADA EM TODOS)
# -----------------------------------------------------------------------

# --- LINHA 1 ---
p1 <- ggplot() +
  geom_point(data = estruc, aes(x = n_species_total, y = persistence_species, color = tipo_mutualismo_label), alpha = 0.02, size = 0.8, show.legend = FALSE) +
  geom_ribbon(data = preds_size, aes(x = x_real, ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "grey20") +
  geom_line(data = preds_size, aes(x = x_real, y = predicted), color = "black", linewidth = 1) +
  labs(x = "Species Richness (S)", y = "Species Persistence") +
  annotate("text", x = min(estruc$n_species_total), y = 0.05, label = texto["size_z"], hjust = 0, fontface = "italic", size = 4) +
  theme_classic() + theme(axis.text = element_text(color = "black"), aspect.ratio = 1)

p2 <- ggplot() +
  geom_point(data = estruc, aes(x = conectancia, y = persistence_species, color = tipo_mutualismo_label), alpha = 0.02, size = 0.8, show.legend = FALSE) +
  geom_ribbon(data = preds_cone, aes(x = x_real, ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "grey20") +
  geom_line(data = preds_cone, aes(x = x_real, y = predicted), color = "black", linewidth = 1) +
  labs(x = "Connectance", y = "") +
  annotate("text", x = min(estruc$conectancia), y = 0.05, label = texto["conect_z"], hjust = 0, fontface = "italic", size = 4) +
  theme_classic() + theme(axis.text = element_text(color = "black"), aspect.ratio = 1)

p3 <- ggplot() +
  geom_point(data = estruc, aes(x = NODF, y = persistence_species, color = tipo_mutualismo_label), alpha = 0.02, size = 0.8, show.legend = FALSE) +
  geom_ribbon(data = preds_nodf, aes(x = x_real, ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "grey20") +
  geom_line(data = preds_nodf, aes(x = x_real, y = predicted), color = "black", linewidth = 1) +
  labs(x = "Nestedness (NODF)", y = "") +
  annotate("text", x = min(estruc$NODF), y = 0.05, label = texto["NODF_z"], hjust = 0, fontface = "italic", size = 4) +
  theme_classic() + theme(axis.text = element_text(color = "black"), aspect.ratio = 1)

# --- LINHA 2 ---
p4 <- ggplot() +
  geom_point(data = estruc, aes(x = Q, y = persistence_species, color = tipo_mutualismo_label), alpha = 0.02, size = 0.8, show.legend = FALSE) +
  geom_ribbon(data = preds_modu, aes(x = x_real, ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "grey20") +
  geom_line(data = preds_modu, aes(x = x_real, y = predicted), color = "black", linewidth = 1) +
  labs(x = "Modularity (Q)", y = "Species Persistence") +
  annotate("text", x = min(estruc$Q), y = 0.05, label = texto["Q_z"], hjust = 0, fontface = "italic", size = 4) +
  theme_classic() + theme(axis.text = element_text(color = "black"), aspect.ratio = 1)

p5 <- ggplot() +
  geom_point(data = estruc, aes(x = mean_degree_core, y = persistence_species, color = tipo_mutualismo_label), alpha = 0.02, size = 0.8, show.legend = FALSE) +
  geom_ribbon(data = preds_core, aes(x = x_real, ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "grey20") +
  geom_line(data = preds_core, aes(x = x_real, y = predicted), color = "black", linewidth = 1) +
  labs(x = "Mean degree of core species", y = "") +
  annotate("text", x = min(estruc$mean_degree_core), y = 0.05, label = texto["deg_core_z"], hjust = 0, fontface = "italic", size = 4) +
  theme_classic() + theme(axis.text = element_text(color = "black"), aspect.ratio = 1)

# --- O p6 é o único que DEIXA a legenda nascer (show.legend = TRUE) ---
p6 <- ggplot() +
  geom_point(data = estruc, aes(x = mean_degree_periphery, y = persistence_species, color = tipo_mutualismo_label), alpha = 0.02, size = 0.8, show.legend = TRUE) +
  geom_ribbon(data = preds_peri, aes(x = x_real, ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "grey20") +
  geom_line(data = preds_peri, aes(x = x_real, y = predicted), color = "black", linewidth = 1) +
  labs(x = "Mean degree of peripheral species", y = "", color = "Mutualism") +
  annotate("text", x = min(estruc$mean_degree_periphery), y = 0.05, label = texto["deg_peri_z"], hjust = 0, fontface = "italic", size = 4) +
  theme_classic() + theme(axis.text = element_text(color = "black"), aspect.ratio = 1)


# -----------------------------------------------------------------------
# 4. EXPORTAR PAINEL DE 6 GRÁFICOS (A legenda vai para a extrema direita)
# -----------------------------------------------------------------------
tiff("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/persistence_VS_network_metrics_cp_0.tiff",
     w = 3600, h = 2400, res = 300, compression = "lzw")

layout_matriz_6 <- "
ABC
DEF
"

final_plot <- wrap_plots(A = p1, B = p2, C = p3, D = p4, E = p5, F = p6, design = layout_matriz_6) + 
  plot_layout(guides = "collect") + 
  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')') & 
  # O truque final: força as bolinhas da legenda a ficarem 100% visíveis e grandes
  guides(color = guide_legend(title = "Mutualism", override.aes = list(alpha = 1, size = 2.5))) &
  theme(
    legend.position = "right",
    plot.tag = element_text(face = "bold", size = 14),
    axis.text = element_text(color = "black")
  )

final_plot
dev.off()

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
################################################################################
################################################################################
# PHYSIOLOGICAL COST VARIABLE
################################################################################
################################################################################
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

library(lme4)
library(glmmTMB)
library(ggplot2)
library(ggeffects)
library(patchwork)
library(tidyverse)
library(broom.mixed)

# -----------------------------------------------------------------------
# 1. FILTRAR CENÁRIO E TRATAR NAs (Sem nenhuma menção a n_interacoes)
# -----------------------------------------------------------------------
resultados = readRDS("C:/Users/bruno/OneDrive/Documentos/posdoc/Suíça/projeto/resultados_modelo_completo.rds")

resultados <- resultados %>%
  mutate(
    param_nivel_BCe = dplyr::recode(param_nivel_BCe,
                                    "fraco"    = "low",
                                    "moderado" = "moderate",
                                    "forte"    = "high"
    ),
    tipo_mutualismo_label = dplyr::recode(sistema,
                                          "anemo_fish" = "anemo-fish",
                                          "ant_domacea" = "ant-domacea",
                                          "ant_NEF" = "Ant-EFN"
    )
  )

estruc <- resultados %>%
  filter(param_nivel_BCe == "moderate") %>%
  filter(!is.na(NODF), !is.na(Q), !is.na(conectancia),
         !is.na(n_species_total), !is.na(param_Cp_multiplier),
         !is.na(mean_degree_core), !is.na(mean_degree_periphery))

# -----------------------------------------------------------------------
# 2. PADRONIZAR (Z-SCORE) OS PREDITORES
# -----------------------------------------------------------------------
estruc <- estruc %>%
  mutate(
    NODF_z     = as.numeric(scale(NODF)),
    Q_z        = as.numeric(scale(Q)),
    conect_z   = as.numeric(scale(conectancia)),
    size_z     = as.numeric(scale(n_species_total)),
    deg_core_z = as.numeric(scale(mean_degree_core)),
    deg_peri_z = as.numeric(scale(mean_degree_periphery)),
    custo_z    = as.numeric(scale(param_Cp_multiplier))
  )

# -----------------------------------------------------------------------
# 3. AJUSTAR MODELO MULTIVARIADO COM INTERAÇÕES (Métrica * Custo)
# -----------------------------------------------------------------------
mod_interacao <- glmmTMB(
  cbind(n_species_final, n_species_total - n_species_final) ~
    size_z * custo_z + 
    conect_z * custo_z + 
    NODF_z * custo_z + 
    Q_z * custo_z + 
    deg_core_z * custo_z + 
    deg_peri_z * custo_z +
    (1 | nome_rede),
  data   = estruc,
  family = binomial
)

# Diagnóstico de Colinearidade para checagem (VIF e VIF Ajustado)
cat("=== DIAGNÓSTICO DE COLINEARIDADE (MODELO COMPLETO) ===\n")
print(check_collinearity(mod_interacao))
cat("\n")

# -----------------------------------------------------------------------
# 5. PERGUNTA 1 — COMO cada métrica afeta a persistência?
# -----------------------------------------------------------------------
cat("=== RESUMO DO MODELO FINAL ===\n")
print(summary(mod_interacao))

tabela_efeitos <- tidy(mod_interacao, effects = "fixed", conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ ""
    )
  ) %>%
  select(term, estimate, std.error, conf.low, conf.high, p.value, sig)

cat("\n=== PERGUNTA 1: EFEITO DE CADA MÉTRICA (em log-odds por DP) ===\n")
print(tabela_efeitos)

# -----------------------------------------------------------------------
# 6. PERGUNTA 2 — QUAL métrica é mais importante? (Ranking por |estimate|)
# -----------------------------------------------------------------------
tabela_ranking <- tabela_efeitos %>%
  mutate(abs_estimate = abs(estimate)) %>%
  arrange(desc(abs_estimate)) %>%
  select(term, estimate, abs_estimate, p.value, sig)

cat("\n=== PERGUNTA 2: RANKING DE IMPORTÂNCIA (|efeito| por DP) ===\n")
print(tabela_ranking)

# -----------------------------------------------------------------------
# 7. R² DO MODELO (Variação Total Explicada)
# -----------------------------------------------------------------------
cat("\n=== R² DO MODELO FINAL ===\n")
print(r2(mod_final))

# -----------------------------------------------------------------------
# 8. SALVAR RESULTADOS
# -----------------------------------------------------------------------
write_xlsx(tabela_efeitos,  
           "C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/tabela_efeitos_estrutura_cp_variable.xlsx",  
           row.names = FALSE)
write_xlsx(tabela_ranking,  
           "C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/tabela_ranking_estrutura_cp_variable.xlsx", 
           row.names = FALSE)
saveRDS(mod_final, 
        "C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/mod_estrutura_final_cp_variable.rds")

cat("\nResultados salvos com sucesso!\n")


# =======================================================================
#                             FIGURAS
# =======================================================================

# EXTRAÇÃO AUTOMÁTICA DOS COEFICIENTES PARA O ANNOTATE
df_coef <- tidy(mod_interacao, effects = "fixed") %>% 
  filter(term != "(Intercept)") %>%
  mutate(
    p_label = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ "ns"
    ),
    texto_grafico = paste0("beta = ", round(estimate, 2), " ", p_label)
  )

# Cria o vetor nomeado para o annotate resgatar sem erros
texto <- setNames(df_coef$texto_grafico, df_coef$term)

# -----------------------------------------------------------------------
# 4. EXTRAIR PREDIÇÕES: CUSTO NO EIXO X, SEPARADO POR NÍVEIS DA MÉTRICA
# -----------------------------------------------------------------------
preds_size <- as.data.frame(ggpredict(mod_interacao, terms = c("custo_z [all]", "size_z [-1, 0, 1]"))) %>% 
  mutate(x_real = x * sd(estruc$param_Cp_multiplier) + mean(estruc$param_Cp_multiplier))

preds_cone <- as.data.frame(ggpredict(mod_interacao, terms = c("custo_z [all]", "conect_z [-1, 0, 1]"))) %>% 
  mutate(x_real = x * sd(estruc$param_Cp_multiplier) + mean(estruc$param_Cp_multiplier))

preds_nodf <- as.data.frame(ggpredict(mod_interacao, terms = c("custo_z [all]", "NODF_z [-1, 0, 1]"))) %>% 
  mutate(x_real = x * sd(estruc$param_Cp_multiplier) + mean(estruc$param_Cp_multiplier))

preds_modu <- as.data.frame(ggpredict(mod_interacao, terms = c("custo_z [all]", "Q_z [-1, 0, 1]"))) %>% 
  mutate(x_real = x * sd(estruc$param_Cp_multiplier) + mean(estruc$param_Cp_multiplier))

preds_core <- as.data.frame(ggpredict(mod_interacao, terms = c("custo_z [all]", "deg_core_z [-1, 0, 1]"))) %>% 
  mutate(x_real = x * sd(estruc$param_Cp_multiplier) + mean(estruc$param_Cp_multiplier))

preds_peri <- as.data.frame(ggpredict(mod_interacao, terms = c("custo_z [all]", "deg_peri_z [-1, 0, 1]"))) %>% 
  mutate(x_real = x * sd(estruc$param_Cp_multiplier) + mean(estruc$param_Cp_multiplier))

# Renomear as legendas internas
ajustar_legendas <- function(df) {
  df %>% mutate(group = factor(group, levels = c("-1", "0", "1"), labels = c("Low (-1 SD)", "Average", "High (+1 SD)")))
}

preds_size <- ajustar_legendas(preds_size)
preds_cone <- ajustar_legendas(preds_cone)
preds_nodf <- ajustar_legendas(preds_nodf)
preds_modu <- ajustar_legendas(preds_modu)
preds_core <- ajustar_legendas(preds_core)
preds_peri <- ajustar_legendas(preds_peri)
# -----------------------------------------------------------------------
# 5. CONSTRUÇÃO DOS 6 GRÁFICOS (OMISSÃO ESTREITA DE LEGENDAS EXTRA)
# -----------------------------------------------------------------------
# Paleta para as Linhas de Predição (Cenários de SD)
cores_sd <- c(
  "Low (-1 SD)"  = "#3182bd", # Azul
  "Average"      = "black",   # Preto
  "High (+1 SD)" = "#de2d26"  # Vermelho
)

base_plot_interacao <- function(df_preds, label_texto_beta, titulo_metrica) {
  
  # Força o grupo a virar texto puro
  df_preds$group <- as.character(df_preds$group)
  
  ggplot(df_preds) +
    # 1. Bandas de confiança (Ocultadas completamente de qualquer guia)
    geom_ribbon(aes(x = x_real, ymin = conf.low, ymax = conf.high, group = group, fill = group), 
                alpha = 0.12, color = NA, show.legend = FALSE) +
    
    # 2. Linhas sólidas das predições
    geom_line(aes(x = x_real, y = predicted, group = group, color = group), 
              linewidth = 1.2, show.legend = TRUE) +
    
    # Mapeamento manual estrito
    scale_color_manual(values = cores_sd, name = "Metric Value", breaks = names(cores_sd)) +
    scale_fill_manual(values = cores_sd) +
    
    scale_x_continuous(breaks = c(0, 0.1, 0.30, 0.5)) +
    ylim(0, 1.02) +
    
    # Coeficiente do modelo no cantinho esquerdo
    annotate("text", x = 0, y = 0.05, label = label_texto_beta, hjust = 0, fontface = "italic", size = 3.8) +
    
    labs(x = "Physiological Cost", y = "Species Persistence", title = titulo_metrica) +
    
    # Força o descarte de qualquer guia de preenchimento ou de grupo neste painel
    guides(fill = "none", group = "none") +
    
    theme_classic() + 
    theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
      axis.text = element_text(color = "black"), 
      aspect.ratio = 1
    )
}

# -----------------------------------------------------------------------
# EXTRAÇÃO DINÂMICA DOS BETAS
# -----------------------------------------------------------------------
df_interacoes <- df_coef %>% filter(grepl(":", term))

get_beta_safe <- function(index) {
  if(index <= nrow(df_interacoes)) {
    return(df_interacoes$texto_grafico[index])
  } else {
    return("beta = 0.00 ns")
  }
}

# -----------------------------------------------------------------------
# RECONSTRUÇÃO DOS PAINÉIS
# -----------------------------------------------------------------------
p1 <- base_plot_interacao(preds_size, get_beta_safe(1), "Network Size")
p2 <- base_plot_interacao(preds_cone, get_beta_safe(2), "Connectance")
p3 <- base_plot_interacao(preds_nodf, get_beta_safe(3), "Nestedness (NODF)")
p4 <- base_plot_interacao(preds_modu, get_beta_safe(4), "Modularity (Q)")
p5 <- base_plot_interacao(preds_core, get_beta_safe(5), "Mean Degree Core")
p6 <- base_plot_interacao(preds_peri, get_beta_safe(6), "Mean Degree Periphery")

p2 <- p2 + labs(y = "") ; p3 <- p3 + labs(y = "")
p5 <- p5 + labs(y = "") ; p6 <- p6 + labs(y = "")

# -----------------------------------------------------------------------
# 6. EXPORTAR PAINEL - COM MATADOR DE LEGENDA FANTASMA
# -----------------------------------------------------------------------
tiff("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/interacao_custo_VS_metricas_FINAL.tiff",
     w = 3600, h = 2400, res = 300, compression = "lzw")

layout_matriz_6 <- "
ABC
DEF
"

# O operador '&' no final aplica o 'guides(fill = "none", group = "none")' 
# em absolutamente todos os painéis coletados de uma vez só.
final_plot <- wrap_plots(A = p1, B = p2, C = p3, D = p4, E = p5, F = p6, design = layout_matriz_6) + 
  plot_layout(guides = "collect") + 
  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')') & 
  guides(fill = "none", group = "none") &
  theme(
    legend.position = "right",
    plot.tag = element_text(face = "bold", size = 14),
    axis.text = element_text(color = "black")
  )

final_plot
dev.off()

