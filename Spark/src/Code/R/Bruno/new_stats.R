library(lme4)
library(ggpubr)
library(tidyverse)
#if(!require(readr)) install.packages("readr")
library(readr)
library(glmmTMB)
library(ggplot2)
library(openxlsx2)
library(ggeffects)
library(bbmle)

resultados = readRDS("C:/Users/bruno/OneDrive/Documentos/posdoc/Suíça/projeto/resultados_modelo_completo.rds")
str(resultados)



resultados <- resultados %>%
  mutate(
    param_nivel_BCe = recode(param_nivel_BCe,
                             "fraco"    = "low",
                             "moderado" = "moderate",
                             "forte"    = "high"
    )
  )
resultados <- resultados %>%
  mutate(
    tipo_mutualismo_label = recode(sistema,
                                   "anemo_fish" = "anemo-fish",
                                   "ant_domacea" = "ant-domacea",
                                   "ant_NEF" = "Ant-EFN",
                                   
    )
  )

resultados$service_providers = as.factor(resultados$service_providers)
################################################################################
# 1. General Effects of mutualistic net benefit 
################################################################################

## 1.1 mutualistic effects on species persistence
mod <- glmmTMB(
  cbind(n_species_final, n_species_total - n_species_final) ~ 
    param_nivel_BCe + (1 | nome_rede),
  data   = resultados,
  family = binomial
)
# Extrai o parâmetro de dispersão direto do modelo glmmTMB
sigma(mod)
install.packages("DHARMa")
library(DHARMa)

# 1. Simula os resíduos baseados no seu modelo
residuos_simulados <- simulateResiduals(fittedModel = mod)

# 2. Roda o teste formal de superdispersão
testDispersion(residuos_simulados)

null = glmmTMB(
  cbind(n_species_final, n_species_total - n_species_final) ~ 
    1 + (1 | nome_rede),
  data   = resultados,
  family = binomial
)

anova(mod, null, test = "Chisq")
summary(mod)


# 1.1.a. Figure





preds <- ggpredict(mod, terms = "param_nivel_BCe", bias_correction = TRUE)
tiff("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/sp_pers_VS_net_beneft.tiff",
     w = 2000, h = 2000, res = 300, compression = "lzw")
ggplot() +
  # Dados brutos como fundo
  geom_jitter(data = resultados, 
              aes(x = param_nivel_BCe, y = persistence_species, color = tipo_mutualismo_label),
              alpha = 0.08, width = 0.15, size = 0.8) +
  
  # Predição do modelo — cor FIXA, sem mapear para legenda
  geom_ribbon(data = preds, 
              aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, group = 1), 
              alpha = 0.25, fill = "grey30") +
  geom_line(data = preds, 
            aes(x = x, y = predicted, group = 1), 
            color = "black", linewidth = .75) +
  
  labs(
    x = "Net benefit of mutualism",
    y = "Species persistence",
    color = "Mutualism"
  ) +
  
 # scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
#                                "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))+
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 2.5))) +
  
  theme_classic()+
  theme(aspect.ratio = 1)

dev.off()

################################################################################
#1.2 Mutualism effect on time to convergence
mod_ttc <- glmmTMB(
  time_to_convergence ~ param_nivel_BCe  + (1| nome_rede),
  data   = resultados,
  family = nbinom2()  # binomial negativa tipo II (variância quadrática na média)
)

null = glmmTMB(
  time_to_convergence ~ 1 +
   (1| nome_rede),
  data   = resultados,
  family = nbinom2()  # binomial negativa tipo II (variância quadrática na média)
)

anova(mod_ttc, null, test = "Chisq")
summary(mod_ttc)



# 1.2.a  Figure


preds <- ggpredict(mod_ttc, terms = "param_nivel_BCe", bias_correction = TRUE)
tiff("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/time_equilibrium_VS_net_beneft.tiff",
     w = 2000, h = 2000, res = 300, compression = "lzw")
ggplot() +
  # Dados brutos como fundo
  geom_jitter(data = resultados, 
              aes(x = param_nivel_BCe, y = time_to_convergence, color = tipo_mutualismo_label),
              alpha = 0.08, width = 0.15, size = 0.8) +
  
  # Predição do modelo — cor FIXA, sem mapear para legenda
  geom_ribbon(data = preds, 
              aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, group = 1), 
              alpha = 0.25, fill = "grey30") +
  geom_line(data = preds, 
            aes(x = x, y = predicted, group = 1), 
            color = "black", linewidth = .75) +
  
  labs(
    x = "Net benefit of mutualism",
    y = "Time to equilibrium",
    color = "Mutualism"
  ) +
  
   #scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
  #                             "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))+
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 2.5))) +
  
  theme_classic()+
  theme(aspect.ratio = 1) +
  
  scale_y_continuous(
    breaks = seq(0, 10, by = 2)
  )

dev.off()


################################################################################
# 2. General Effects phisiological costs 
################################################################################

# 2.1 Physiological costs over moderate mutualism
phisio = resultados %>%
  filter(param_nivel_BCe == "moderate")
phisio$param_Cp_multiplier = as.factor(phisio$param_Cp_multiplier)

#param_Cp_multiplier
cp <- glmmTMB(
  cbind(n_species_final, n_species_total - n_species_final) ~ 
    param_Cp_multiplier + (1 | nome_rede),
  data   = phisio,
  family = binomial
)

null <- glmmTMB(
  cbind(n_species_final, n_species_total - n_species_final) ~ 
    1 + (1 | nome_rede),
  data   = phisio,
  family = binomial
)

anova(cp, null, test = "Chisq")
summary(cp)

# 2.1.a Figure

preds <- ggpredict(cp, terms = "param_Cp_multiplier", bias_correction = TRUE)
tiff("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/sp_pers_VS_Cp.tiff",
     w = 2000, h = 2000, res = 300, compression = "lzw")
ggplot() +
  # Dados brutos como fundo
  geom_jitter(data = phisio, 
              aes(x = param_Cp_multiplier, y = persistence_species, color = tipo_mutualismo_label),
              alpha = 0.08, width = 0.15, size = 0.8) +
  
  # Predição do modelo — cor FIXA, sem mapear para legenda
  geom_ribbon(data = preds, 
              aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, group = 1), 
              alpha = 0.25, fill = "grey30") +
  geom_line(data = preds, 
            aes(x = x, y = predicted, group = 1), 
            color = "black", linewidth = .75) +
  
  labs(
    x = "Physiological costs",
    y = "Species persistence",
    color = "Mutualism"
  ) +
  
  # scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
  #                                "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))+
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 2.5))) +
  
  theme_classic()+
  theme(aspect.ratio = 1)
dev.off()

################################################################################
# 2. Persistence of Species position at network 
################################################################################

#3.1 Descriptive
str(resultados)
cv_core = sd(resultados$n_core_total) /
mean(resultados$n_core_total) 

cv_peri = sd(resultados$n_periphery_total) /
  mean(resultados$n_periphery_total) 
cv_peri

#3.2.a Position x persistence core species

core_peri = resultados %>%
  filter(param_nivel_BCe == "moderate",
         service_providers != "random")
table(core_peri$service_providers)



position <- glmmTMB(
  cbind(n_species_final, n_species_total - n_species_final) ~ 
    service_providers + (1 | nome_rede),
  data   = core_peri,
  family = binomial
)

null  <- glmmTMB(
  cbind(n_species_final, n_species_total - n_species_final) ~ 
    1 + (1 | nome_rede),
  data   = core_peri,
  family = binomial
)

anova(position, null, test = "Chisq")
summary(position)


#3.2.b Figure

preds <- ggpredict(position, terms = "service_providers", bias_correction = TRUE)
tiff("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/sp_pers_VS_Position.tiff",
     w = 2000, h = 2000, res = 300, compression = "lzw")


ggplot() +
  # Dados brutos como fundo
  geom_jitter(data = core_peri, 
              aes(x = service_providers, y = persistence_species, color = tipo_mutualismo_label),
              alpha = 0.08, width = 0.15, size = 0.8) +
  
  # Predição do modelo — cor FIXA, sem mapear para legenda
  geom_ribbon(data = preds, 
              aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, group = 1), 
              alpha = 0.25, fill = "grey30") +
  geom_line(data = preds, 
            aes(x = x, y = predicted, group = 1), 
            color = "black", linewidth = .75) +
  
 # geom_pointrange(data = preds, 
  #                aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high),
  #                color = "black", fill = "white", shape = 21, size = 1, stroke = 1.3) +
  
  #geom_errorbar(data = preds, 
  #              aes(x = x, ymin = conf.low, ymax = conf.high),
   #             color = "black", 
   #             width = 0.1,       # Largura da barrinha horizontal (ganha destaque visual)
   #             linewidth = 1.2) +
  
  labs(
    x = "Species position",
    y = "Species persistence",
    color = "Mutualism"
  ) +
  
  # scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
  #                                "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))+
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 2.5))) +
  
  theme_classic()+
  theme(aspect.ratio = 1)
dev.off()

################################################################################
# 4. Persistence of Species effects of network structure
################################################################################
str(resultados)



#4.1 descriptive
descriptive <- resultados %>%
  group_by(sistema) %>%
  summarise(
    mean_Q = mean(Q, na.rm = TRUE),
    mean_NODF = mean(NODF, na.rm = TRUE),
    mean_C = mean(conectancia, na.rm = TRUE),
    mean_interactions = mean(n_interacoes, na.rm = TRUE),
    mean_species = mean(n_species_total, na.rm = TRUE),
    mean_persistence = mean(n_species_final, na.rm = TRUE),
    
    sd_Q = sd(Q, na.rm = TRUE),
    sd_NODF = sd(NODF, na.rm = TRUE),
    sd_C = sd(conectancia, na.rm = TRUE),
    sd_interactions = sd(n_interacoes, na.rm = TRUE),
    sd_species = sd(n_species_total, na.rm = TRUE),
    sd_persistence = sd(n_species_final, na.rm = TRUE),
  )
#write.xlsx(descriptive, "C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/descriptive.xlsx")

#4.2 data manegement - pivot longer


## =============================================================================
# ANÁLISE: EFEITO DAS MÉTRICAS ESTRUTURAIS NA PERSISTÊNCIA DE ESPÉCIES
# =============================================================================
#
# Pergunta 1 — COMO cada métrica estrutural afeta a persistência?
#   -> Modelo multivariado único, coeficientes em z-score (direção, magnitude, p)
#
# Pergunta 2 — QUAL métrica é mais importante?
#   -> Ranking dos |coeficientes| padronizados do MESMO modelo multivariado
#
# Cenário fixado em param_nivel_BCe == "moderate" (ajuste se quiser outro)
# =============================================================================

library(dplyr)
library(glmmTMB)
library(car)
library(broom.mixed)
install.packages("broom.mixed")
# -----------------------------------------------------------------------
# 1. FILTRAR CENÁRIO E TRATAR NAs (NODF/Q podem faltar em redes grandes)
# -----------------------------------------------------------------------

estruc <- resultados %>%
  filter(param_nivel_BCe == "moderate")

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

redes_excluidas <- resultados %>%
  filter(param_nivel_BCe == "moderate") %>%
  filter(is.na(NODF) | is.na(Q)) %>%
  distinct(nome_rede)

cat(sprintf("Redes únicas excluídas (NODF/Q ausente): %d\n\n", nrow(redes_excluidas)))


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
# 3. CHECAR MULTICOLINEARIDADE ENTRE PREDITORES (antes do VIF do modelo)
# -----------------------------------------------------------------------

cat("=== MATRIZ DE CORRELAÇÃO ENTRE PREDITORES (z-score) ===\n")
cor_matrix <- cor(estruc[, c("NODF_z","Q_z","conect_z","size_z",
                             "n_inter_z","deg_core_z","deg_peri_z")],
                  use = "complete.obs")
print(round(cor_matrix, 2))
cat("\n")


# -----------------------------------------------------------------------
# 4. MODELO MULTIVARIADO COMPLETO
#
#    NOTA: n_interacoes_z tende a ser quase colinear com size_z e
#    conect_z (interações = conectância × n_plantas × n_animais).
#    Se o VIF de n_inter_z for muito alto, ele é removido no passo 5.
# -----------------------------------------------------------------------

mod_full <- glmmTMB(
  cbind(n_species_final, n_species_total - n_species_final) ~
    NODF_z + Q_z + conect_z + size_z + n_inter_z + deg_core_z + deg_peri_z +
    (1 | nome_rede),
  data   = estruc,
  family = binomial
)

cat("=== VIF DO MODELO COMPLETO ===\n")
print(vif(mod_full))
cat("\n")


# -----------------------------------------------------------------------
# 5. REMOVER PREDITORES COM VIF > 5 (UM POR VEZ, O MAIOR PRIMEIRO)
#    E REAJUSTAR O MODELO — repete até todos os VIFs ficarem <= 5
# -----------------------------------------------------------------------

remove_high_vif <- function(formula_terms, data, threshold = 5) {
  
  repeat {
    f <- as.formula(paste(
      "cbind(n_species_final, n_species_total - n_species_final) ~",
      paste(formula_terms, collapse = " + "),
      "+ (1 | nome_rede)"
    ))
    
    m <- glmmTMB(f, data = data, family = binomial)
    
    if (length(formula_terms) <= 1) return(list(model = m, terms = formula_terms))
    
    v <- vif(m)
    if (max(v) <= threshold) return(list(model = m, terms = formula_terms))
    
    termo_remover <- names(v)[which.max(v)]
    cat(sprintf("Removendo '%s' (VIF = %.2f)\n", termo_remover, max(v)))
    formula_terms <- setdiff(formula_terms, termo_remover)
  }
}

cat("=== SELEÇÃO DE PREDITORES (VIF <= 5) ===\n")
todos_termos <- c("NODF_z","Q_z","conect_z","size_z","n_inter_z","deg_core_z","deg_peri_z")

resultado_vif <- remove_high_vif(todos_termos, estruc, threshold = 5)
mod_final     <- resultado_vif$model
termos_finais <- resultado_vif$terms

cat(sprintf("\nPreditores finais: %s\n\n", paste(termos_finais, collapse=", ")))
cat("=== VIF DO MODELO FINAL ===\n")
print(vif(mod_final))
cat("\n")


# -----------------------------------------------------------------------
# 6. PERGUNTA 1 — COMO cada métrica afeta a persistência?
#    (direção, magnitude, significância)
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
# 7. PERGUNTA 2 — QUAL métrica é mais importante?
#    (ranking pelo |coeficiente| padronizado do MESMO modelo)
# -----------------------------------------------------------------------

tabela_ranking <- tabela_efeitos %>%
  mutate(abs_estimate = abs(estimate)) %>%
  arrange(desc(abs_estimate)) %>%
  select(term, estimate, abs_estimate, p.value, sig)

cat("\n=== PERGUNTA 2: RANKING DE IMPORTÂNCIA (|efeito| por DP) ===\n")
print(tabela_ranking)


# -----------------------------------------------------------------------
# 8. R² DO MODELO FINAL (variação explicada)
# -----------------------------------------------------------------------

library(performance)
cat("\n=== R² DO MODELO FINAL ===\n")
print(r2(mod_final))


# -----------------------------------------------------------------------
# 9. SALVAR RESULTADOS
# -----------------------------------------------------------------------

write.csv(tabela_efeitos,  "tabela_efeitos_estrutura.csv",  row.names = FALSE)
write.csv(tabela_ranking,  "tabela_ranking_estrutura.csv",  row.names = FALSE)
saveRDS(mod_final, "mod_estrutura_final.rds")

cat("\nResultados salvos:\n")
cat("  tabela_efeitos_estrutura.csv\n")
cat("  tabela_ranking_estrutura.csv\n")
cat("  mod_estrutura_final.rds\n")

library(patchwork) # Garante a colagem perfeita dos gráficos


# --- 1. GERAR AS PREDICÕES REAIS (Usando os modelos individuais corretos) ---
# [all] garante que o ggeffects explore a amplitude real de cada variável no banco
preds_con <- as.data.frame(ggpredict(mod_conectancia, terms = "conectancia [all]", bias_correction = T))
preds_nod <- as.data.frame(ggpredict(mod_nodf, terms = "NODF [all]", bias_correction = T))
preds_mod <- as.data.frame(ggpredict(mod_modularity, terms = "Q [all]", bias_correction = T))
preds_core <- as.data.frame(ggpredict(core, terms = "mean_degree_core [all]", bias_correction = T))
preds_peri <- as.data.frame(ggpredict(periphery, terms = "mean_degree_periphery [all]", bias_correction = T))



# Filtro para o cenário moderado nos dados brutos
estruc <- resultados %>% filter(param_nivel_BCe == "moderate")


# --- 2. CONSTRUIR CADA GRÁFICO SEPARADAMENTE ---

# Gráfico A: Conectância
p1 <- ggplot() +
  geom_point(data = estruc, aes(x = conectancia, y = persistence_species, color = tipo_mutualismo_label), alpha = 0.02, size = 0.8) +
  geom_ribbon(data = preds_con, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "grey20") +
  geom_line(data = preds_con, aes(x = x, y = predicted), color = "black", linewidth = 1) +
  labs(x = "Connectance", y = "Species Persistence", color = "Mutualism") + # 'color' padronizado
  theme_classic() + 
  theme(axis.text = element_text(color = "black"), aspect.ratio = 1)

# Gráfico B: NODF
p2 <- ggplot() +
  geom_point(data = estruc, aes(x = NODF, y = persistence_species, color = tipo_mutualismo_label), alpha = 0.02, size = 0.8) +
  geom_ribbon(data = preds_nod, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "grey20") +
  geom_line(data = preds_nod, aes(x = x, y = predicted), color = "black", linewidth = 1) +
  labs(x = "Nestedness (NODF)", y = "", color = "Mutualism") + # Remove o título de Y, mantém o color
  theme_classic() + 
  theme(axis.text = element_text(color = "black"), aspect.ratio = 1)

# Gráfico C: Modularidade Q
p3 <- ggplot() +
  geom_point(data = estruc, aes(x = Q, y = persistence_species, color = tipo_mutualismo_label), alpha = 0.02, size = 0.8) +
  geom_ribbon(data = preds_mod, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "grey20") +
  geom_line(data = preds_mod, aes(x = x, y = predicted), color = "black", linewidth = 1) +
  labs(x = "Modularity (Q)", y = "", color = "Mutualism") +
  theme_classic() + 
  theme(axis.text = element_text(color = "black"), aspect.ratio = 1)

# Gráfico D: Mean degree core
p4 <- ggplot() +
  geom_point(data = estruc, aes(x = mean_degree_core, y = persistence_species, color = tipo_mutualismo_label), alpha = 0.02, size = 0.8) +
  geom_ribbon(data = preds_core, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "grey20") +
  geom_line(data = preds_core, aes(x = x, y = predicted), color = "black", linewidth = 1) +
  labs(x = "Mean degree of core species", y = "Species Persistence", color = "Mutualism") +
  theme_classic() + 
  theme(axis.text = element_text(color = "black"), aspect.ratio = 1)

# Gráfico E: Mean degree periphery
p5 <- ggplot() +
  geom_point(data = estruc, aes(x = mean_degree_periphery, y = persistence_species, color = tipo_mutualismo_label), alpha = 0.02, size = 0.8) +
  geom_ribbon(data = preds_peri, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "grey20") +
  geom_line(data = preds_peri, aes(x = x, y = predicted), color = "black", linewidth = 1) +
  labs(x = "Mean degree of peripheral species", y = "", color = "Mutualism") +
  theme_classic() + 
  theme(axis.text = element_text(color = "black"), aspect.ratio = 1)+
  guides(color = guide_legend(title = "Mutualism", override.aes = list(alpha = 1, size = 2.5)))


# --- 3. JUNTAR TUDO COM PATCHWORK E AJUSTAR LAYOUT ---

tiff("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/persistence_VS_network_metrics_PERFEITO.tiff",
     w = 3600, h = 2400, res = 300, compression = "lzw")

# Criamos uma matriz de design: 
# Linha 1: p1 (a), p2 (b), p3 (c)
# Linha 2: p4 (d), p5 (e), e '#' indica um espaço vazio planejado para a legenda unificada
layout_matriz <- "
ABC
DE#
"

# Juntamos usando o wrap_plots que interpreta essa matriz perfeitamente
final_plot <- wrap_plots(A = p1, B = p2, C = p3, D = p4, E = p5, design = layout_matriz) + 
  plot_layout(guides = "collect") + 
  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')') & 
  theme(
    legend.position = "right",                         # Direciona a legenda coletada para o espaço '#'
    plot.tag = element_text(face = "bold", size = 14), # Tags em negrito
    axis.text = element_text(color = "black")
  )

# Renderiza o painel final no TIFF
final_plot

dev.off()

