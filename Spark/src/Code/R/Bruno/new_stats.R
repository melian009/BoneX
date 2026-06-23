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
library(emmeans)
library(multcomp)
library(multcompView)

resultados = readRDS("C:/Users/bruno/OneDrive/Documentos/posdoc/Suíça/projeto/resultados_1DP_metricas_finais.rds")
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
    sistema = case_when(
      str_detect(nome_rede, "M_PL") ~ "pollination",
      str_detect(nome_rede, "M_SD") ~ "dispersal",
      str_detect(nome_rede, "M_PA") ~ "ant-EFN",
      str_detect(nome_rede, "M_AD") ~ "ant-domatia",
      str_detect(nome_rede, "M_AF") ~ "anemo-fish",
      TRUE ~ "other"
    )
  )
str(resultados) 

saveRDS(resultados, "C:/Users/bruno/OneDrive/Documentos/posdoc/Suíça/projeto/resultados_1DP_metricas_finais.rds")
resultados$service_providers = as.factor(resultados$service_providers)



################################################################################
# 1. General Effects phisiological costs 
################################################################################

# 2.1 Physiological costs over moderate mutualism
phisio = resultados %>%
  filter(param_nivel_BCe == "moderate")
phisio$param_Cp_multiplier = as.factor(phisio$param_Cp_multiplier)

#param_Cp_multiplier
cp <- glmmTMB(
  cbind(n_species_final, n_species_total - n_species_final) ~ 
    param_Cp_multiplier + (1 | nome_rede/sistema),
  data   = phisio,
  family = binomial
)

null <- glmmTMB(
  cbind(n_species_final, n_species_total - n_species_final) ~ 
    1 + (1 | nome_rede/sistema),
  data   = phisio,
  family = binomial
)

anova(cp, null, test = "Chisq")
summary(cp)


# 1. Calcula as médias marginais estimadas para cada sistema
medias_sistema <- emmeans(cp, ~ param_Cp_multiplier, type = "response")

# 2. Aplica o teste de Tukey para comparar todos contra todos
teste_tukey <- pairs(medias_sistema, adjust = "tukey")

# 3. Visualizar a tabela de comparações e p-valores
print(teste_tukey)

tukey = as.data.frame(teste_tukey)
write_xlsx(tukey, "C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/tukey_sp_vs_costs.xlsx")


# 2.1.a Figure

## 1. Gerar as predições do modelo
preds <- ggpredict(cp, terms = "param_Cp_multiplier", bias_correction = TRUE)
df_preds <- as.data.frame(preds) # Colunas geradas: x, predicted, conf.low, conf.high

# 2. Extrair as letrinhas usando o objeto correto das médias
letras_cp <- cld(medias_sistema, Letters = letters, adjust = "tukey")

# 3. Preparar o dataframe de letrinhas com os nomes exatos para o join
df_letras <- as.data.frame(letras_cp) %>% 
  rename(x = param_Cp_multiplier, letra = .group) %>% 
  mutate(letra = trimws(letra))

tiff("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/sp_pers_VS_Cp.tiff",
     w = 2000, h = 2000, res = 300, compression = "lzw")

ggplot() +
  # Dados brutos do seu dataframe 'phisio'
  geom_jitter(data = phisio, 
              aes(x = param_Cp_multiplier, y = persistence_species, color = sistema),
              alpha = 0.03, size = 0.6, 
              position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6)) +
  
  # Linha conectando as médias gerais (como é o efeito puro do CP, group = 1)
  geom_line(data = df_preds, 
            aes(x = x, y = predicted, group = 1),
            color = "black", linewidth = 0.8) +
  
  # Médias estimadas e intervalos de confiança usando os nomes do 'df_preds' (x, predicted)
  geom_pointrange(data = df_preds, 
                  aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high),
                  color = "black", size = 0.7, linewidth = 1) +
  
  # Camada das letras corrigida puxando o join por 'x' e jogando fixo no Y = 1.2
  geom_text(data = df_preds %>% left_join(df_letras, by = "x"),
            aes(x = x, label = letra),
            y = 1.1,
            fontface = "bold", size = 3.5, color = "black") +
  
  labs(
    x = "Physiological Cost",
    y = "Species Persistence",
    color = "Mutualism"
  ) +
  
  # Ajustado limite para 1.3 para que o Y = 1.2 das letras apareça no gráfico
  scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, 0.2)) +
  scale_color_brewer(palette = "Set2") + 
  # Força os pontos da legenda a ficarem com tamanho grande e cor 100% opaca (alpha = 1)
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 3))) +
  
  theme_classic() +
  theme(
    aspect.ratio = 1,
    axis.text = element_text(color = "black", size = 11),
    axis.title = element_text(face = "bold", size = 12),
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )
    
dev.off()


################################################################################
# 1. General Effects sistem costs 
################################################################################

# 2.1 Physiological costs over moderate mutualism
phisio = resultados %>%
  filter(param_nivel_BCe == "moderate")
phisio$sistema = as.factor(phisio$sistema)

#param_Cp_multiplier
cp <- glmmTMB(
  cbind(n_species_final, n_species_total - n_species_final) ~ 
    sistema + (1 | nome_rede),
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

# 1. Calcula as médias marginais estimadas para cada sistema
medias_sistema <- emmeans(cp, ~ sistema, type = "response")

# 2. Aplica o teste de Tukey para comparar todos contra todos
teste_tukey <- pairs(medias_sistema, adjust = "tukey")

# 3. Visualizar a tabela de comparações e p-valores
print(teste_tukey)

tukey = as.data.frame(teste_tukey)
write_xlsx(tukey, "C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/tukey_sp_vs_system.xlsx")


# 2.1.a Figure
# 1. Gerar as predições do modelo
preds <- ggpredict(cp, terms = "sistema", bias_correction = TRUE)
df_preds <- as.data.frame(preds)

# 2. Extrair as letrinhas do Tukey usando o emmeans do seu modelo
medias_sistema <- emmeans(cp, ~ sistema, type = "response")
tukey_letras <- cld(medias_sistema, Letters = letters, adjust = "tukey")

# 3. Preparar um dataframe de letrinhas para o ggplot reconhecer as colunas
# Ajustamos o nome da coluna para bater com o eixo X ('x') do ggpredict
df_letras <- as.data.frame(tukey_letras) %>% 
  rename(x = sistema, letra = .group)

# Limpar espaços em branco que o cld() às vezes deixa nas letras
df_letras$letra <- trimws(df_letras$letra)

tiff("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/sp_pers_VS_system.tiff",
     w = 2000, h = 2000, res = 300, compression = "lzw")

ggplot() +
  # Dados brutos como fundo para mostrar a distribuição
  geom_jitter(data = phisio, 
              aes(x = sistema, y = persistence_species, color = sistema),
              alpha = 0.06, width = 0.15, size = 0.8) +
  
  # Linha conectando as médias gerais (como é o efeito puro do CP, group = 1)
  geom_line(data = df_preds, 
            aes(x = x, y = predicted, group = 1),
            color = "gray", linewidth = 0.8) +
  
  # Média e Intervalo de Confiança do modelo
  geom_pointrange(data = df_preds, 
                  aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high),
                  color = "black", size = 0.3, linewidth = 1) +
  
  # CAMADA DAS LETRAS: Adiciona as letrinhas do Tukey flutuando
  geom_text(data = df_preds %>% left_join(df_letras, by = "x"),
            aes(x = x, label = letra),
            y = 1.1,
            fontface = "bold", size = 3.5, color = "black") +
  
  labs(
    x = "Mutualism",
    y = "Species persistence",
    color = "Mutualism"
  ) +
  
  scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, 0.2)) + # Dá espaço para a letra não sumir no topo
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 2.5))) +
  
  theme_classic() +
  theme(
    aspect.ratio = 1,
    axis.text.x = element_text(angle = 35, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title = element_text(face = "bold", size = 12),
    legend.position = "none" # Como o eixo X já diz o que é, podemos ocultar a legenda para ganhar espaço
  )

dev.off()
table(eco$sistema)



################################################################################

#2.2 System and costs

# 2.1 Physiological costs over moderate mutualism
phisio = resultados %>%
  filter(param_nivel_BCe == "moderate")
phisio$sistema = as.factor(phisio$sistema)
phisio$param_Cp_multiplier = as.factor(phisio$param_Cp_multiplier)

#param_Cp_multiplier
cp <- glmmTMB(
  cbind(n_species_final, n_species_total - n_species_final) ~ 
    sistema * param_Cp_multiplier + (1 | nome_rede),
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

# 1. Calcula as médias marginais estimadas para cada sistema
medias_sistema <- emmeans(cp, ~ sistema | param_Cp_multiplier, type = "response")

# 2. Aplica o teste de Tukey para comparar todos contra todos
teste_tukey <- pairs(medias_sistema, adjust = "tukey")

# 3. Visualizar a tabela de comparações e p-valores
print(teste_tukey)

tukey = as.data.frame(teste_tukey)
write_xlsx(tukey, "C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/tukey_sp_vs_system&costs.xlsx")


# 2.1.a Figure
# 1. Gerar as predições do modelo
preds_inter <- ggpredict(cp, terms = c("param_Cp_multiplier", "sistema"), type = "fixed", bias_correction = TRUE)
df_preds <- as.data.frame(preds_inter) %>% 
  rename(param_Cp_multiplier = x, sistema = group)

tiff("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/sp_pers_VS_system&cost.tiff",
     w = 2000, h = 2000, res = 300, compression = "lzw")

ggplot() +
  # Dados brutos: pontos espalhados com transparência e organizados por sistema (dodge)
  geom_jitter(data = phisio, 
              aes(x = param_Cp_multiplier, y = persistence_species, color = sistema),
              alpha = 0.03, size = 0.6, 
              position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6)) +
  
  # Linhas conectando as médias estimadas do mesmo sistema ao longo dos custos
  geom_line(data = df_preds, 
            aes(x = param_Cp_multiplier, y = predicted, group = sistema, color = sistema),
            linewidth = 1, position = position_dodge(width = 0.6)) +
  
  # Médias estimadas e intervalos de confiança (barras verticais) do modelo por cima
  geom_pointrange(data = df_preds, 
                  aes(x = param_Cp_multiplier, y = predicted, ymin = conf.low, ymax = conf.high, color = sistema),
                  size = 0.6, linewidth = 0.9, position = position_dodge(width = 0.6)) +
  
  labs(
    x = "Physiological Cost",
    y = "Species Persistence",
    color = "Mutualism"
  ) +
  
  # Como tiramos as letras, o Y pode ir até 1.0 cravado sem precisar de margem extra no topo
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_color_brewer(palette = "Set2") + 
  
  theme_classic() +
  theme(
    aspect.ratio = 1,
    axis.text = element_text(color = "black", size = 11),
    axis.title = element_text(face = "bold", size = 12),
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )
dev.off()

################################################################################
# 2. Persistence of Species position at network 
################################################################################

#3.1 Descriptive
str(resultados)
cv_core = sd(resultados$n_core_total) /
mean(resultados$n_core_total)
cv_core

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
#install.packages("broom.mixed")
# =============================================================================
# ANÁLISE: EFEITO DAS MÉTRICAS ESTRUTURAIS NA PERSISTÊNCIA DE ESPÉCIES
# =============================================================================
#
# Três modelos complementares:
#
#   Modelo 1 — Estrutura da rede
#     Pergunta: aninhamento, modularidade e conectância afetam persistência?
#
#   Modelo 2 — Posição na rede (core/periphery)
#     Pergunta: o grau médio das espécies core e periféricas afeta persistência?
#
#   Modelo 3 — Estrutura + posição juntas (modelo integrado)
#     Pergunta: o efeito de core/periphery persiste depois de controlar
#               pela estrutura geral da rede?
#
# Todos os modelos controlam pelo tamanho da rede (n_species_total)
# e usam preditores padronizados (z-score) para comparação de magnitudes.
# =============================================================================

library(dplyr)
library(glmmTMB)
library(performance)

# -----------------------------------------------------------------------
# 1. FILTRAR CENÁRIO E TRATAR NAs
# -----------------------------------------------------------------------

estruc <- resultados %>%
  filter(param_nivel_BCe == "moderate") %>%   # ajuste o cenário se quiser
  filter(
    !is.na(NODF), !is.na(Q), !is.na(conectancia),
    !is.na(n_species_total), !is.na(n_interacoes),
    !is.na(mean_degree_core), !is.na(mean_degree_periphery)
  )

cat("=== DATASET ===\n")
cat(sprintf("N linhas:  %d\n", nrow(estruc)))
cat(sprintf("N redes:   %d\n", length(unique(estruc$nome_rede))))
cat(sprintf("N cenário: %s\n\n", unique(estruc$param_nivel_BCe)))


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
    deg_peri_z = as.numeric(scale(mean_degree_periphery))
    # n_interacoes removido — é derivado de conectancia × tamanho (redundante)
  )


# -----------------------------------------------------------------------
# 3. CHECAR CORRELAÇÕES ENTRE PREDITORES
# -----------------------------------------------------------------------

cat("=== CORRELAÇÃO ENTRE PREDITORES ===\n")
cor_matrix <- cor(
  estruc[, c("NODF_z","Q_z","conect_z","size_z","deg_core_z","deg_peri_z")],
  use = "complete.obs"
)
print(round(cor_matrix, 2))
cat("\n")


# -----------------------------------------------------------------------
# FUNÇÃO AUXILIAR: checar VIF, remover o maior se acima do threshold,
# repetir até todos ficarem abaixo do threshold
# -----------------------------------------------------------------------

remover_vif_alto <- function(formula_terms, data, threshold = 10) {
  repeat {
    f <- as.formula(paste(
      "cbind(n_species_final, n_species_total - n_species_final) ~",
      paste(formula_terms, collapse = " + "),
      "+ (1 | nome_rede)"
    ))
    m <- glmmTMB(f, data = data, family = binomial)
    
    if (length(formula_terms) <= 1) return(list(model = m, terms = formula_terms))
    
    v      <- check_collinearity(m)
    maxvif <- max(v$VIF)
    
    if (maxvif <= threshold) return(list(model = m, terms = formula_terms))
    
    termo_remover <- v$Term[which.max(v$VIF)]
    cat(sprintf("  Removendo '%s' (VIF = %.2f)\n", termo_remover, maxvif))
    formula_terms <- setdiff(formula_terms, termo_remover)
  }
}


# -----------------------------------------------------------------------
# 4. MODELO 1 — ESTRUTURA DA REDE
#    Preditores: NODF, Q, conectância, tamanho
# -----------------------------------------------------------------------

cat("=== MODELO 1: ESTRUTURA DA REDE ===\n\n")

cat("Checando VIF — Modelo 1:\n")
res1   <- remover_vif_alto(c("NODF_z","Q_z","conect_z","size_z"), estruc)
mod1   <- res1$model
cat(sprintf("Preditores finais: %s\n\n", paste(res1$terms, collapse=", ")))

cat("VIF final — Modelo 1:\n")
print(check_collinearity(mod1))

cat("\nResumo — Modelo 1:\n")
print(summary(mod1))

cat("\nR² — Modelo 1:\n")
print(r2(mod1))


# -----------------------------------------------------------------------
# 5. MODELO 2 — POSIÇÃO NA REDE (CORE/PERIPHERY)
#    Preditores: grau médio core, grau médio periphery, tamanho (controle)
# -----------------------------------------------------------------------

cat("\n=== MODELO 2: POSIÇÃO NA REDE (CORE/PERIPHERY) ===\n\n")

cat("Checando VIF — Modelo 2:\n")
res2   <- remover_vif_alto(c("deg_core_z","deg_peri_z","size_z"), estruc)
mod2   <- res2$model
cat(sprintf("Preditores finais: %s\n\n", paste(res2$terms, collapse=", ")))

cat("VIF final — Modelo 2:\n")
print(check_collinearity(mod2))

cat("\nResumo — Modelo 2:\n")
print(summary(mod2))

cat("\nR² — Modelo 2:\n")
print(r2(mod2))


# -----------------------------------------------------------------------
# 6. MODELO 3 — ESTRUTURA + POSIÇÃO JUNTAS (modelo integrado)
#    Pergunta: o efeito de core/periphery persiste após controlar
#              pela estrutura geral?
# -----------------------------------------------------------------------

cat("\n=== MODELO 3: ESTRUTURA + POSIÇÃO INTEGRADAS ===\n\n")

cat("Checando VIF — Modelo 3:\n")
res3   <- remover_vif_alto(c("NODF_z","Q_z","conect_z","size_z","deg_core_z","deg_peri_z"), estruc)
mod3   <- res3$model
cat(sprintf("Preditores finais: %s\n\n", paste(res3$terms, collapse=", ")))

cat("VIF final — Modelo 3:\n")
print(check_collinearity(mod3))

cat("\nResumo — Modelo 3:\n")
print(summary(mod3))

cat("\nR² — Modelo 3:\n")
print(r2(mod3))


# -----------------------------------------------------------------------
# 7. TABELA COMPARATIVA DE EFEITOS (os 3 modelos lado a lado)
# -----------------------------------------------------------------------

extrair_coefs <- function(mod, nome_modelo) {
  s  <- summary(mod)$coefficients$cond
  ci <- confint(mod, parm = "beta_", method = "Wald")
  
  data.frame(
    modelo    = nome_modelo,
    term      = rownames(s),
    estimate  = round(s[, "Estimate"],   3),
    std.error = round(s[, "Std. Error"], 3),
    conf.low  = round(ci[rownames(s), 1], 3),
    conf.high = round(ci[rownames(s), 2], 3),
    p.value   = round(s[, "Pr(>|z|)"],   4),
    sig       = ifelse(s[, "Pr(>|z|)"] < 0.001, "***",
                       ifelse(s[, "Pr(>|z|)"] < 0.01,  "**",
                              ifelse(s[, "Pr(>|z|)"] < 0.05,  "*", "")))
  ) %>%
    filter(term != "(Intercept)")
}

tabela_comparativa <- bind_rows(
  extrair_coefs(mod1, "Structure"),
  extrair_coefs(mod2, "Position"),
  extrair_coefs(mod3, "Integrated")
)

cat("\n=== TABELA COMPARATIVA DE EFEITOS ===\n")
print(tabela_comparativa)


# -----------------------------------------------------------------------
# 8. RANKING DE IMPORTÂNCIA — Modelo 3 (integrado)
#    Ordenado por |coeficiente| — a métrica com maior efeito por DP
# -----------------------------------------------------------------------

ranking <- tabela_comparativa %>%
  filter(modelo == "Integrated") %>%
  mutate(abs_estimate = abs(estimate)) %>%
  arrange(desc(abs_estimate)) %>%
  dplyr::select(term, estimate, abs_estimate, p.value, sig)

cat("\n=== RANKING DE IMPORTÂNCIA (Modelo integrado, |efeito| por DP) ===\n")
print(ranking)


# -----------------------------------------------------------------------
# 9. SALVAR RESULTADOS
# -----------------------------------------------------------------------

write_xlsx(tabela_comparativa, "C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/tabela_efeitos_3modelos_sp_persist.xlsx",  row.names = FALSE)
write_xlsx(ranking,            "C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/ranking_importancia_sp_persist.xlsx",      row.names = FALSE)
saveRDS(list(mod1=mod1, mod2=mod2, mod3=mod3), "C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/modelos_estrutura_sp_persist.rds")

cat("\nResultados salvos:\n")
cat("  tabela_efeitos_3modelos.csv\n")
cat("  ranking_importancia.csv\n")
cat("  modelos_estrutura.rds\n")




library(ggplot2)
library(dplyr)

# -----------------------------------------------------------------------
# PREPARAR DADOS — coeficientes do Modelo 3
# -----------------------------------------------------------------------

forest_data <- tabela_comparativa %>%
  dplyr::filter(modelo == "Integrated") %>%
  mutate(
    # Rótulos em inglês para publicação
    term_label = dplyr::recode(term,
                               "NODF_z"     = "z-Nestedness (NODF)",
                               "Q_z"        = "z-Modularity (Q)",
                               "conect_z"   = "z-Connectance",
                               "size_z"     = "z-Network size",
                               "deg_core_z" = "z-Mean degree — core",
                               "deg_peri_z" = "z-Mean degree — periphery"
    ),
    # Ordenar pelo efeito (maior positivo em cima, maior negativo embaixo)
    term_label = factor(term_label, levels = term_label[order(estimate)]),
    # Significativo ou não
    significativo = p.value < 0.05,
    # Rótulo do β para anotação dentro da figura
    beta_label = paste0("β = ", sprintf("%.3f", estimate), sig)
  )

# -----------------------------------------------------------------------
# FOREST PLOT
# -----------------------------------------------------------------------

fig_forest <- ggplot(forest_data,
                     aes(x = estimate, y = term_label, color = significativo)) +
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
  
  # IC 95% significativos — linha sólida
  geom_errorbar(
    data = forest_data %>% filter(significativo),
    aes(xmin = conf.low, xmax = conf.high),
    width = 0.2, linewidth = 0.8,
    orientation = "y"
  ) +
  
  # IC 95% não significativos — linha tracejada
  geom_errorbar(
    data = forest_data %>% filter(!significativo),
    aes(xmin = conf.low, xmax = conf.high),
    width = 0.2, linewidth = 0.8, linetype = "dashed",
    orientation = "y"
  ) +
  
  geom_point(size = 3.5) +
  
  # Anotação do β — usando a coluna diretamente
  geom_text(
    aes(x = max(conf.high, na.rm = TRUE) + 0.02,
        label = beta_label),
    hjust = 0, size = 3.2
  ) +
  
  scale_color_manual(
    values = c("TRUE" = "black", "FALSE" = "grey60"),
    guide  = "none"
  ) +
  
  labs(
    x     = "Standardized effect size (β)",
    y     = NULL,
    title = ""
  ) +
  
  scale_x_continuous(
    expand = expansion(mult = c(0.05, 0.35))
  ) +
  
  theme_classic() +
  theme(
    axis.text.y        = element_text(color = "black", size = 11),
    axis.text.x        = element_text(color = "black", size = 10),
    axis.title.x       = element_text(face = "bold", size = 12),
    plot.title         = element_text(face = "bold", size = 13),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.4)
  )


fig_forest

ggsave("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/fig_forest_model3_z.tiff", fig_forest, width = 8, height = 5, dpi = 300)
