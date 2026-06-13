library(lme4)
library(ggpubr)
library(tidyverse)
#if(!require(readr)) install.packages("readr")
library(readr)
library(glmmTMB)
library(ggplot2)
library(openxlsx)
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

estruc <- resultados %>%
  filter(param_nivel_BCe == "moderate") 

# 1. Modelo para Conectância
mod_conectancia <- glmmTMB(
  cbind(n_species_final, n_species_total - n_species_final) ~ 
    conectancia + (1 | nome_rede),
  data   = estruc,
  family = binomial
)


# 2. Modelo para Aninhamento (NODF)
mod_nodf <- glmmTMB(
  cbind(n_species_final, n_species_total - n_species_final) ~ 
    NODF + (1 | nome_rede),
  data   = estruc,
  family = binomial
)

# 3. Modelo para Modularidade (Q)
mod_modularity <- glmmTMB(
  cbind(n_species_final, n_species_total - n_species_final) ~ 
    Q + (1 | nome_rede),
  data   = estruc,
  family = binomial
)


null <-glmmTMB(
  cbind(n_species_final, n_species_total - n_species_final) ~ 1 + (1 | nome_rede),
  data = estruc, family = binomial
)


compare = AICtab(mod_nodf, mod_conectancia, mod_modularity, null, base = T, delta = T, weights = T)
round(compare_df$weight,3)

compare_df <- data.frame(
  Modelo = attr(compare, "row.names"),
  as.data.frame(compare)
)
class(compare_df)
write_xlsx(compare_df, "C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/aic_structure.xlsx")


library(patchwork) # Garante a colagem perfeita dos gráficos


# --- 1. GERAR AS PREDICÕES REAIS (Usando os modelos individuais corretos) ---
# [all] garante que o ggeffects explore a amplitude real de cada variável no banco
preds_con <- as.data.frame(ggpredict(mod_conectancia, terms = "conectancia [all]"))
preds_nod <- as.data.frame(ggpredict(mod_nodf, terms = "NODF [all]"))
preds_mod <- as.data.frame(ggpredict(mod_modularity, terms = "Q [all]"))

# Filtro para o cenário moderado nos dados brutos
estruc <- resultados %>% filter(param_nivel_BCe == "moderate")


# --- 2. CONSTRUIR CADA GRÁFICO SEPARADAMENTE ---

# Gráfico A: Conectância (Eixo X vai de 0 a 1)
p1 <- ggplot() +
  geom_point(data = estruc, aes(x = conectancia, y = persistence_species, color = tipo_mutualismo_label), alpha = 0.02, size = 0.8) +
  geom_ribbon(data = preds_con, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "grey20") +
  geom_line(data = preds_con, aes(x = x, y = predicted), color = "black", linewidth = 1) +
  labs(x = "Connectance", y = "Species Persistence") +
  theme_classic() + theme(legend.position = "none", axis.text = element_text(color = "black"))+
  theme(aspect.ratio = 1)

# Gráfico B: NODF (Eixo X vai de 0 a 100)
p2 <- ggplot() +
  geom_point(data = estruc, aes(x = NODF, y = persistence_species, color = tipo_mutualismo_label), alpha = 0.02, size = 0.8) +
  geom_ribbon(data = preds_nod, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "grey20") +
  geom_line(data = preds_nod, aes(x = x, y = predicted), color = "black", linewidth = 1) +
  labs(x = "Nestedness (NODF)", y = "") + # Remove o Y para não repetir no painel
  theme_classic() + theme(legend.position = "none", axis.text = element_text(color = "black"))+
  theme(aspect.ratio = 1)

# Gráfico C: Modularidade Q (Eixo X vai de 0 a 1)
p3 <- ggplot() +
  geom_point(data = estruc, aes(x = Q, y = persistence_species, color = tipo_mutualismo_label), alpha = 0.02, size = 0.8) +
  geom_ribbon(data = preds_mod, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "grey20") +
  geom_line(data = preds_mod, aes(x = x, y = predicted), color = "black", linewidth = 1) +
  labs(x = "Modularity (Q)", y = "", color = "Mutualism") +
  theme_classic() + theme(axis.text = element_text(color = "black")) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 2.5)))+
  theme(aspect.ratio = 1)


# --- 3. JUNTAR TUDO COM PATCHWORK E SALVAR ---

tiff("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/persistence_VS_network_metrics_FIXED.tiff",
     w = 3200, h = 1200, res = 300, compression = "lzw")

# O operador '|' coloca os gráficos lado a lado e 'plot_layout(guides = "collect")' unifica a legenda à direita
(p1 | p2 | p3) + plot_layout(guides = "collect")

dev.off()

