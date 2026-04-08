# =============================================================================
# TESTES ESTATÍSTICOS
# =============================================================================
getwd()
library(lme4)
library(ggpubr)
library(tidyverse)
#if(!require(readr)) install.packages("readr")
library(readr)
library(glmmTMB)
library(ggplot2)

results1 <- read_csv("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/first simulations/results_mutualistic_networks_SIM1.csv")

results2 <- read_csv("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/first simulations/results_mutualistic_networks_SIM2.csv")
if(!require(readr)) install.packages("readr")
library(readr)

results3 <- read_csv("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/first simulations/results_mutualistic_networks_SIM3.csv")
results4 <- read_csv("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/first simulations/results_mutualistic_networks_SIM4.csv")

str(results3)
# Para conferir se agora temos colunas de verdade:
head(results3[, 1:5])
results
summary(results3)
str(results2)
results3 <- results3 %>%
  mutate(costs = "high")
results4 <- results4 %>%
  mutate(costs = "low")
res = bind_rows(results3, results4)
summary(res)



providers = glm(cbind(n_species_final, 60 - n_species_final) ~ costs, family = binomial, data = res)
plot(fitted(providers), residuals(providers))
nulo <- glm(cbind(n_species_final, 60 - n_species_final) ~ 1, family = binomial, data = res)
anova(providers, nulo, test = "Chisq")
summary(providers)

# coeficientes
(b0 <- coef(providers)[1])
(b1 <- coef(providers)[2])

# probabilidades
(p_high <- plogis(b0))
(p_low  <- plogis(b0 + b1))

p_high
p_low

# odds ratio
exp(b1)



newdata <- data.frame(costs = c("high", "low"))

pred <- predict(providers,
                newdata = newdata,
                type = "link",
                se.fit = TRUE)

# converter para probabilidade
newdata$fit <- plogis(pred$fit)
newdata$lower <- plogis(pred$fit - 1.96 * pred$se.fit)
newdata$upper <- plogis(pred$fit + 1.96 * pred$se.fit)

tiff("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/first simulations/persistenceXcosts_sim3_Xsim4.tiff")
pdf("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/first simulations/persistenceXcosts_sim3_Xsim4.pdf",
    width = 7, height = 5)
ggplot() +
  geom_violin(data = res,
              aes(x = costs, y = n_species_final/60),
              width = 0.1, height = 0.02,
              alpha = 0.2, color = "red") +
  
  geom_errorbar(data = newdata,
                aes(x = costs, ymin = lower, ymax = upper),
                width = 0.1) +
  
  geom_point(data = newdata,
             aes(x = costs, y = fit),
             size = 4, color = "black") +
  
  ylim(0,1) +
  ylab("Probability of species survival")+ xlab("Costs")+
  theme(aspect.ratio = 1)+
  theme_classic()
dev.off()





# -----------------------------------------------------------------------------
# Teste 1: Core vs Periphery  - qual provedor é melhor?
# -----------------------------------------------------------------------------

library(dplyr)
library(tidyr)

res_long <- res %>%
  select(costs, mut_structure,
         core_survived, n_core_total,
         periphery_survived, n_periphery_total) %>%
  pivot_longer(
    cols = c(core_survived, periphery_survived,
             n_core_total, n_periphery_total),
    names_to = c("type", ".value"),
    names_pattern = "(core|periphery_(.*))"
  )

res_low = res_long %>%
  filter(costs == "low")

res_high = res_long %>%
  filter(costs == "high")



model_high <- glm(
  cbind(survived, total - survived) ~ type,
  family = binomial,
  data = res_high
)
nulo_high <- glm(
  cbind(survived, total - survived) ~ 1,
  family = binomial,
  data = res_high
)
anova(model, nulo, test = "Chisq")
summary(model)


# coeficientes
(b0 <- coef(model_high)[1])
(b1 <- coef(model_high)[2])

# probabilidades
(p_high <- plogis(b0))
(p_low  <- plogis(b0 + b1))

p_high
p_low

# odds ratio
exp(b1)

#######################################

model_low <- glm(
  cbind(survived, total - survived) ~ type,
  family = binomial,
  data = res_low
)
nulo_low <- glm(
  cbind(survived, total - survived) ~ 1,
  family = binomial,
  data = res_low
)
anova(model, nulo, test = "Chisq")
summary(model)


# coeficientes
(b0 <- coef(model_low)[1])
(b1 <- coef(model_low)[2])

# probabilidades
(p_high <- plogis(b0))
(p_low  <- plogis(b0 + b1))

p_high
p_low

# odds ratio
exp(b1)


##################################
newdata <- data.frame(type = c("core", "periphery"))
pred_low <- predict(model_low,
                    newdata = newdata,
                    type = "link",
                    se.fit = TRUE)

new_low <- newdata
new_low$fit   <- plogis(pred_low$fit)
new_low$lower <- plogis(pred_low$fit - 1.96 * pred_low$se.fit)
new_low$upper <- plogis(pred_low$fit + 1.96 * pred_low$se.fit)
new_low$costs <- "low"

pred_high <- predict(model_high,
                     newdata = newdata,
                     type = "link",
                     se.fit = TRUE)

new_high <- newdata
new_high$fit   <- plogis(pred_high$fit)
new_high$lower <- plogis(pred_high$fit - 1.96 * pred_high$se.fit)
new_high$upper <- plogis(pred_high$fit + 1.96 * pred_high$se.fit)
new_high$costs <- "high"

newdata_all <- rbind(new_low, new_high)
res_long$prop <- res_long$survived / res_long$total



tiff("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/first simulations/persistenceXtype.tiff")
pdf("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/first simulations/persistenceXtype.pdf",
    width = 7, height = 5)

ggplot() +
  geom_jitter(data = res_long,
              aes(x = costs, y = prop, fill = type),
              alpha = 0.3,
              position = position_jitterdodge(jitter.width = 0.5,
                                              dodge.width = 0.8),
              color = "gray") +
  
  geom_violin(data = res_long,
              aes(x = costs, y = prop, fill = type),
              alpha = 0.3,
              position = position_dodge(width = 0.8))+
  
  geom_errorbar(data = newdata_all,
                aes(x = costs, ymin = lower, ymax = upper, color = type),
                width = 0.15,
                position = position_dodge(width = 0.8)) +
  
  geom_point(data = newdata_all,
             aes(x = costs, y = fit, color = type),
             size = 3,
             position = position_dodge(width = 0.8)) +
  
  ylim(0,1) +
  ylab("Probability of species survival") +
  xlab("Costs") +
  theme_classic()

dev.off()
# -----------------------------------------------------------------------------
# Teste 2: Persistencia vs estrutura da rede
# -----------------------------------------------------------------------------
library(dplyr)
library(tidyr)

res_long <- res %>%
  select(costs, mut_structure,
         core_survived, n_core_total,
         periphery_survived, n_periphery_total) %>%
  pivot_longer(
    cols = c(core_survived, periphery_survived,
             n_core_total, n_periphery_total),
    names_to = c("type", ".value"),
    names_pattern = "(core|periphery_(.*))"
  )

res_low = res_long %>%
  filter(costs == "low")

res_high = res_long %>%
  filter(costs == "high")



model_high <- glm(
  cbind(survived, total - survived) ~ mut_structure,
  family = binomial,
  data = res_high
)
nulo_high <- glm(
  cbind(survived, total - survived) ~ 1,
  family = binomial,
  data = res_high
)
anova(model_high, nulo_high, test = "Chisq")
summary(model_high)

library(emmeans)

emm_high <- emmeans(model_high, pairwise ~ mut_structure, type = "response")

#######################################

model_low <- glm(
  cbind(survived, total - survived) ~ mut_structure,
  family = binomial,
  data = res_low
)
nulo_low <- glm(
  cbind(survived, total - survived) ~ 1,
  family = binomial,
  data = res_low
)
anova(model_low, nulo_low, test = "Chisq")
summary(model_low)

emm_low <- emmeans(model_low, pairwise ~ mut_structure, type = "response")


##
library(multcomp)
library(multcompView)

cld_low <- cld(emm_low, Letters = letters, adjust = "tukey")

df_low <- as.data.frame(cld_low)
df_low$costs <- "low"


cld_high <- cld(emm_high, Letters = letters, adjust = "tukey")

df_high <- as.data.frame(cld_high)
df_high$costs <- "high"

plot_df <- rbind(df_low, df_high)

tiff("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/first simulations/persistenceXstructure.tiff")
pdf("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/first simulations/persistenceXstructure.pdf",
    width = 7, height = 5)

plot_df$label_y <- plot_df$asymp.UCL + 0.1

pd <- position_dodge(width = 0.8)

ggplot(plot_df,
       aes(x = costs, y = prob, color = mut_structure)) +
  
  geom_jitter(data = res_long,
              aes(x = costs, y = prop, fill = mut_structure),
              alpha = 0.3,
              position = position_jitterdodge(jitter.width = 0.5,
                                              dodge.width = 0.8),
              color = "gray") +
  
  geom_violin(data = res_long,
              aes(x = costs, y = prop, fill = mut_structure),
              alpha = 0.3,
              position = position_dodge(width = 0.8))+
  
  
  geom_point(position = pd, size = 3) +
  
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = pd,
                width = 0.15) +
  
 
  
  ylim(0,1) +
  labs(y = "Probability of species survival", x ="Costs", fill = "Structure",
       color = "Structure") +
  theme_classic()

dev.off()

# -----------------------------------------------------------------------------
# Teste 3: Service vs species persistence
# -----------------------------------------------------------------------------
str(res)

persist_sp = lm(services_loss_relative ~ persistence_species, data = results4)
persist_int = lm(services_loss_relative ~ persistence_species, data = results4)
providers = lm(services_loss_relative ~ service_providers, data = results4)
structure = lm(services_loss_relative ~ mut_structure, data = results4)
mix = lm(services_loss_relative ~ service_providers * mut_structure, data = results4)
nulo = lm(services_loss_relative ~ 1, data = results4)
library(bbmle)
AICtab(persist_sp, persist_int, providers, structure, mix, nulo, base = T, delta = T, weights = T)

summary(mix)

library(emmeans)
compare = emmeans(mix, pairwise ~  service_providers * mut_structure, type = "response")

contrasts = compare$contrasts
contrasts = as_tibble(contrasts)

constrasts = contrasts %>%
  mutate(signif = if_else(p.value <= 0.05, "*", ""))

library(openxlsx)
write.xlsx(contrasts, "C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/first simulations/contrasts_full.xlsx")
constrasts

summary(providers)
contrasts = emmeans(providers, pairwise ~  service_providers, type = "response")
contrasts = compare$contrasts
contrasts = as_tibble(contrasts)
write.xlsx(contrasts, "C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/first simulations/contrasts_providers.xlsx")

contrasts = emmeans(structure, pairwise ~  mut_structure, type = "response")
contrasts = compare$contrasts
contrasts = as_tibble(contrasts)
write.xlsx(contrasts, "C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/first simulations/contrasts_structure.xlsx")

str(results4)
library(sjPlot)
library(ggeffects)
library(ggplot2)

# 1. Gera as predições do modelo
# Se o seu modelo for 'mix', e você quer ver o efeito de 'mut_structure'
#predicoes <- predict_response(providers, terms = c("mut_structure", "service_providers"))

predicoes <- as.data.frame(predict_response(persist_sp, terms = "persistence_species"))
tiff("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/first simulations/serviceXspecies.tiff")
pdf("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/first simulations/serviceXspecies.pdf",
    width = 7, height = 5)

ggplot() +
  # 1. Pontos: Apenas os dados usados NESTE modelo
  geom_jitter(data = results4, 
              aes(x = persistence_species, y = services_loss_relative, col = service_providers), 
              alpha = 0.4, width = 0.1) +
  
  # 2. Intervalo de Confiança do modelo
  geom_ribbon(data = predicoes, 
              aes(x = x, ymin = conf.low, ymax = conf.high, group = 1), 
              alpha = 0.2, fill = "steelblue") +
  
  # 3. Linha de Predição do modelo
  geom_line(data = predicoes, 
            aes(x = x, y = predicted, group = 1), 
            linewidth = 1.5, color = "black") +
  
  # Estética
  theme_classic() +
  labs(x = "Species persistence", 
       y = "Relative loss os ecosystem services",
       col = "Service providers") # Nome do grupo aqui
dev.off()

predicoes <- as.data.frame(predict_response(providers, terms = "service_providers"))
predicoes <- as.data.frame(predict_response(structure, terms = "mut_structure"))
tiff("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/first simulations/serviceXstructure.tiff")
pdf("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/first simulations/serviceXstructure.pdf",
    width = 7, height = 5)



ggplot() +
  # 1. Pontos: Apenas os dados usados NESTE modelo
  geom_jitter(data = results4, 
              aes(x = mut_structure, y = services_loss_relative), 
              alpha = 0.4, width = 0.1, color = "gray") +
  
  geom_violin(data = results4, 
              aes(x = mut_structure, y = services_loss_relative, fill = mut_structure), 
              alpha = 0.4, width = 0.1) +
  
  # 2. Intervalo de Confiança do modelo
  geom_ribbon(data = predicoes, 
              aes(x = x, ymin = conf.low, ymax = conf.high, group = 1), 
              alpha = 0.2, fill = "steelblue") +
  
  # 3. Linha de Predição do modelo
  geom_errorbar(data = predicoes, 
                aes(x = x, ymin = conf.low, ymax = conf.high), 
                width = 0.15, linewidth = 0.8, color = "black") +
  
  # Estética
  theme_classic() +
  theme(aspect.ratio = 1, legend.position = "none")+
  labs(x = "Interaction network structure", 
       y = "Relative loss os ecosystem services",
       fill = "")+ # Nome do grupo aqui
  scale_fill_viridis_d(option = "viridis")
dev.off()

library(viridis)
