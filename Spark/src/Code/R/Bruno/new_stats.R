library(lme4)
library(ggpubr)
library(tidyverse)
#if(!require(readr)) install.packages("readr")
library(readr)
library(glmmTMB)
library(ggplot2)

resultados = readRDS("C:/Users/bruno/OneDrive/Documentos/posdoc/Suíça/projeto/resultados_modelo_completo.rds")
str(resultados)



################################################################################
# 1. General Effects of mutualistic effect 
################################################################################

## 1.1 mutualistic effects on species persistence
mod <- glmer(
  cbind(n_species_final, n_species_total - n_species_final) ~ 
    param_nivel_BCe + (1 | nome_rede) + (1| sistema),
  data   = resultados,
  family = binomial
)

null = glmer(
  cbind(n_species_final, n_species_total - n_species_final) ~ 
    1 + (1 | nome_rede) + (1| sistema),
  data   = resultados,
  family = binomial
)

anova(mod, null, test = "Chisq")
summary(mod)


# 1.1.a. Figure

resultados <- resultados %>%
  mutate(
    param_nivel_BCe = recode(param_nivel_BCe,
                             "fraco"    = "low",
                             "moderado" = "moderate",
                             "forte"    = "high"
    ),
    nivel_BCe_label = factor(
      param_nivel_BCe,
      levels = c("low", "moderate", "high"),
      labels = c("Low net benefit\n(B-Ce = 0.20)",
                 "Moderate net benefit\n(B-Ce = 0.42)",
                 "High net benefit\n(B-Ce = 0.66)")
    )
  )

resultados <- resultados %>%
  mutate(
    tipo_mutualismo_label = recode(sistema,
                                   "anemo_fish" = "anemo-fish",
                                   "ant_domacea" = "ant-domacea",
                                   "ant_EFN" = "Ant-EFN",
                                   
    )
  )

library(ggeffects)

preds <- ggpredict(mod, terms = "param_nivel_BCe")

ggplot() +
  # Dados brutos como fundo (contexto visual, baixa opacidade)
  geom_jitter(data = resultados, 
              aes(x = param_nivel_BCe, y = persistence_species, color = sistema),
              alpha = 0.08, width = 0.15, size = 0.8) +
  
  # Predições do modelo (a inferência real)
  geom_ribbon(data = preds, 
              aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, 
                  fill = group, group = group), 
              alpha = 0.2, color = NA) +
  geom_line(data = preds, 
            aes(x = x, y = predicted, color = group, group = group), 
            linewidth = 1.2) +
  #geom_point(data = preds, 
  #           aes(x = x, y = predicted, color = group), 
   #          size = 3) +
  
  labs(
    x = "Net benefit of mutualism",
    y = "Species persistence",
    color = "Mutualism"
  ) +
  
  theme_classic()

################################################################################
cov = lmer(time_to_convergence ~ param_nivel_BCe + (1|sistema), data = resultados)
plot(fitted(cov), residuals(cov))
null = 

hist(resultados$time_to_convergence)




ggplot(resultados, aes(y = time_to_convergence, x = param_nivel_BCe, color = sistema))+
  geom_jitter(alpha = 0.3)+
  theme_classic()

ggplot(resultados, aes(y = persistence_species, x = param_nivel_BCe, color = sistema))+
  geom_jitter(alpha = 0.3)+
  theme_classic()
mean(core_peri$time_to_convergence)