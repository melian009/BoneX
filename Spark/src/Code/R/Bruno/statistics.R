# =============================================================================
# TESTES ESTATÍSTICOS
# =============================================================================
getwd()
library(lme4)
library(ggpubr)
library(tidyverse)
#if(!require(readr)) install.packages("readr")
library(readr)
results1 <- read_csv("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/first simulations/results_mutualistic_networks_SIM1.csv")

results2 <- read_csv("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/first simulations/results_mutualistic_networks_SIM2.csv")
if(!require(readr)) install.packages("readr")
library(readr)

results3 <- read_csv("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/first simulations/results_mutualistic_networks_SIM3.csv")
results4 <- read_csv("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/first simulations/results_mutualistic_networks_SIM4.csv")


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

providers = lmer(persistence_species ~ costs + (1|mut_structure), res)
plot(fitted(providers), residuals(providers))
nulo <- lmer(persistence_species ~ 1 + (1|mut_structure), res)
anova(providers, nulo, test = "Chisq")
summary(providers)

tiff("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/first simulations/persistenceXcosts_sim3_Xsim4.tiff")
pdf("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/first simulations/persistenceXcosts_sim3_Xsim4.pdf",
    width = 7, height = 5)
ggplot(res, aes(x = costs, y = persistence_species, fill = service_providers))+
  geom_boxplot()+
  theme_classic()+
  theme(aspect.ratio = 1)+
  labs(x = "Costs", y = "Species persistence", fill = "Species position")
dev.off()
loss(results4, results3)
# -----------------------------------------------------------------------------
# Teste 1: Core vs Periphery vs Random - qual provedor é melhor?
# -----------------------------------------------------------------------------
# ANOVA para testar diferenças entre os 3 grupos

inter_net = function(results) {
  
  # 1. Modelos de ANOVA para o Tukey
  AOV_interacao <- aov(services_loss_relative ~ service_providers * mut_structure, data = results)
  AOV_aditivo   <- aov(services_loss_relative ~ service_providers + mut_structure, data = results)
  
  # 2. Modelos Lineares para o teste de Qui-quadrado
  # Nota: service_providers * mut_structure inclui os efeitos principais + interação
  LM_completo <- lm(services_loss_relative ~ service_providers * mut_structure, data = results)
  LM_reduzido <- lm(services_loss_relative ~ service_providers + mut_structure, data = results)
  nulo <- lm(services_loss_relative ~ 1, data = results)
  summary_completo <- summary(LM_completo)
  summary_reduzido <- summary(LM_reduzido)
  
  # Comparação de modelos
  comp_modelos = anova(LM_reduzido, LM_completo, test = "Chisq")
  ANOVA <- anova(LM_completo, nulo, test = "Chisq")
  p_valor = comp_modelos$`Pr(>Chi)`[2] # Pega o p-valor da comparação
  
  print(ANOVA)
  # 3. Lógica de decisão (Fluxo Correto)
  if (!is.na(p_valor) && p_valor < 0.05) {
    cat("\n--- Interação Significativa: Usando Modelo com Interação ---\n")
    print(TukeyHSD(AOV_interacao))
    print(comp_modelos)
    print(summary_completo)
  } else {
    cat("\n--- Interação NÃO Significativa: Usando Modelo Aditivo ---\n")
    print(TukeyHSD(AOV_aditivo))
    print(comp_modelos)
    print(summary_reduzido)
  }
  
}


inter_net(results4)
inter_net(results3)
# -----------------------------------------------------------------------------
# Teste 2: Persistência de Core vs Periphery DENTRO de cada rede
# -----------------------------------------------------------------------------
# Teste t pareado comparando core_persistence vs periphery_persistence
persistence <- function(results){

results_test <- results %>%
  mutate(diff_core_periphery = core_persistence - periphery_persistence)

t_test_result <- t.test(results_test$core_persistence, 
                        results_test$periphery_persistence,
                        paired = TRUE)

print("=== Teste t: Core vs Periphery ===")
print(t_test_result)
print(paste("Core sobrevive mais?", 
            ifelse(t_test_result$p.value < 0.05, "SIM", "NÃO")))
}

persistence(results1)
persistence(results2)
persistence(results3)
persistence(results4)

# -----------------------------------------------------------------------------
# Teste 3: Por estrutura de rede
# -----------------------------------------------------------------------------
for(structure in c("nested", "modular", "random")) {
  cat(sprintf("\n=== %s ===\n", toupper(structure)))
  
  data_subset <- results %>% filter(mut_structure == structure)
  
  anova_subset <- aov(persistence_species ~ service_providers, 
                      data = data_subset)
  
  print(summary(anova_subset))
}


# -----------------------------------------------------------------------------
# Teste 4: Persistencia por posição
# -----------------------------------------------------------------------------

teste = lmer(persistence_species ~ mut_structure + (1|service_providers), data = results)
null = lmer(persistence_species ~ 1 +(1|service_providers), data = results)
anova(teste, null, test = "Chisq")
summary(teste)
teste = lm(persistence_species ~ service_providers * mut_structure , data = results3)
anova(teste)

summary(teste)
```

---
  
  ## 📊 RESUMO DO GRID
  
  ### Grid de Simulações:
  ```
3 estruturas × 3 provedores × N replicatas

Estruturas:
  ├── nested     (aninhada)
├── modular    (modular)
└── random     (aleatória)

Provedores:
  ├── core       (apenas espécies centrais provêm serviços)
├── periphery  (apenas espécies periféricas provêm serviços)
└── random     (todas as espécies podem prover, conexão aleatória)

Com 10 replicatas = 90 simulações totais
Com 20 replicatas = 180 simulações totais