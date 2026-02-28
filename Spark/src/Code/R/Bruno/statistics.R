# =============================================================================
# TESTES ESTATÍSTICOS
# =============================================================================
getwd()
library(ggpubr)
library(tidyverse)
if(!require(readr)) install.packages("readr")
library(readr)

results <- read_csv("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/results_mutualistic_networks_SIM2.csv")
if(!require(readr)) install.packages("readr")
library(readr)

results <- read_csv("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/results_mutualistic_networks_SIM1.csv")


# Para conferir se agora temos colunas de verdade:
head(results[, 1:5])
results
summary(results)


# -----------------------------------------------------------------------------
# Teste 1: Core vs Periphery vs Random - qual provedor é melhor?
# -----------------------------------------------------------------------------
# ANOVA para testar diferenças entre os 3 grupos
anova_persistence <- aov(services_loss_relative ~ service_providers * mut_structure, 
                         data = results)
anova_persistence1 <- aov(services_loss_relative ~ service_providers + mut_structure, 
                         data = results)
anova_persistence2 <- aov(services_loss_relative ~ service_providers, 
                          data = results)

anova(anova_persistence, anova_persistence1, test = "Chisq")
print("=== ANOVA: Persistência ===")
print(summary(anova_persistence))

# Post-hoc: quais grupos diferem?
tukey_persistence <- TukeyHSD(anova_persistence2)
print(tukey_persistence)

  # -----------------------------------------------------------------------------
# Teste 2: Persistência de Core vs Periphery DENTRO de cada rede
# -----------------------------------------------------------------------------
# Teste t pareado comparando core_persistence vs periphery_persistence
results_test <- results %>%
  mutate(diff_core_periphery = core_persistence - periphery_persistence)

t_test_result <- t.test(results_test$core_persistence, 
                        results_test$periphery_persistence,
                        paired = TRUE)

print("=== Teste t: Core vs Periphery ===")
print(t_test_result)
print(paste("Core sobrevive mais?", 
            ifelse(t_test_result$p.value < 0.05, "SIM", "NÃO")))

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