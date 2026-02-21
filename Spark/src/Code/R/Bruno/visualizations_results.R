# =============================================================================
# ANÁLISE RÁPIDA
# =============================================================================
library(tidyverse)
library(dplyr)
library(ggplot2)
library(openxlsx)

# Carregar resultados (se já salvou)
# results <- readRDS("results_mutualistic_networks.rds")

# -----------------------------------------------------------------------------
# RESUMO GERAL
# -----------------------------------------------------------------------------
summary_general <- results %>%
  group_by(mut_structure, service_providers) %>%
  summarise(
    n = n(),
    mean_persistence = mean(persistence_species),
    sd_persistence = sd(persistence_species),
    mean_core_persistence = mean(core_persistence),
    mean_periphery_persistence = mean(periphery_persistence),
    mean_services_loss = mean(services_loss_relative),
    .groups = "drop"
  )

print("=== RESUMO GERAL ===")
print(summary_general)
write.xlsx(summary_general,"C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/sumary_general_SIM1.xlsx")
# -----------------------------------------------------------------------------
# TABELA: Comparação Core vs Periphery
# -----------------------------------------------------------------------------
comparison_table <- results %>%
  group_by(mut_structure, service_providers) %>%
  summarise(
    persistence_total = mean(persistence_species),
    persistence_core = mean(core_persistence),
    persistence_periphery = mean(periphery_persistence),
    services_loss = mean(services_loss_relative),
    .groups = "drop"
  ) %>%
  arrange(mut_structure, service_providers)

print("=== COMPARAÇÃO CORE VS PERIPHERY ===")
print(comparison_table)
write.xlsx(comparison_table, "C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/comparison_table_SIM1.xlsx")
# -----------------------------------------------------------------------------
# GRÁFICO 1: Persistência por Estrutura e Provedor
# -----------------------------------------------------------------------------
p1 <- ggplot(results, aes(x = service_providers, y = persistence_species, 
                          fill = service_providers)) +
  geom_boxplot() +
  facet_wrap(~ mut_structure) +
  theme_minimal() +
  labs(title = "Persistência de Espécies",
       subtitle = "Por estrutura de rede e tipo de provedor de serviços",
       x = "Provedor de Serviços",
       y = "Proporção de Espécies Persistentes") +
  theme(legend.position = "bottom")

print(p1)
ggsave("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/persistence_by_structure_provider_SIM1.png", p1, width = 10, height = 6)

# -----------------------------------------------------------------------------
# GRÁFICO 2: Perda de Serviços
# -----------------------------------------------------------------------------
p2 <- ggplot(results, aes(x = service_providers, y = services_loss_relative, 
                          fill = service_providers)) +
  geom_boxplot() +
  facet_wrap(~ mut_structure) +
  theme_minimal() +
  labs(title = "Perda Relativa de Serviços Ecossistêmicos",
       subtitle = "Por estrutura de rede e tipo de provedor",
       x = "Provedor de Serviços",
       y = "Perda Relativa") +
  theme(legend.position = "bottom")

print(p2)
ggsave("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/services_loss_by_structure_provider_SIM1.png", p2, width = 10, height = 6)

# -----------------------------------------------------------------------------
# GRÁFICO 3: Core vs Periphery Persistence
# -----------------------------------------------------------------------------
results_long <- results %>%
  select(sim_id, mut_structure, service_providers, 
         core_persistence, periphery_persistence) %>%
  pivot_longer(cols = c(core_persistence, periphery_persistence),
               names_to = "group_type",
               values_to = "persistence") %>%
  mutate(group_type = ifelse(group_type == "core_persistence", 
                             "Core", "Periphery"))

p3 <- ggplot(results_long, aes(x = group_type, y = persistence, 
                               fill = group_type)) +
  geom_boxplot() +
  facet_grid(mut_structure ~ service_providers) +
  theme_minimal() +
  labs(title = "Persistência: Espécies Core vs Periphery",
       subtitle = "Por estrutura de rede mutualista e provedor de serviços",
       x = "Tipo de Espécie",
       y = "Proporção que Sobreviveu") +
  theme(legend.position = "bottom")

print(p3)
ggsave("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/core_vs_periphery_persistence_SIM1.png", p3, width = 12, height = 8)

# -----------------------------------------------------------------------------
# GRÁFICO 4: Persistência vs Serviços
# -----------------------------------------------------------------------------
p4 <- ggplot(results, aes(x = persistence_species, y = services_final,
                          color = service_providers, shape = mut_structure)) +
  geom_point(size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Relação entre Persistência e Serviços Finais",
       x = "Persistência de Espécies",
       y = "Serviços Finais",
       color = "Provedor",
       shape = "Estrutura") +
  theme(legend.position = "right")

print(p4)
ggsave("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/persistence_vs_services_SIM1.png", p4, width = 10, height = 6)
