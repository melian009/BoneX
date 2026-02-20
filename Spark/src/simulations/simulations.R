# =============================================================================
# SIMULAÇÕES COMPLETAS - GRID COMPLETO
# =============================================================================

library(dplyr)
library(ggplot2)

# Rodar grid completo com replicatas
# Total: 3 estruturas × 3 provedores × 10 replicatas = 90 simulações
results <- explore_parameters_v3(
  # GRID PRINCIPAL
  mut_structures = c("nested", "modular", "random"),
  service_providers_options = c("core", "periphery", "random"),
  
  # TAMANHOS
  n_group1 = 10,
  n_group2 = 15,
  n_services = 5,
  
  # AMBIENTE
  A_min = 1,
  A_max = 10,
  w_min = 2,
  w_max = 5,
  t_max = 100,
  
  # REPLICATAS
  n_replicates = 1000,
  seed = 123
)

# Salvar resultados
write.csv(results, "results_mutualistic_networks.csv", row.names = FALSE)
saveRDS(results, "results_mutualistic_networks.rds")

print("Resultados salvos!")
