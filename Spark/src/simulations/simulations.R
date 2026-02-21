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
  n_group1 = 25,
  n_group2 = 35,
  n_services = 5,
  
  # AMBIENTE
  A_min = 5,
  A_max = 5,
  w_min = 0,
  w_max = 0,
  t_max = 1000,
  
  # Cost/benefit parameters (can be vectors)
  B_shape1 = 0.5,
  B_shape2 = 0.5,
  Ce_shape1 = 0.5,
  Ce_shape2 = 0.5,
  Cp_shape1 = 1,
  Cp_shape2 = 1,
  Cp_multiplier = 0.0,
  
  # Zi parameters
  zi_min = 5,
  zi_max = 5,
  
  # REPLICATAS
  n_replicates = 1000,
  seed = 123
)

# Salvar resultados
write.csv(results, "C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/first simulations/results_mutualistic_networks_SIM1.csv", row.names = FALSE)
saveRDS(results, "C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/first simulations/results_mutualistic_networks_SIM1.rds")

print("Resultados salvos!")
summary(results)
