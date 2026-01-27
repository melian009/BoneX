# =============================================================================
# RODAR MIL VEZES COM PARÂMETROS FIXOS
# Investigar variação estocástica da rede
# =============================================================================

library(dplyr)

# Função simples: roda N vezes com parâmetros fixos
run_replicas <- function(
    # Número de réplicas
  n_replicas = 10000,
  
  # Parâmetros da REDE (fixos)
  sp_n = 30,
  network_type = "modular",
  internal_connectance = 0.65,
  external_connectance = 0.15,
  n_modules = 3,
  
  # Parâmetros de B e Ce (fixos)
  B_shape1 = 0.5,
  B_shape2 = 0.5,
  Ce_shape1 = 0.5,
  Ce_shape2 = 0.5,
  
  # Cp = 0 sempre!
  Cp_multiplier = 0,
  
  # Ambiente (fixo, não importa)
  A_min = 1,
  A_max = 10,
  w_min = 1,
  w_max = 3,
  t_max = 100,
  
  # Zi
  zi_min = 1,
  zi_max = 10,
  
  seed = 123
) {
  
  cat("\n")
  cat("═══════════════════════════════════════════════════════════\n")
  cat("  RODANDO", n_replicas, "RÉPLICAS\n")
  cat("═══════════════════════════════════════════════════════════\n")
  cat(sprintf("  Rede: %s (sp=%d, int=%.2f, ext=%.2f)\n", 
              network_type, sp_n, internal_connectance, external_connectance))
  cat(sprintf("  B: shape1=%.2f, shape2=%.2f\n", B_shape1, B_shape2))
  cat(sprintf("  Ce: shape1=%.2f, shape2=%.2f\n", Ce_shape1, Ce_shape2))
  cat(sprintf("  Cp: %.2f (SEM EFEITO AMBIENTAL)\n", Cp_multiplier))
  cat("═══════════════════════════════════════════════════════════\n\n")
  
  # Lista para armazenar resultados
  resultados <- list()
  
  # Loop simples
  for(i in 1:n_replicas) {
    
    # Mostrar progresso
    if(i %% 100 == 0 | i == n_replicas) {
      cat(sprintf("  Progresso: %d/%d (%.1f%%)\n", i, n_replicas, (i/n_replicas)*100))
    }
    
    set.seed(seed + i)
    
    # 1. Criar rede
    mut_net <- interaction_networks(
      sp_n = sp_n,
      type = network_type,
      n_modules = n_modules,
      internal_connectance = internal_connectance,
      external_connectance = external_connectance
    )
    
    # 2. Criar ambiente (não importa, Cp=0)
    theta <- environment(A_min, A_max, w_min, w_max, t_max = t_max)
    
    # 3. Gerar parâmetros das espécies
    B_vec <- rbeta(sp_n, B_shape1, B_shape2)
    Ce_vec <- rbeta(sp_n, Ce_shape1, Ce_shape2)
    Cp_vec <- rep(0, sp_n)  # Cp = 0!
    zi <- runif(sp_n, zi_min, zi_max)
    
    # 4. Rodar simulação
    dynamics <- simulation(mut_net, B_vec, Ce_vec, Cp_vec, zi, theta)
    
    # 5. Salvar resultados
    resultados[[i]] <- data.frame(
      replicate = i,
      n_species_final = sum(dynamics$final_state),
      prop_species_maintained = sum(dynamics$final_state) / sp_n,
      persistence_species = dynamics$prop_active_species,
      persistence_interactions = dynamics$prop_remaining_interactions,
      time_to_convergence = nrow(dynamics$state_history),
      mean_B = mean(B_vec),
      mean_Ce = mean(Ce_vec),
      total_interactions_initial = sum(mut_net),
      mean_degree_initial = mean(rowSums(mut_net))
    )
  }
  
  # Combinar em dataframe
  df <- bind_rows(resultados)
  
  cat("\n")
  cat("═══════════════════════════════════════════════════════════\n")
  cat(sprintf("  ✅ COMPLETO! %d réplicas\n", nrow(df)))
  cat("═══════════════════════════════════════════════════════════\n\n")
  
  return(df)
}


# =============================================================================
# COMO USAR
# =============================================================================

# ─────────────────────────────────────────────────────────────────────────────
# OPÇÃO 1: Rodar mil vezes com B=0.5 e Ce=0.5
# ─────────────────────────────────────────────────────────────────────────────
resultados <- run_replicas(
  n_replicas = 10000,
  
  # Rede fixa
  sp_n = 30,
  internal_connectance = 0.65,
  external_connectance = 0.15,
  
  # B e Ce fixos em 0.5
  B_shape1 = 0.5,
  B_shape2 = 0.5,
  Ce_shape1 = 0.5,
  Ce_shape2 = 0.5
)

# Ver resultados
head(resultados)
summary(resultados$prop_species_maintained)

# Salvar
write.csv(resultados, "resultados_10000_replicas_bc0.5.csv", row.names = FALSE)
saveRDS(resultados, "resultados_10000_replicasbc0.5.rds")


# ─────────────────────────────────────────────────────────────────────────────
# OPÇÃO 2: Teste rápido com 10 réplicas
# ─────────────────────────────────────────────────────────────────────────────
teste <- run_replicas(
  n_replicas = 10,
  B_shape1 = 0.5,
  Ce_shape1 = 0.5
)


# ─────────────────────────────────────────────────────────────────────────────
# OPÇÃO 3: Variar B mantendo Ce fixo (sem grid!)
# ─────────────────────────────────────────────────────────────────────────────
# Rodar separadamente para cada valor de B
resultados_B03 <- run_replicas(n_replicas = 500, B_shape1 = 0.3, Ce_shape1 = 0.5)
resultados_B05 <- run_replicas(n_replicas = 500, B_shape1 = 0.5, Ce_shape1 = 0.5)
resultados_B07 <- run_replicas(n_replicas = 500, B_shape1 = 0.7, Ce_shape1 = 0.5)

# Juntar tudo
resultados_B03$scenario <- "B=0.3"
resultados_B05$scenario <- "B=0.5"
resultados_B07$scenario <- "B=0.7"

todos <- rbind(resultados_B03, resultados_B05, resultados_B07)


# =============================================================================
# ANÁLISES
# =============================================================================

library(ggplot2)

# ─────────────────────────────────────────────────────────────────────────────
# 1. Estatísticas descritivas
# ─────────────────────────────────────────────────────────────────────────────
cat("\n📊 ESTATÍSTICAS:\n")
cat("─────────────────────────────────────────────────────────────\n")
cat(sprintf("Proporção média de espécies mantidas: %.3f\n", 
            mean(resultados$prop_species_maintained)))
cat(sprintf("Desvio padrão: %.3f\n", 
            sd(resultados$prop_species_maintained)))
cat(sprintf("Mínimo: %.3f\n", 
            min(resultados$prop_species_maintained)))
cat(sprintf("Máximo: %.3f\n", 
            max(resultados$prop_species_maintained)))
cat(sprintf("Mediana: %.3f\n", 
            median(resultados$prop_species_maintained)))


# ─────────────────────────────────────────────────────────────────────────────
# 2. Histograma
# ─────────────────────────────────────────────────────────────────────────────
tiff("Proportion of species left distribution 2.tiff", w = 2000, h = 2000, res = 300)
ggplot(resultados, aes(x = prop_species_maintained)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(prop_species_maintained)), 
             color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Proportion of species left distribution",
    subtitle = sprintf("10000 replicates | B = (alpha 1 = 0.5, alpha 2 = 0.5), 
    Ce = (alpha 1 = 0.5, alpha 2 = 0.5),  Cp=0 | Mean=%.3f | N sp = 30 | 
    internal connectance = 0.65 | external connectance = 0.15 | N modules = 3" ,
                       mean(resultados$prop_species_maintained)),
    x = "Porportion of species left",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(size = 10),
    plot.subtitle = element_text(size = 10)
  )
dev.off()

# ───────────────────────────────────────────────────────────────────────
