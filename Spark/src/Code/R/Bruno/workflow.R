# =============================================================================
# WORKFLOW COMPLETO - VERSÃO ATUALIZADA
# =============================================================================

library(dplyr)
library(ggplot2)

# -----------------------------------------------------------------------------
# CONFIGURAÇÃO INICIAL
# -----------------------------------------------------------------------------
set.seed(123)  # Para reprodutibilidade

# Definir tamanhos dos grupos
n_plants <- 10        # Número de plantas
n_pollinators <- 15   # Número de polinizadores
n_species_total <- n_plants + n_pollinators  # Total: 25 espécies

n_services <- 5       # Número de serviços ecossistêmicos

# -----------------------------------------------------------------------------
# PASSO 1: CRIAR REDE MUTUALISTA BIPARTIDA
# -----------------------------------------------------------------------------
# Esta é a rede CORRETA biologicamente
# Plantas (linhas) interagem com Polinizadores (colunas)

bipartite_network <- mutualistic_network_bipartite(
  n_group1 = n_plants,      # 10 plantas
  n_group2 = n_pollinators, # 15 polinizadores
  type = "nested",          # Tipo de estrutura: "random", "nested", "modular", "star"
  nested_degree = 0.8,      # Grau de aninhamento (quanto maior, mais nested)
  nested_min_connectance = 0.1,
  nested_max_connectance = 0.9
)

# Ver a rede bipartida
print("=== REDE BIPARTIDA ===")
print(dim(bipartite_network))  # 10 × 15
print(bipartite_network)

# Ver quantas interações existem
n_interactions <- sum(bipartite_network)
print(paste("Número de interações:", n_interactions))

# -----------------------------------------------------------------------------
# PASSO 2: IDENTIFICAR ESPÉCIES CORE E PERIPHERY
# -----------------------------------------------------------------------------
# Isso identifica quais espécies são centrais (muitas conexões)
# e quais são periféricas (poucas conexões)

core_periphery_info <- identify_core_periphery_bipartite(
  B = bipartite_network,
  threshold_percentile = 0.5  # 50% superior = core, 50% inferior = periphery
)

# Ver resultados
print("=== CLASSIFICAÇÃO CORE/PERIPHERY ===")

print("PLANTAS:")
print("  Grau de cada planta:")
print(core_periphery_info$degree_group1)
print("  Classificação:")
print(core_periphery_info$classification_group1)
print(paste("  Core:", length(core_periphery_info$core_group1)))
print(paste("  Periphery:", length(core_periphery_info$periphery_group1)))

print("POLINIZADORES:")
print("  Grau de cada polinizador:")
print(core_periphery_info$degree_group2)
print("  Classificação:")
print(core_periphery_info$classification_group2)
print(paste("  Core:", length(core_periphery_info$core_group2)))
print(paste("  Periphery:", length(core_periphery_info$periphery_group2)))

# -----------------------------------------------------------------------------
# PASSO 3: CONVERTER PARA REDE QUADRADA
# -----------------------------------------------------------------------------
# Suas simulações precisam de uma matriz quadrada
# Esta função transforma bipartida (10×15) em quadrada (25×25)

square_network <- bipartite_to_square(bipartite_network)

# Ver a rede quadrada
print("=== REDE QUADRADA ===")
print(dim(square_network))  # 25 × 25
print(paste("É simétrica?", isSymmetric(square_network)))

# Visualizar a estrutura
print("Estrutura da matriz quadrada:")
print("  Linhas 1-10: Plantas")
print("  Linhas 11-25: Polinizadores")
print("  Bloco [1-10, 1-10]: zeros (plantas não interagem entre si)")
print("  Bloco [1-10, 11-25]: interações plantas → polinizadores")
print("  Bloco [11-25, 1-10]: interações polinizadores → plantas")
print("  Bloco [11-25, 11-25]: zeros (polinizadores não interagem entre si)")

# -----------------------------------------------------------------------------
# PASSO 4: CRIAR REDE DE SERVIÇOS ECOSSISTÊMICOS
# -----------------------------------------------------------------------------
# Agora vamos decidir QUAIS espécies provêm serviços
# Opções: "all", "core", "periphery", "random"

# Opção A: TODAS as espécies provêm serviços
services_network_all <- ecosystem_services_network_v2(
  sp_n = n_species_total,
  services_n = n_services,
  mutualistic_network = square_network,  # Passar a rede quadrada
  service_providers = "all",              # TODAS provêm serviços
  type = "random",
  distribution = "lognormal"
)

# Opção B: APENAS espécies CORE provêm serviços
services_network_core <- ecosystem_services_network_v2(
  sp_n = n_species_total,
  services_n = n_services,
  mutualistic_network = square_network,
  service_providers = "core",             # APENAS CORE provêm serviços
  threshold_percentile = 0.5,
  type = "random",
  distribution = "lognormal"
)

# Opção C: APENAS espécies PERIPHERY provêm serviços
services_network_periphery <- ecosystem_services_network_v2(
  sp_n = n_species_total,
  services_n = n_services,
  mutualistic_network = square_network,
  service_providers = "periphery",        # APENAS PERIPHERY provêm serviços
  threshold_percentile = 0.5,
  type = "random",
  distribution = "lognormal"
)

# Ver qual opção você escolheu
print("=== REDE DE SERVIÇOS ECOSSISTÊMICOS ===")
print("Opção escolhida: CORE")
print(dim(services_network_core))  # 25 × 5
print("Espécies que provêm serviços:")
print(attr(services_network_core, "allowed_providers"))

# -----------------------------------------------------------------------------
# PASSO 5: DEFINIR PARÂMETROS DAS ESPÉCIES
# -----------------------------------------------------------------------------
# Benefícios, custos e preferências ambientais de cada espécie

B_vec <- rbeta(n_species_total, 0.5, 0.5)      # Benefício que cada espécie fornece
Ce_vec <- rbeta(n_species_total, 0.5, 0.5)     # Custo ecológico de interagir
Cp_vec <- rbeta(n_species_total, 0.5, 0.5) * 0.2  # Custo fisiológico
zi <- runif(n_species_total, 1, 10)            # Ótimo ambiental de cada espécie

print("=== PARÂMETROS DAS ESPÉCIES ===")
print(paste("B_vec (benefício):", round(mean(B_vec), 3)))
print(paste("Ce_vec (custo ecológico):", round(mean(Ce_vec), 3)))
print(paste("Cp_vec (custo fisiológico):", round(mean(Cp_vec), 3)))
print(paste("zi (ótimo ambiental):", round(mean(zi), 3)))

# -----------------------------------------------------------------------------
# PASSO 6: CRIAR CONDIÇÕES AMBIENTAIS
# -----------------------------------------------------------------------------
# Ambiente varia ao longo do tempo

theta <- environment(
  A_min = 1,    # Amplitude mínima
  A_max = 10,   # Amplitude máxima
  w_min = 1,    # Frequência mínima
  w_max = 5,    # Frequência máxima
  t_max = 100   # Tempo total de simulação
)

print("=== CONDIÇÕES AMBIENTAIS ===")
print(paste("Comprimento:", length(theta)))
print(paste("Theta mínimo:", round(min(theta), 3)))
print(paste("Theta máximo:", round(max(theta), 3)))
print(paste("Theta médio:", round(mean(theta), 3)))

# -----------------------------------------------------------------------------
# PASSO 7: RODAR SIMULAÇÃO DA DINÂMICA DAS ESPÉCIES
# -----------------------------------------------------------------------------
# Esta é a sua função simulation() - NÃO precisa mudar nada nela!

resultado_dinamica <- simulation(
  A = square_network,  # Rede quadrada de interações mutualistas
  B_vec = B_vec,
  Ce_vec = Ce_vec,
  Cp_vec = Cp_vec,
  zi = zi,
  theta = theta
)

# Ver resultados da dinâmica
print("=== RESULTADOS DA DINÂMICA ===")
print(paste("Proporção de espécies persistentes:", 
            round(resultado_dinamica$prop_active_species, 3)))
print(paste("Proporção de interações mantidas:", 
            round(resultado_dinamica$prop_remaining_interactions, 3)))
print(paste("Espécies finais ativas:", sum(resultado_dinamica$final_state)))
print(paste("Tempo até convergência:", nrow(resultado_dinamica$state_history)))

# Ver quais espécies sobreviveram
especies_sobreviventes <- which(resultado_dinamica$final_state == 1)
print("Espécies que sobreviveram:")
print(names(especies_sobreviventes))

# -----------------------------------------------------------------------------
# PASSO 8: CALCULAR SERVIÇOS ECOSSISTÊMICOS
# -----------------------------------------------------------------------------
# Esta é a sua função ecosystem() - NÃO precisa mudar nada nela!

# Usando rede onde CORE provê serviços
servicos_dinamica <- ecosystem(
  object = resultado_dinamica,
  ES_matrix = services_network_core  # Escolha: _all, _core, ou _periphery
)

# Ver resultados dos serviços
print("=== SERVIÇOS ECOSSISTÊMICOS ===")
print("Serviços iniciais (t=1):")
print(servicos_dinamica$E_initial)
print("Serviços finais (t=final):")
print(servicos_dinamica$E_final)
print("Perda de serviços:")
print(servicos_dinamica$delta_E)
print(paste("Perda total:", round(sum(servicos_dinamica$delta_E), 3)))
print(paste("Perda relativa:", 
            round(sum(servicos_dinamica$delta_E) / sum(servicos_dinamica$E_initial), 3)))

# -----------------------------------------------------------------------------
# PASSO 9: VISUALIZAR RESULTADOS
# -----------------------------------------------------------------------------

# Gráfico 1: Dinâmica de espécies ao longo do tempo
plot(resultado_dinamica$prop_species_history, 
     type = "l", lwd = 2, col = "blue",
     xlab = "Tempo", ylab = "Proporção de Espécies Ativas",
     main = "Dinâmica de Persistência de Espécies")
grid()

# Gráfico 2: Dinâmica de interações ao longo do tempo
plot(resultado_dinamica$prop_interactions_history, 
     type = "l", lwd = 2, col = "red",
     xlab = "Tempo", ylab = "Proporção de Interações Mantidas",
     main = "Dinâmica de Interações")
grid()

# Gráfico 3: Serviços ao longo do tempo
servicos_totais <- rowSums(servicos_dinamica$services_history)
plot(servicos_totais, 
     type = "l", lwd = 2, col = "darkgreen",
     xlab = "Tempo", ylab = "Serviços Totais",
     main = "Dinâmica de Serviços Ecossistêmicos")
grid()

# Gráfico 4: Grau das espécies (inicial vs final)
par(mfrow = c(1, 2))
barplot(resultado_dinamica$initial_degree,
        main = "Grau Inicial",
        ylab = "Número de Conexões",
        col = "lightblue",
        las = 2)
barplot(resultado_dinamica$degree_history[nrow(resultado_dinamica$degree_history), ],
        main = "Grau Final",
        ylab = "Número de Conexões",
        col = "lightcoral",
        las = 2)
par(mfrow = c(1, 1))

print("=== WORKFLOW COMPLETO ===")
```

---
  
  ## 📝 RESUMO DO WORKFLOW
  ```
1. mutualistic_network_bipartite()
↓ (cria rede bipartida 10×15)

2. identify_core_periphery_bipartite()
↓ (identifica core/periphery)

3. bipartite_to_square()
↓ (converte para 25×25)

4. ecosystem_services_network_v2()
↓ (decide quem provê serviços: all/core/periphery)

5. Definir parâmetros (B_vec, Ce_vec, Cp_vec, zi)
↓

6. environment()
↓ (cria condições ambientais)

7. simulation()
↓ (roda dinâmica - SUA FUNÇÃO ANTIGA)

8. ecosystem()
↓ (calcula serviços - SUA FUNÇÃO ANTIGA)

9. Visualizar resultados