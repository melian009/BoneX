
library(ggpubr)
library(tidyverse)
#if(!require(readr)) install.packages("readr")
library(readr)
library(glmmTMB)
library(ggplot2)
library(openxlsx2)
library(ggeffects)
library(bbmle)
library(emmeans)
library(multcomp)
library(multcompView)

resultados = readRDS("C:/Users/bruno/OneDrive/Documentos/posdoc/Suíça/projeto/resultados_1DP_metricas_finais.rds")
str(resultados)



estruc <- resultados %>%
  filter(param_nivel_BCe == "moderate") %>%   # ajuste o cenário se quiser
  filter(
    !is.na(NODF), !is.na(Q), !is.na(conectancia),
    !is.na(n_species_total), !is.na(n_interacoes),
    !is.na(mean_degree_core), !is.na(mean_degree_periphery)
  )

cat("=== DATASET ===\n")
cat(sprintf("N linhas:  %d\n", nrow(estruc)))
cat(sprintf("N redes:   %d\n", length(unique(estruc$nome_rede))))
cat(sprintf("N cenário: %s\n\n", unique(estruc$param_nivel_BCe)))


# -----------------------------------------------------------------------
# 2. PADRONIZAR (Z-SCORE) OS PREDITORES
# -----------------------------------------------------------------------
str(estruc)
estruc <- estruc %>%
  mutate(
    NODF_z     = as.numeric(scale(NODF)),
    Q_z        = as.numeric(scale(Q)),
    conect_z   = as.numeric(scale(conectancia)),
    size_z     = as.numeric(scale(n_species_total)),
    deg_core_z = as.numeric(scale(mean_degree_core)),
    deg_peri_z = as.numeric(scale(mean_degree_periphery))
    # n_interacoes removido — é derivado de conectancia × tamanho (redundante)
  )

estruc$param_Cp_multiplier = as.factor(estruc$param_Cp_multiplier)
dados_pca <- estruc %>% 
  dplyr::select(NODF_z,Q_z,conect_z,size_z,deg_core_z,deg_peri_z)

pca_redes <- prcomp(dados_pca, scale. = TRUE)
summary(pca_redes)
rotation = pca_redes$rotation[, 1:6] %>%
  as.data.frame(.)
write_xlsx(rotation, "C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/PCA.xlsx" )

# 3. Salvando os eixos principais de volta no seu banco de dados
estruc$PC1 <- pca_redes$x[, 1]
estruc$PC2 <- pca_redes$x[, 2]

# (Opcional) Olhe o resumo para ver quanto da variação o PC1 e PC2 explicam
# summary(pca_redes)

# 4. Ajustando os fatores
estruc$replicate <- as.factor(estruc$replicate)


library(ggrepel)

# Extrai as coordenadas das redes (Pontos)
df_pontos <- as.data.frame(pca_redes$x)
# A correção necessária:
df_pontos$sistema <- estruc$sistema 

# Extrai a rotação das variáveis (Setas/Loadings)
df_setas <- as.data.frame(pca_redes$rotation)
df_setas$variavel <- rownames(df_setas)

df_setas$variavel <- case_when(
  df_setas$variavel == "size_z"      ~ "z-Species Richness",
  df_setas$variavel == "deg_core_z"  ~ "z-Core Degree",
  df_setas$variavel == "deg_peri_z"  ~ "z-Periphery Degree",
  df_setas$variavel == "NODF_z"      ~ "z-Nestedness",
  df_setas$variavel == "Q_z"         ~ "z-Modularity",
  df_setas$variavel == "conect_z"    ~ "z-Connectance",
  TRUE                              ~ df_setas$variavel
)

# Multiplicador para esticar as setas
multiplicador <- max(abs(df_pontos$PC1)) / max(abs(df_setas$PC1)) * 0.7
df_setas$PC1_f <- df_setas$PC1 * multiplicador
df_setas$PC2_f <- df_setas$PC2 * multiplicador

# ==========================================
# 2. MONTANDO O GRÁFICO NO GGPLOT2
# ==========================================

fig_pca <- ggplot() +
  # Agora o ggplot vai encontrar a coluna 'sistema' dentro de df_pontos!
  geom_point(data = df_pontos, aes(x = PC1, y = PC2, color = sistema), 
             alpha = 0.3, size = 2) +
  
  geom_segment(data = df_setas, aes(x = 0, y = 0, xend = PC1_f, yend = PC2_f),
               arrow = arrow(length = unit(0.25, "cm")), 
               color = "firebrick", size = 0.8) +
  
  geom_text_repel(data = df_setas, aes(x = PC1_f, y = PC2_f, label = variavel),
                  color = "black", fontface = "bold", size = 3.5,
                  box.padding = 0.3, segment.color = NA) +
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray80") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray80") +
  
  labs(
    x = "PC1 - Network Topology Continuum (43.0%)",
    y = "PC2 - Species Richness & Generalism (35.2%)",
    color = "Mutualism"
  ) +
  
  # Uma paleta de cores bonita para discriminar os sistemas
  scale_color_brewer(palette = "Set2") + 
  
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 2.5))) +
  
  theme_classic() +
  theme(
    aspect.ratio = 1, 
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 9),
    legend.position = "right"
  ) + 
  coord_cartesian(xlim = c(-4, 6), ylim = c(-6, 4)) 

print(fig_pca)
str(estru)
# ==========================================
# 3. SALVANDO EM TIFF (Mesmo padrão que definimos antes)
# ==========================================
ggsave(
  filename = "C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/pca_biplot.tiff",
  plot = fig_pca,
  width = 13,
  height = 13,
  units = "cm",
  dpi = 300,
  compression = "lzw"
)

# 5. O Modelo Estatístico Lindo e Protegido
# Agora você testa a interação dos eixos da estrutura da rede com o custo

estruc$services_retained <- estruc$services_final / estruc$services_initial

n = nrow(estruc)
estruc$services_retained_trans = (estruc$services_retained * (n-1) + 0.5) / n

mod_analise1_pca <- glmmTMB(
  services_retained_trans
  ~ (PC1 + PC2) * param_Cp_multiplier +
    (1 | nome_rede/replicate),
  data   = estruc,
  family = beta_family()
)

null_analise1_pca <- glmmTMB(
    services_retained_trans
    ~ 1 +
      (1 | nome_rede/replicate),
    data   = estruc,
    family = beta_family()
  )

anova(mod_analise1_pca , null_analise1_pca , test = "Chisq")
summary(mod_analise1_pca)


#-------------------------------------------------------------------------------
# FIGURA
#-------------------------------------------------------------------------------


# 1. Calcula as predições do modelo para a interação PC1 e Custo
pred_pc1 <- ggpredict(mod_analise1_pca, terms = c("param_Cp_multiplier", "PC1 [-2, 0, 2]"), bias_correction = TRUE)

# 2. Plota o gráfico com o ggplot2
fig_efeito_pc1 <- ggplot(pred_pc1, aes(x = x, y = predicted, group = group, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, color = NA) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_color_viridis_d(labels = c("High Modularity", "Intermediate Topology", "High Nested / Connected")) +
  labs(
    x = "Phisiological Cost",
    y = "Persistnce of Volume of ESs",
    color = "Structural Topology Continumm",
    fill = "Arquitetura da Rede (PC1)",
    title = ""
  ) +
  theme_classic()+
  theme(aspect.ratio = 1,
        # --- A MÁGICA ESTÁ AQUI ---
        # Posiciona a legenda no canto inferior direito (valores de 0 a 1)
        legend.position = c(0.98, 0.02), 
        # Alinha a legenda pelo seu próprio canto inferior direito
        legend.justification = c(1, 0), 
        
        # Adiciona uma caixa de fundo branco semi-transparente para legibilidade
        legend.background = element_rect(fill = alpha("white", 0.7), color = NA),
        # Remove a caixa ao redor das chaves da legenda
        legend.key = element_blank(),
        
        # Mantém os tamanhos de fonte que definimos
        legend.title = element_text(size = 8.5, lineheight = 0.9, face = "plain"), 
        legend.text = element_text(size = 7.5) 
  )

print(fig_efeito_pc1)
ggsave(
  filename = "C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/fig_efeito_pc1_ES.pdf", # Extensão .tiff
  plot = fig_efeito_pc1,
  width = 11,                       # 11 cm de largura dá o respiro ideal para a legenda lateral
  height = 10,                      # 10 cm de altura mantém o gráfico bem proporcional
  units = "cm",
  dpi = 300,                        # Resolução padrão de publicação
  compression = "lzw"               # Compactação sem perda de qualidade (essencial para TIFF)
)

# 1. Calcula as predições para os 3 níveis do PC2 [-2, 0, 2]
pred_pc2 <- ggpredict(mod_analise1_pca, terms = c("param_Cp_multiplier", "PC2 [-2, 0, 2]"))

# 2. Renomeia os níveis ligando a matemática à biologia das 3 métricas agrupadas
levels(pred_pc2$group) <- c(
  "Rich / Generalist", 
  "Intermediate", 
  "Poor / Specialist"
)


# 3. Plota o gráfico de linhas
fig_efeito_pc2 <- ggplot(pred_pc2, aes(x = x, y = predicted, group = group, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, color = NA) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  # Usando uma paleta qualitativa clara para os 3 perfis
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    x = "Physiological Cost",
    y = "Persistnce of Volume of ESs",
    color = "Species' Richness & \nCore-Periphery Generalism (PC2)",
    title = ""
  ) +
  theme_classic() +
  theme(aspect.ratio = 1,
        # --- A MÁGICA ESTÁ AQUI ---
        # Posiciona a legenda no canto inferior direito (valores de 0 a 1)
        legend.position = c(0.98, 0.02), 
        # Alinha a legenda pelo seu próprio canto inferior direito
        legend.justification = c(1, 0), 
        
        # Adiciona uma caixa de fundo branco semi-transparente para legibilidade
        legend.background = element_rect(fill = alpha("white", 0.7), color = NA),
        # Remove a caixa ao redor das chaves da legenda
        legend.key = element_blank(),
        
        # Mantém os tamanhos de fonte que definimos
        legend.title = element_text(size = 8.5, lineheight = 0.9, face = "plain"), 
        legend.text = element_text(size = 7.5) 
  )

print(fig_efeito_pc2)
ggsave(
  filename = "C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/fig_efeito_pc2_ES.tiff", # Extensão .tiff
  plot = fig_efeito_pc2,
  width = 11,                       # 11 cm de largura dá o respiro ideal para a legenda lateral
  height = 10,                      # 10 cm de altura mantém o gráfico bem proporcional
  units = "cm",
  dpi = 300,                        # Resolução padrão de publicação
  compression = "lzw"               # Compactação sem perda de qualidade (essencial para TIFF)
)
