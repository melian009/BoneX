str(estruc)


# Criar a variável de proporção mantida
estruc$services_retained <- estruc$services_final / estruc$services_initial

# Como a regressão beta não aceita exatamente 0 ou 1 cravados, 
# aplicamos uma transformação padrão (Smithson & Verkuilen, 2006)
n <- nrow(estruc)
estruc$services_retained_trans <- (estruc$services_retained * (n - 1) + 0.5) / n

mod5 <- glmmTMB(
  services_retained_trans ~
    (NODF_z + Q_z + conect_z + size_z + deg_core_z + deg_peri_z) * custo_cat +
    (1 | nome_rede),
  data   = estruc,
  family = beta_family(),
  sparse = TRUE, # 🛠️ Ativa matrizes esparsas para economizar RAM
  control = glmmTMBControl(
    optCtrl = list(iter.max = 1500, eval.max = 2000),
    collect = FALSE
  )
)

null5 <- glmmTMB(
  services_retained_trans ~
    custo_cat +
    (1 | nome_rede),
  data   = estruc,
  family = beta_family(),
  sparse = TRUE, # 🛠️ Ativa matrizes esparsas para economizar RAM
  control = glmmTMBControl(
    optCtrl = list(iter.max = 1500, eval.max = 2000),
    collect = FALSE
  )
)

anova(mod5, null5, test = "Chisq")
summary(mod5)

print(summary(mod4))

cat("\nR² — Modelo 4:\n")
print(r2(mod5))

vif_resultado <- check_collinearity(mod4)
print(vif_resultado)
# -----------------------------------------------------------------------
# 3. EXTRAIR COEFICIENTES POR NÍVEL DE CUSTO
#    Para o forest plot, queremos o efeito de cada métrica
#    em cada nível de custo separadamente
# -----------------------------------------------------------------------

# Função para extrair coeficientes de um nível específico de custo
# usando ggeffects para obter os efeitos marginais condicionais
library(ggeffects)

extrair_por_custo <- function(mod, metrica_z, niveis_custo) {
  
  lapply(niveis_custo, function(cp) {
    
    pred <- ggpredict(
      mod,
      terms = c(paste0(metrica_z, " [-1, 0, 1]"), paste0("custo_cat [", cp, "]"))
    )
    
    # Calcular efeito como diferença entre +1DP e -1DP
    val_low  <- pred$predicted[pred$x == -1]
    val_high <- pred$predicted[pred$x ==  1]
    
    data.frame(
      custo   = cp,
      metrica = metrica_z,
      efeito  = val_high - val_low,   # diferença em escala de resposta
      low     = pred$predicted[pred$x == -1],
      mid     = pred$predicted[pred$x ==  0],
      high    = pred$predicted[pred$x ==  1]
    )
  }) %>% bind_rows()
}


# -----------------------------------------------------------------------
# 4. FOREST PLOT — um painel por nível de custo
# -----------------------------------------------------------------------
# install.packages("multcomp")
library(multcomp)

extrair_coefs_perfeitos_mod4 <- function(mod, niveis_custo) {
  metricas <- c("NODF_z", "Q_z", "conect_z", "size_z", "deg_core_z", "deg_peri_z")
  
  resultados <- lapply(niveis_custo, function(cp) {
    df_nivel <- lapply(metricas, function(m) {
      
      if (cp == "Cp = 0") {
        # No custo zero, avaliamos apenas o efeito base isolado
        lin_comb <- paste(m, "== 0")
      } else {
        # Nos outros custos, avaliamos a soma exata: Base + Interação
        # Ajustado para os espaços exatos "Cp = X.X" do seu summary
        lin_comb <- paste(m, "+", paste0("`", m, ":custo_cat", cp, "`"), "== 0")
      }
      
      # Realiza o teste de hipótese linear exato para a combinação
      g <- glht(mod, linfct = setNames(lin_comb, lin_comb))
      
      # Extrai o p-valor exato e os intervalos de confiança do modelo
      g_summary <- summary(g)
      g_confint <- confint(g)
      
      data.frame(
        term      = m,
        estimate  = g_summary$test$coefficients[1],
        conf.low  = g_confint$confint[2], # Limite inferior exato do modelo
        conf.high = g_confint$confint[3], # Limite superior exato do modelo
        p.value   = g_summary$test$pvalues[1],
        custo     = cp
      )
    }) %>% bind_rows()
    
    return(df_nivel)
  }) %>% bind_rows()
  
  # Formatação final para o GGPLOT
  resultados %>%
    mutate(
      sig           = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**", p.value < 0.05 ~ "*", TRUE ~ ""),
      significativo = p.value < 0.05,
      term_label    = dplyr::recode(term,
                                    "NODF_z"     = "Nestedness (NODF)",
                                    "Q_z"        = "Modularity (Q)",
                                    "conect_z"   = "Connectance",
                                    "size_z"     = "Network size",
                                    "deg_core_z" = "Mean degree — core",
                                    "deg_peri_z" = "Mean degree — periphery"),
      beta_label    = paste0("β=", sprintf("%.2f", estimate), sig)
    )
}

# --- Execução ---
rm(forest_int, fig_forest_int)
if(!is.null(dev.list())) dev.off()

niveis <- c("Cp = 0", "Cp = 0.1", "Cp = 0.3", "Cp = 0.5")
forest_int <- extrair_coefs_perfeitos_mod4(mod5, niveis)
write.xlsx(forest_int, "C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/ranking_importancia_VS_cost_SE_persist.xlsx" )

# Ordenar métricas pelo efeito médio entre todos os custos
ordem <- forest_int %>%
  group_by(term_label) %>%
  summarise(med = mean(estimate, na.rm = TRUE)) %>%
  arrange(med) %>%
  pull(term_label)

forest_int$term_label <- factor(forest_int$term_label, levels = ordem)
forest_int$custo      <- factor(forest_int$custo, levels = niveis)



# -----------------------------------------------------------------------
# 5. FIGURA — forest plot com um painel por custo
# -----------------------------------------------------------------------

fig_forest_int <- ggplot(forest_int,
                         aes(x = estimate, y = term_label,
                             color = significativo)) +
  
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "grey50", linewidth = 0.5) +
  
  # IC significativos
  geom_errorbar(
    data = forest_int %>% filter(significativo),
    aes(xmin = conf.low, xmax = conf.high),
    width = 0.25, linewidth = 0.7, orientation = "y"
  ) +
  
  # IC não significativos
  geom_errorbar(
    data = forest_int %>% filter(!significativo),
    aes(xmin = conf.low, xmax = conf.high),
    width = 0.25, linewidth = 0.7,
    linetype = "dashed", orientation = "y"
  ) +
  
  geom_point(size = 3) +
  
  geom_text(
    aes(x = max(forest_int$conf.high, na.rm = TRUE) + 0.08, 
        label = beta_label),
    hjust = 0, size = 2.8, color = "black"
  ) +
  
  facet_wrap(~ custo, ncol = 4) +
  
  scale_color_manual(
    values = c("TRUE" = "black", "FALSE" = "grey60"),
    guide  = "none"
  ) +
  
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.40))) +
  
 
  labs(
    x = "Standardized effect size (β)",
    y = NULL
  ) +
  
  theme_classic() +
  theme(
    strip.text         = element_text(face = "bold", size = 10),
    strip.background   = element_blank(),
    axis.text.y        = element_text(color = "black", size = 10),
    axis.text.x        = element_text(color = "black", size = 9),
    axis.title.x       = element_text(face = "bold", size = 11),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    panel.spacing      = unit(1, "lines")
  )

fig_forest_int

ggsave("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/fig_forest_structure_by_cost_SE_quantity.tiff",
       fig_forest_int, width = 14, height = 5, dpi = 300)
dev.off()

# -----------------------------------------------------------------------
# GERAR PREDIÇÕES DO MODELO 3 - conditional effects plot
# Para cada métrica, fixar nos 3 níveis (-1DP, média, +1DP)
# e variar o custo fisiológico no eixo X
# -----------------------------------------------------------------------

metricas_info <- list(
  list(term = "param_Cp_multiplier",
       cond = "NODF_z [-1, 0, 1]",
       label = "Nestedness (NODF)"),
  list(term = "param_Cp_multiplier",
       cond = "Q_z [-1, 0, 1]",
       label = "Modularity (Q)"),
  list(term = "param_Cp_multiplier",
       cond = "conect_z [-1, 0, 1]",
       label = "Connectance"),
  list(term = "param_Cp_multiplier",
       cond = "size_z [-1, 0, 1]",
       label = "Network size"),
  list(term = "param_Cp_multiplier",
       cond = "deg_core_z [-1, 0, 1]",
       label = "Mean degree — core"),
  list(term = "param_Cp_multiplier",
       cond = "deg_peri_z [-1, 0, 1]",
       label = "Mean degree — periphery")
)

# Extrair predições para cada métrica
df_cond <- lapply(metricas_info, function(m) {
  pred <- ggpredict(
    mod5,
    terms = c("custo_cat", m$cond),
    bias_correction = TRUE
  )
  data.frame(
    metrica   = m$label,
    x         = pred$x,
    predicted = pred$predicted,
    conf.low  = pred$conf.low,
    conf.high = pred$conf.high,
    group     = factor(pred$group,
                       levels = c("-1", "0", "1"),
                       labels = c("−1 SD", "Mean", "+1 SD"))
  )
}) %>% bind_rows()

# Ordenar painéis pelo ranking de importância (Modelo 3)
ordem_metricas <- c(
  "Mean degree — periphery",
  "Network size",
  "Mean degree — core",
  "Modularity (Q)",
  "Nestedness (NODF)",
  "Connectance"
)
df_cond$metrica <- factor(df_cond$metrica, levels = ordem_metricas)

# -----------------------------------------------------------------------
# FIGURA
# -----------------------------------------------------------------------

# Garantir que o X seja tratado como fator/categórico
df_cond$x <- as.factor(df_cond$x)

fig_cond <- ggplot() +
  # 1. Dados brutos ao fundo: coloridos por SISTEMA
  # Usamos a paleta Set2 para os sistemas terem cores bem distintas e alpha baixo para não soterrar o gráfico
  geom_jitter(data = estruc, 
              aes(x = custo_cat, y = persistence_species, color = sistema),
              alpha = 0.03, size = 0.5, position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6)) +
  
  # 2. Médias estimadas e intervalos de confiança: destacados na frente
  # Usamos o 'shape = group' para mudar o formato do ponto central para cada valor de SD (-1, Média, +1)
  # Usamos cores pretas/escuras fixas para que as estimativas fiquem nítidas sobre o fundo colorido
  geom_pointrange(data = df_cond, 
                  aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, 
                      shape = group, group = group),
                  color = "black", fill = "white", size = 0.6, linewidth = 0.8,
                  position = position_dodge(width = 0.6)) +
  
  # Um painel por métrica de rede
  facet_wrap(~ metrica, ncol = 3) +
  
  # Paletas e Legendas separadas para não dar conflito
  scale_color_brewer(palette = "Set2", name = "Mutualism System") +
  scale_shape_manual(values = c(24, 21, 25), name = "Network metric value") + # 24 = Triângulo cima, 21 = Círculo, 25 = Triângulo baixo
  
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  
  labs(
    x = "Physiological cost (Cp)",
    y = "Predicted species persistence"
  ) +
  
  # Forçar os pontos da legenda de sistemas a ficarem visíveis e sem transparência
  guides(
    color = guide_legend(override.aes = list(alpha = 1, size = 3)),
    shape = guide_legend(override.aes = list(size = 1, fill = "black"))
  ) +
  
  theme_classic() +
  theme(
    strip.text         = element_text(face = "bold", size = 10),
    strip.background   = element_blank(),
    axis.text          = element_text(color = "black", size = 9),
    axis.title         = element_text(face = "bold", size = 11),
    legend.position    = "bottom",
    legend.box         = "vertical", # Organiza as duas legendas uma embaixo da outra para não cortar
    panel.spacing      = unit(0.8, "lines"),
    aspect.ratio       = 1
  )

# Renderizar o gráfico na tela
fig_cond

ggsave("fig_conditional_effects.tiff",
       fig_cond, width = 10, height = 7, dpi = 300)
#8888888888888888888888888888888888888888888888888888888888888888888888888888888
#8888888888888888888888888888888888888888888888888888888888888888888888888888888
#8888888888888888888888888888888888888888888888888888888888888888888888888888888
#8888888888888888888888888888888888888888888888888888888888888888888888888888888

providers <- resultados %>%
  filter(param_nivel_BCe == "moderate") %>%
  mutate(
    NODF_z     = as.numeric(scale(NODF)),
    Q_z        = as.numeric(scale(Q)),
    conect_z   = as.numeric(scale(conectancia)),
    size_z     = as.numeric(scale(n_species_total)),
    deg_core_z = as.numeric(scale(mean_degree_core)),
    deg_peri_z = as.numeric(scale(mean_degree_periphery))
  )
    
providers$param_Cp_multiplier = as.factor(providers$param_Cp_multiplier)

#Criar a variável de proporção mantida
providers$services_retained <- providers$services_final / providers$services_initial

# Como a regressão beta não aceita exatamente 0 ou 1 cravados, 
# aplicamos uma transformação padrão (Smithson & Verkuilen, 2006)
n <- nrow(providers)
providers$services_retained_trans <- (providers$services_retained * (n - 1) + 0.5) / n

mod7 <- glmmTMB(
  services_retained_trans ~ service_providers * param_Cp_multiplier + 
    NODF_z + Q_z + conect_z + size_z + deg_core_z + deg_peri_z + 
    (1 | nome_rede),
  data   = providers,
  family = beta_family()
  )

null7 <- glmmTMB(
  services_retained_trans ~ param_Cp_multiplier + 
  NODF_z + Q_z + conect_z + size_z + deg_core_z + deg_peri_z + 
  (1 | nome_rede),
data   = providers,
family = beta_family()
)

anova(mod7, null7, test = "Chisq")
summary(mod7)
cat("=== MODELO 4: ESTRUTURA × CUSTO ===\n")

cat("\nR² — Modelo 4:\n")
print(r2(mod7))

vif_resultado <- check_collinearity(mod7)
print(vif_resultado)
# -----------------------------------------------------------------------
# 3. EXTRAIR COEFICIENTES POR NÍVEL DE CUSTO
#    Para o forest plot, queremos o efeito de cada métrica
#    em cada nível de custo separadamente
# -----------------------------------------------------------------------

library(glmmTMB)
library(tidyverse)
library(openxlsx)

# -----------------------------------------------------------------------
# FUNÇÃO LEVE PARA EXTRAIR EFEITOS DOS PROVEDORES (BOMBA DE RAM DEFUSED)
# -----------------------------------------------------------------------
extrair_coefs_providers_mod7 <- function(mod, niveis_custo) {
  s     <- summary(mod)$coefficients$cond
  v_cov <- vcov(mod)$cond 
  
  # Captura todas as categorias de provedores presentes no summary automaticamente
  todos_termos <- rownames(s)
  termos_providers <- todos_termos[grep("service_providers", todos_termos)]
  # Filtra para pegar apenas os efeitos principais (remove as interações com o ":" por enquanto)
  termos_providers_base <- unique(gsub(":.*", "", termos_providers))
  
  resultados <- lapply(niveis_custo, function(cp) {
    
    df_cp <- lapply(termos_providers_base, function(prov) {
      
      # Ajusta o nome do fator de custo de acordo com a sua fórmula (param_Cp_multiplier)
      # Se for Cp = 0, no R vira o intercepto/referência, então avaliamos o efeito base
      if (cp == "Cp = 0" | cp == "0") {
        if (prov %in% rownames(s)) {
          return(data.frame(
            term      = prov,
            estimate  = s[prov, "Estimate"],
            se        = s[prov, "Std. Error"],
            p.value   = s[prov, "Pr(>|z|)"],
            custo     = "Cp = 0"
          ))
        }
      } else {
        # Nos outros custos, procuramos a interação exata: Base + Interação
        # Removemos o prefixo "Cp = " para bater com o número do multiplicador se necessário
        val_num <- gsub("Cp = ", "", cp)
        sufixo_custo <- paste0("param_Cp_multiplier", val_num)
        
        term_int <- paste0(prov, ":", sufixo_custo)
        
        # Caso o R tenha invertido a ordem na matriz de coeficientes
        if (!term_int %in% rownames(s)) {
          term_int <- paste0(sufixo_custo, ":", prov)
        }
        
        if (term_int %in% rownames(s)) {
          est_base <- s[prov, "Estimate"]
          est_int  <- s[term_int, "Estimate"]
          
          # Variância Combinada: Var(A+B) = Var(A) + Var(B) + 2*Cov(A,B)
          var_combinada <- v_cov[prov, prov] + v_cov[term_int, term_int] + 2 * v_cov[prov, term_int]
          se_combinado  <- sqrt(max(0, var_combinada))
          est_combinado <- est_base + est_int
          
          z_stat        <- est_combinado / se_combinado
          pv_combinado  <- 2 * (1 - pnorm(abs(z_stat)))
          
          return(data.frame(
            term      = prov,
            estimate  = est_combinado,
            se        = se_combinado,
            p.value   = pv_combinado,
            custo     = paste0("Cp = ", val_num)
          ))
        }
      }
      return(NULL)
    }) %>% bind_rows()
    
    return(df_cp)
  }) %>% bind_rows()
  
  # Formatação cosmética final para o GGPLOT
  resultados %>%
    mutate(
      conf.low      = estimate - (1.96 * se),
      conf.high     = estimate + (1.96 * se),
      sig           = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**", p.value < 0.05 ~ "*", TRUE ~ ""),
      significativo = p.value < 0.05,
      # Embeleza o nome removendo o prefixo do R
      term_label    = gsub("service_providers", "Provider: ", term),
      beta_label    = paste0("β=", sprintf("%.2f", estimate), sig)
    )
}

# -----------------------------------------------------------------------
# EXECUÇÃO DO FLUXO
# -----------------------------------------------------------------------
niveis <- c("Cp = 0", "Cp = 0.1", "Cp = 0.3", "Cp = 0.5")

# Extração rápida e segura
forest_int <- extrair_coefs_providers_mod7(mod7, niveis)

# Salvar planilha estruturada
write.xlsx(forest_int, "C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/ranking_providers_VS_cost.xlsx")

# Ordenar os fatores para o GGPLOT ficar perfeito
ordem_providers <- forest_int %>%
  group_by(term_label) %>%
  summarise(med = mean(estimate, na.rm = TRUE)) %>%
  arrange(med) %>%
  pull(term_label)

forest_int$term_label <- factor(forest_int$term_label, levels = ordem_providers)
forest_int$custo      <- factor(forest_int$custo, levels = niveis)



# -----------------------------------------------------------------------
# 5. FIGURA — forest plot com um painel por custo
# -----------------------------------------------------------------------

fig_forest_int <- ggplot(forest_int,
                         aes(x = estimate, y = term_label,
                             color = significativo)) +
  
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "grey50", linewidth = 0.5) +
  
  # IC significativos
  geom_errorbar(
    data = forest_int %>% filter(significativo),
    aes(xmin = conf.low, xmax = conf.high),
    width = 0.25, linewidth = 0.7, orientation = "y"
  ) +
  
  # IC não significativos
  geom_errorbar(
    data = forest_int %>% filter(!significativo),
    aes(xmin = conf.low, xmax = conf.high),
    width = 0.25, linewidth = 0.7,
    linetype = "dashed", orientation = "y"
  ) +
  
  geom_point(size = 3) +
  
  geom_text(
    aes(x = max(forest_int$conf.high, na.rm = TRUE) + 0.5, 
        label = beta_label),
    hjust = 0, size = 2.8, color = "black"
  ) +
  
  facet_wrap(~ custo, ncol = 4) +
  
  scale_color_manual(
    values = c("TRUE" = "black", "FALSE" = "grey60"),
    guide  = "none"
  ) +
  
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.40))) +
  
  
  labs(
    x = "Standardized effect size (β)",
    y = NULL
  ) +
  
  theme_classic() +
  theme(
    strip.text         = element_text(face = "bold", size = 10),
    strip.background   = element_blank(),
    axis.text.y        = element_text(color = "black", size = 10),
    axis.text.x        = element_text(color = "black", size = 9),
    axis.title.x       = element_text(face = "bold", size = 11),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    panel.spacing      = unit(1, "lines")
  )

fig_forest_int

ggsave("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/fig_forest_providers_costs_SE_quantity.tiff",
       fig_forest_int, width = 14, height = 5, dpi = 300)

################################################################################
# Figura predição marginal

## 1. Pegar os níveis originais exatos direto dos fatores usados no modelo
niveis_provedores <- levels(mod7$frame$service_providers)
niveis_custo      <- levels(mod7$frame$param_Cp_multiplier)

# Caso eles não sejam fatores no modelo original, pegamos os valores únicos:
if(is.null(niveis_provedores)) niveis_provedores <- unique(mod7$frame$service_providers)
if(is.null(niveis_custo))      niveis_custo      <- unique(mod7$frame$param_Cp_multiplier)

# 2. Montar o grid usando as variáveis idênticas
cenario_pred <- expand.grid(
  service_providers   = niveis_provedores,
  param_Cp_multiplier = niveis_custo,
  NODF_z = 0, Q_z = 0, conect_z = 0, size_z = 0, deg_core_z = 0, deg_peri_z = 0,
  stringsAsFactors    = FALSE
)

# Adiciona a coluna do efeito aleatório exigida
cenario_pred$nome_rede <- NA

# 3. Rodar a predição blindada
cenario_pred$predicted <- predict(
  mod7, 
  newdata = cenario_pred, 
  type    = "response",
  re.form = NA
)

# 3. Plotar o gráfico de linhas de interação
fig_linhas_servicos <- ggplot(cenario_pred, 
                              aes(x = param_Cp_multiplier, y = predicted, 
                                  color = service_providers, group = service_providers)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3.5) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0.05)) +
  scale_color_brewer(palette = "Set1") + # Cores bem distintas para os provedores
  labs(
    x = "Physiological cost",
    y = "Proportion of the Volume of ESs Maintained",
    color = "Service Provider"
  ) +
  theme_classic() +
  theme(
    axis.text    = element_text(color = "black", size = 10),
    axis.title   = element_text(face = "bold", size = 11),
    legend.title = element_text(face = "bold"),
    legend.position = "right"
  )

print(fig_linhas_servicos)
ggsave(
  "C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/fig_lines_providers_by_cost.tiff",
  plot   = fig_linhas_servicos, 
  width  = 7,       # Proporção perfeita para ocupar uma coluna ou meia página
  height = 5,       # Altura ideal para dar espaço à legenda e aos eixos
  dpi    = 300,     # Resolução padrão exigida pelas revistas
  compression = "lzw" # Opcional: reduz o peso do arquivo .tiff sem perder qualidade
)


################################################################################
################################################################################
################################################################################
################################################################################
library(dplyr)

tabela_descritiva <- providers %>%
  group_by(service_providers, param_Cp_multiplier) %>%
  summarise(
    "mean initial ES richness"  = mean(n_services, na.rm = TRUE),
    "sd initial ES richness"    = sd(n_services, na.rm = TRUE),
    "mean final ES richness"  = mean(riqueza_servicos_final, na.rm = TRUE),
    "sd final ES richness"    = sd(riqueza_servicos_final, na.rm = TRUE),
    
    "mean  relative loss of ES"  = mean(services_loss_relative, na.rm = TRUE),
    "sd relative loss of ES"    = sd(services_loss_relative, na.rm = TRUE),
    
    "mean initial ES volume"  = mean(services_initial, na.rm = TRUE),
    "sd initial ES volume"    = sd(services_initial, na.rm = TRUE),
    "mean final ES volume"  = mean(services_final, na.rm = TRUE),
    "sd final ES volume"    = sd(services_final, na.rm = TRUE),
    "N networks"      = n(),
    .groups      = "drop"
  )

print(tabela_descritiva)

str(providers)
mod8 <- glmmTMB(
  cbind(riqueza_servicos_final, n_services - riqueza_servicos_final ) ~ service_providers * param_Cp_multiplier + 
    NODF_z + Q_z + conect_z + size_z + deg_core_z + deg_peri_z + 
    (1 | nome_rede),
  data   = providers,
  family = binomial
)

null8 <- glmmTMB(
  cbind(riqueza_servicos_final, n_services - riqueza_servicos_final ) ~ param_Cp_multiplier + 
  NODF_z + Q_z + conect_z + size_z + deg_core_z + deg_peri_z + 
  (1 | nome_rede),
data   = providers,
family = binomial
)

anova(mod8, null8, test = "Chisq")
summary(mod8)
cat("=== MODELO 4: ESTRUTURA × CUSTO ===\n")

cat("\nR² — Modelo 4:\n")
print(r2(mod8))

vif_resultado <- check_collinearity(mod8)
print(vif_resultado)
# -----------------------------------------------------------------------
# 3. EXTRAIR COEFICIENTES POR NÍVEL DE CUSTO
#    Para o forest plot, queremos o efeito de cada métrica
#    em cada nível de custo separadamente
# -----------------------------------------------------------------------

library(glmmTMB)
library(tidyverse)
library(openxlsx)

# -----------------------------------------------------------------------
# FUNÇÃO LEVE PARA EXTRAIR EFEITOS DOS PROVEDORES (BOMBA DE RAM DEFUSED)
# -----------------------------------------------------------------------
extrair_coefs_providers_mod8 <- function(mod, niveis_custo) {
  s     <- summary(mod)$coefficients$cond
  v_cov <- vcov(mod)$cond 
  
  # Captura todas as categorias de provedores presentes no summary automaticamente
  todos_termos <- rownames(s)
  termos_providers <- todos_termos[grep("service_providers", todos_termos)]
  # Filtra para pegar apenas os efeitos principais (remove as interações com o ":" por enquanto)
  termos_providers_base <- unique(gsub(":.*", "", termos_providers))
  
  resultados <- lapply(niveis_custo, function(cp) {
    
    df_cp <- lapply(termos_providers_base, function(prov) {
      
      # Ajusta o nome do fator de custo de acordo com a sua fórmula (param_Cp_multiplier)
      # Se for Cp = 0, no R vira o intercepto/referência, então avaliamos o efeito base
      if (cp == "Cp = 0" | cp == "0") {
        if (prov %in% rownames(s)) {
          return(data.frame(
            term      = prov,
            estimate  = s[prov, "Estimate"],
            se        = s[prov, "Std. Error"],
            p.value   = s[prov, "Pr(>|z|)"],
            custo     = "Cp = 0"
          ))
        }
      } else {
        # Nos outros custos, procuramos a interação exata: Base + Interação
        # Removemos o prefixo "Cp = " para bater com o número do multiplicador se necessário
        val_num <- gsub("Cp = ", "", cp)
        sufixo_custo <- paste0("param_Cp_multiplier", val_num)
        
        term_int <- paste0(prov, ":", sufixo_custo)
        
        # Caso o R tenha invertido a ordem na matriz de coeficientes
        if (!term_int %in% rownames(s)) {
          term_int <- paste0(sufixo_custo, ":", prov)
        }
        
        if (term_int %in% rownames(s)) {
          est_base <- s[prov, "Estimate"]
          est_int  <- s[term_int, "Estimate"]
          
          # Variância Combinada: Var(A+B) = Var(A) + Var(B) + 2*Cov(A,B)
          var_combinada <- v_cov[prov, prov] + v_cov[term_int, term_int] + 2 * v_cov[prov, term_int]
          se_combinado  <- sqrt(max(0, var_combinada))
          est_combinado <- est_base + est_int
          
          z_stat        <- est_combinado / se_combinado
          pv_combinado  <- 2 * (1 - pnorm(abs(z_stat)))
          
          return(data.frame(
            term      = prov,
            estimate  = est_combinado,
            se        = se_combinado,
            p.value   = pv_combinado,
            custo     = paste0("Cp = ", val_num)
          ))
        }
      }
      return(NULL)
    }) %>% bind_rows()
    
    return(df_cp)
  }) %>% bind_rows()
  
  # Formatação cosmética final para o GGPLOT
  resultados %>%
    mutate(
      conf.low      = estimate - (1.96 * se),
      conf.high     = estimate + (1.96 * se),
      sig           = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**", p.value < 0.05 ~ "*", TRUE ~ ""),
      significativo = p.value < 0.05,
      # Embeleza o nome removendo o prefixo do R
      term_label    = gsub("service_providers", "Provider: ", term),
      beta_label    = paste0("β=", sprintf("%.2f", estimate), sig)
    )
}

# -----------------------------------------------------------------------
# EXECUÇÃO DO FLUXO
# -----------------------------------------------------------------------
niveis <- c("Cp = 0", "Cp = 0.1", "Cp = 0.3", "Cp = 0.5")

# Extração rápida e segura
forest_int <- extrair_coefs_providers_mod7(mod8, niveis)

# Salvar planilha estruturada
write.xlsx(forest_int, "C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/ranking_providers_VS_cost_RIQUEZA.xlsx")

# Ordenar os fatores para o GGPLOT ficar perfeito
ordem_providers <- forest_int %>%
  group_by(term_label) %>%
  summarise(med = mean(estimate, na.rm = TRUE)) %>%
  arrange(med) %>%
  pull(term_label)

forest_int$term_label <- factor(forest_int$term_label, levels = ordem_providers)
forest_int$custo      <- factor(forest_int$custo, levels = niveis)



# -----------------------------------------------------------------------
# 5. FIGURA — forest plot com um painel por custo
# -----------------------------------------------------------------------

fig_forest_int <- ggplot(forest_int,
                         aes(x = estimate, y = term_label,
                             color = significativo)) +
  
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "grey50", linewidth = 0.5) +
  
  # IC significativos
  geom_errorbar(
    data = forest_int %>% filter(significativo),
    aes(xmin = conf.low, xmax = conf.high),
    width = 0.25, linewidth = 0.7, orientation = "y"
  ) +
  
  # IC não significativos
  geom_errorbar(
    data = forest_int %>% filter(!significativo),
    aes(xmin = conf.low, xmax = conf.high),
    width = 0.25, linewidth = 0.7,
    linetype = "dashed", orientation = "y"
  ) +
  
  geom_point(size = 3) +
  
  geom_text(
    aes(x = max(forest_int$conf.high, na.rm = TRUE) + 0.5, 
        label = beta_label),
    hjust = 0, size = 2.8, color = "black"
  ) +
  
  facet_wrap(~ custo, ncol = 4) +
  
  scale_color_manual(
    values = c("TRUE" = "black", "FALSE" = "grey60"),
    guide  = "none"
  ) +
  
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.40))) +
  
  
  labs(
    x = "Standardized effect size (β)",
    y = NULL
  ) +
  
  theme_classic() +
  theme(
    strip.text         = element_text(face = "bold", size = 10),
    strip.background   = element_blank(),
    axis.text.y        = element_text(color = "black", size = 10),
    axis.text.x        = element_text(color = "black", size = 9),
    axis.title.x       = element_text(face = "bold", size = 11),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    panel.spacing      = unit(1, "lines")
  )

fig_forest_int

ggsave("C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/fig_forest_providers_costs_SE_riqueza.tiff",
       fig_forest_int, width = 14, height = 5, dpi = 300)

################################################################################
# Figura predição marginal

## 1. Pegar os níveis originais exatos direto dos fatores usados no modelo
niveis_provedores <- levels(mod8$frame$service_providers)
niveis_custo      <- levels(mod8$frame$param_Cp_multiplier)

# Caso eles não sejam fatores no modelo original, pegamos os valores únicos:
if(is.null(niveis_provedores)) niveis_provedores <- unique(mod8$frame$service_providers)
if(is.null(niveis_custo))      niveis_custo      <- unique(mod8$frame$param_Cp_multiplier)

# 2. Montar o grid usando as variáveis idênticas
cenario_pred <- expand.grid(
  service_providers   = niveis_provedores,
  param_Cp_multiplier = niveis_custo,
  NODF_z = 0, Q_z = 0, conect_z = 0, size_z = 0, deg_core_z = 0, deg_peri_z = 0,
  stringsAsFactors    = FALSE
)

# Adiciona a coluna do efeito aleatório exigida
cenario_pred$nome_rede <- NA

# 3. Rodar a predição blindada
cenario_pred$predicted <- predict(
  mod8, 
  newdata = cenario_pred, 
  type    = "response",
  re.form = NA
)

# 3. Plotar o gráfico de linhas de interação
fig_linhas_servicos <- ggplot(cenario_pred, 
                              aes(x = param_Cp_multiplier, y = predicted, 
                                  color = service_providers, 
                                  group = service_providers)) + # 🛠️ Crucial para separar as linhas
  geom_line(linewidth = 1.2) +
  geom_point(size = 3.5) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0.05)) +
  scale_color_brewer(palette = "Set1") + 
  labs(
    x = "Physiological cost",
    y = "Proportion of ESs Richness Maintained",
    color = "Service Provider"
  ) +
  theme_classic() +
  theme(
    axis.text    = element_text(color = "black", size = 10),
    axis.title   = element_text(face = "bold", size = 11),
    legend.title = element_text(face = "bold"),
    legend.position = "right"
  )

print(fig_linhas_servicos)

print(fig_linhas_servicos)
ggsave(
  "C:/Users/bruno/OneDrive/Documentos/GitHub/BoneX/Spark/Data/Simulated/Bruno/model_run/results/fig_lines_providers_by_cost_RIQUEZA.tiff",
  plot   = fig_linhas_servicos, 
  width  = 7,       # Proporção perfeita para ocupar uma coluna ou meia página
  height = 5,       # Altura ideal para dar espaço à legenda e aos eixos
  dpi    = 300,     # Resolução padrão exigida pelas revistas
  compression = "lzw" # Opcional: reduz o peso do arquivo .tiff sem perder qualidade
)

