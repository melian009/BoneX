library(tidyverse)
library(ggplot2)
#Tracking alpha and physiological costs
#----------------------------------------
#extract environmental fluctuation (theta) 
environ <- data.frame(theta)
environ <- environ %>%
  mutate(t = 1:nrow(.))

#------------------------------------------
# Calculate alpha and physiological costs
# call the functions to extract both
alpha_vals <- alpha_fun(theta, zi)
cf_vals <- Cf(alpha_vals, Cp_vec)

#------------------------------------------
# change to a long format to visualise in ggplt
alpha_df <- as.data.frame(alpha_vals) %>%
  mutate(t = 1:nrow(.)) %>%
  pivot_longer(cols = starts_with("sp"), names_to = "especie", values_to = "alpha")

cf_df <- as.data.frame(cf_vals) %>%
  mutate(t = 1:nrow(.)) %>%
  pivot_longer(cols = starts_with("sp"), names_to = "especie", values_to = "cf")

#--------------------------------------------
#plots
# Plot do alpha
p_alpha <- ggplot(alpha_df, aes(x = t, y = alpha, color = especie)) +
  geom_line(linewidth = 1) +
  theme_classic() +
  labs(title = expression(alpha ~ "(theta - z[i])"),
       x = "Time (t)", y = expression(alpha)) +
  theme(legend.position = "none")

# Plot do custo fisiológico
p_cf <- ggplot(cf_df, aes(x = t, y = cf, color = especie)) +
  geom_line(linewidth = 1) +
  theme_classic() +
  labs(title = bquote("Custo fisiológico Cf (" *alpha* "Cp) ~" * beta * "(0.5,0.5)*0.2"),
       x = "Time (t)", y = "Cf") +
  theme(legend.position = "none")

envir <- ggplot(data = environ, aes(y = theta, x = t)) +
  geom_line(linewidth = 1)+
  theme_classic() +
  labs(title = expression(paste(theta, ,"(A_min = 1, A_max = 10, w_min = 1, w_max = 5, t_max = 100)")),
       x = "Time (t)", y = expression(theta)) +
  theme(legend.position = "none")

#---------------------------------------
library(patchwork)
envir / p_alpha / p_cf

#---------------------------------------

#track results

#environmental services
serv_long <- as.data.frame(services_dinamics$services_history) %>%
  mutate(t = 1:nrow(.)) %>%
  pivot_longer(cols = starts_with("service"), names_to = "service", values_to = "amount")
# arrange in one figure
summary(serv_long)
es <- ggplot(data = serv_long, aes(y = amount, x = t, col = service))+
  geom_line(linewidth = 1)+
  theme_classic()+
  labs(title = "ES dynamics (A is modular, Es is nested", 
       x = "Time (t)", y = "Ecosystem service Value")

es


states_long = data.frame(
  t = 1:nrow(resultado$state_history),
  resultado$state_history, 
  prop_interactions = resultado$prop_interactions_history, 
  prop_species = resultado$prop_species_history
) %>%
  pivot_longer(
    cols = starts_with("sp"),
    names_to = "species",
    values_to = "state"
  )

# Ver resultado
head(df_long)

state = ggplot(states_long, aes(y = state, x = t, colour = species))+
  geom_jitter()+
  theme_classic()+
  labs(title = bquote("Species states  (B and C ~" * beta * "(0.5,0.5)"),
       x = "Time (t)", y = "Species state")
state


species = ggplot(states_long)+
  geom_line(aes(y = prop_species, x = t), linewidth = 1, linetype = 1)+
  geom_line(aes(y = prop_interactions, x = t), linewidth = 1, linetype = 2)+
  theme_classic()+
  annotate("segment", x = max(states_long$t) * 0.9, xend = max(states_long$t), 
           y = 0.8, yend = 0.8, linewidth = 1, linetype = 1) +
  annotate("text", x = max(states_long$t) * 0.9, y = 0.82, 
           label = "Species", hjust = 0, size = 4) +
  annotate("segment", x = max(states_long$t) * 0.9, xend = max(states_long$t), 
           y = 0.7, yend = 0.7, linewidth = 1, linetype = 2) +
  annotate("text", x = max(states_long$t) * 0.9, y = 0.72, 
           label = "Interactions", hjust = 0, size = 4) 
  labs(title = "Proportion")
species

interactions = ggplot(states_long, aes(y = prop_interactions, x = t, colour = species))+
  geom_line(linewidth = 1)+
  theme_classic()+
  labs(title = bquote("Species states  (B and C ~" * beta * "(0.5,0.5)"))
interactions
