library(tidyverse)
#Tracking alpha and physiological costs
#----------------------------------------
#extract environmental fluctuation (theta) 
environ <- data.frame(theta)
environ <- env %>%
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
  geom_line(size = 1) +
  theme_classic() +
  labs(title = "Custo fisiológico Cf",
       x = "Time (t)", y = "Cf") +
  theme(legend.position = "none")

envir <- ggplot(data = environ, aes(y = theta, x = t)) +
  geom_line(linewidth = 1)+
  theme_classic() +
  labs(title = expression(theta),
       x = "Time (t)", y = expression(theta)) +
  theme(legend.position = "none")


# arrange in one figure
library(patchwork)
env /p_alpha / p_cf


