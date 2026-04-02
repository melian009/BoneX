## Exploring the effect of connectance on the proportion of surviving species
# In this set of simulations we will use Beta distribution to model 
  # costs and benefits 
library(tidyverse)
library(paletteer) # for color palettes
library(Cairo)
rm(list=ls())
# Code to run random Boolean networks analysis
# load the functions
source("functions.R")
source("functions_betaD.R")

# Number of Species of each set
nspi <- 50
nspj <- 65
# Number of simulations
nsim <- 100

## Combination of parameters for the beta distribution
alphaC <- c(5, 3, 0.2, 5, 3.5, 0.25)
betaC <- c(3, 5, 1, 3.5, 5, 1)
alphaB <- c(5, 3.5, 0.25, 5, 3, 0.2)
betaB <- c(3.5, 5, 1, 3, 5, 1)

# Expected connectance -- this is the parameter we will vary
setup_connect <- seq(0.1, 1, by = 0.05)

# Create the objects that will store the results

final_res <- tibble()
# Choose values for each distribution
for(k in 1:length(alphaC)){
  pars_combination <- paste0("C~(\u03b1=", alphaC[k], ", \u03b2=", betaC[k], ") ",
                             " B~(\u03b1=", alphaB[k], ", \u03b2=", betaB[k], ")")
  
  for(j in 1:length(setup_connect)){
    connect <- setup_connect[j]
    cat(c("connectance = ", setup_connect[j]))
    res <- tibble()
    for (i in 1:nsim) {
      model_output <- boolean_model(nspi, nspj, connect, 
                                    shape1C = alphaC[k], shape2C = betaC[k], 
                                    shape1B = alphaB[k], shape2B = betaB[k],
                                    shape1Cp = 1, shape2Cp = 1)
      
      res_timesp <- tibble(time_steps=1:nrow(model_output$community), 
                           sp_persistent=apply(model_output$community, 1, sum), 
                           prop_sp=sp_persistent/ncol(model_output$community),
                           iteration = i,
                           connect = connect)
      
      res <- rbind(res,res_timesp)
      
      print(i)
    }
    tmp_final <- res %>% group_by(connect) %>% 
      summarise(mean_time = mean(time_steps), sd_time = sd(time_steps),
                mean_sp = mean(sp_persistent), sd_sp = sd(sp_persistent),
                mean_prop = mean(prop_sp), sd_prop = sd(prop_sp),
                pars_beta = pars_combination)
    
    final_res <-  rbind(final_res, tmp_final)
    
  }
}
# Redordering the pars
final_res <- final_res %>% mutate(pars_beta = factor(pars_beta, levels = unique(pars_beta)))

## Mean time doesn't change that much
pl_time <- ggplot(final_res, aes(x = connect, y = mean_time, color = pars_beta)) + 
  geom_point(size=2) +
  geom_errorbar(aes(ymin=mean_time-sd_time, ymax = mean_time+sd_time), 
                width=0.02) +
  labs(x="Connectance", y="Mean time") + theme_bw() +
  facet_wrap(~pars_beta) +
  #scale_colour_paletteer_d("NatParksPalettes::Cuyahoga") +
  scale_colour_paletteer_d("NatParksPalettes::IguazuFalls") +
  #scale_colour_paletteer_d("MoMAColors::Dali") +
  theme(legend.position = "none")

pl_time

#Mean number of species
pl_sp <- ggplot(final_res, aes(x = connect, y = mean_sp, color = pars_beta)) + 
  geom_point(size=2) +
  geom_errorbar(aes(ymin=mean_sp-sd_sp, ymax = mean_sp+sd_sp), width=0.02) +
  labs(x = "Connectance", y="Mean surviving species") +
  theme_bw() +
  facet_wrap(~pars_beta) +
  scale_colour_paletteer_d("NatParksPalettes::IguazuFalls") +
  #scale_colour_paletteer_d("MoMAColors::Dali") +
  theme(legend.position = "none")

pl_sp

#Mean number of species
pl_prop <- ggplot(final_res, aes(x = connect, y = mean_prop, color = pars_beta)) + 
  geom_point(size=2) +
  geom_errorbar(aes(ymin=mean_prop-sd_prop, ymax = mean_prop+sd_prop), width=0.02) +
  labs(x = "Connectance", y="Mean proportion of surviving species") +
  theme_bw() +
  facet_wrap(~pars_beta) +
  scale_colour_paletteer_d("NatParksPalettes::IguazuFalls") +
  #scale_colour_paletteer_d("MoMAColors::Dali") +
  theme(legend.position = "none")

pl_prop

ggsave("./figures/mean_time.pdf", pl_time, width = 6, height = 4, device = cairo_pdf)

ggsave("./figures/mean_spp.pdf", pl_sp, width = 6, height = 4, device = cairo_pdf)

ggsave("./figures/mean_prop_spp.pdf", pl_prop, width = 6, height = 4, device = cairo_pdf)

## Ploting the shape of the functions
beta_data <- tibble()
for(h in 1:length(alphaC)){
  x_values <- seq(0,1, length.out = 100)
  y_valuesC <- dbeta(x_values, shape1 = alphaC[h], shape2 = betaC[h])
  y_valuesB <- dbeta(x_values, shape1 = alphaB[h], shape2 = betaB[h])
  y_values <- c(y_valuesC, y_valuesB)
  var_type <- c(rep("Costs", 100), rep("Benefits", 100))
  tmp <- tibble(x = rep(x_values,2), y = y_values, type = var_type, setup = letters[h])
  beta_data <- rbind(beta_data, tmp)  
}

pl_dist <- ggplot(beta_data, aes(x = x, y = y, linetype = type, colour = setup)) +
  geom_line() +
  facet_wrap(~setup) +
  scale_colour_paletteer_d("NatParksPalettes::IguazuFalls") +
  theme_bw() +
  theme(legend.position = "bottom", axis.text.x = element_text(size = 5)) +
  guides(color = "none")

pl_dist

ggsave("./figures/mean_distributions.pdf", pl_dist, width = 5, height = 4, device = cairo_pdf)
