## Degree distribution of surviving and extinct species
rm(list=ls())
library(tidyverse)
library(paletteer) # for color palettes
library(Cairo)
#____________________________________________________________#
# Is degree distribution related to probability of extinction?
#____________________________________________________________#

# Code to run random Boolean networks analysis
# load the functions
source("functions.R")
source("functions_betaD.R")

# Number of Species of each set
nspi <- 60
nspj <- 75
# Number of simulations
nsim <- 100
# Expected connectance
connect <- .65

## Combination of parameters for the beta distribution
alphaC <- c(5, 3, 0.2, 5, 3.5, 0.25)
betaC <- c(3, 5, 1, 3.5, 5, 1)
alphaB <- c(5, 3.5, 0.25, 5, 3, 0.2)
betaB <- c(3.5, 5, 1, 3, 5, 1)

## Object to store results
res_degrees <- tibble()
final_res <- tibble()
res <- tibble()

for(k in 1:length(alphaC)){
  pars_combination <- paste0("C~(\u03b1=", alphaC[k], ", \u03b2=", betaC[k], ") ",
                             " B~(\u03b1=", alphaB[k], ", \u03b2=", betaB[k], ")")
  
  for (i in 1:nsim) {
    model_output <- boolean_model(nspi, nspj, connect, 
                                  shape1C = alphaC[k], shape2C = betaC[k], 
                                  shape1B = alphaB[k], shape2B = betaB[k],
                                  shape1Cp = 1, shape2Cp = 1)
    
    tmp_degree <- tibble(species = names(rowSums(model_output$A)),
                         degree=rowSums(model_output$A), 
                         presence= ifelse(t(tail(model_output$community, 1)) == 1, "present", "extinct")[,1],
                         iteration = i,
                         pars_beta = pars_combination)
    res_degrees <- rbind(res_degrees, tmp_degree)
    
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
res_degrees <- res_degrees %>% mutate(pars_beta = factor(pars_beta, levels = unique(pars_beta)))

## Is there a relationship between initial degree and "probability of extinction"
# It doesn't seem so
# For probability of extinction we are using a proxy that is number of time steps 
# a species survived
pl_degrees <- ggplot(res_degrees, aes(x = degree, group = presence, alpha = presence,
                         fill = pars_beta, color = pars_beta)) + 
  geom_histogram(position = "dodge", bins = 18) +
  facet_wrap(~pars_beta) +
  theme_bw() +
  scale_colour_paletteer_d("NatParksPalettes::IguazuFalls") +
  scale_fill_paletteer_d("NatParksPalettes::IguazuFalls") +
  scale_alpha_manual(values = c("extinct" = 0.4, "present" = 0.9)) +
  guides(fill = "none", color = "none") + 
  theme(legend.position = "bottom", axis.text.x = element_text(size = 5),
        axis.title = element_text(size=7), strip.text = element_text(size=5),
        legend.text = element_text(size = 7))

pl_degrees

ggsave("./figures/degree_distributions.pdf", pl_degrees, width = 5, height = 4, device = cairo_pdf)
