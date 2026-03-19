## Difference/distance between two beta distributions using KL divergence
rm(list = ls())
library(tidyverse)
library(reshape2)
library(RColorBrewer)
library(cowplot)
source("functions_lognorm.R")

## Simulations to estimate how the difference/distance between costs and benefits
# influences the persistence of species in a community
# We will use two metrics for estimating the divergence between the distributions
# 1. KL divergence (always positive) is a stat. measure that quantifies the 
# difference between two probability distributions
# 2. Difference between the mean of each distribution (first moment)
# E[x] = exp(mu + (sigma^2)/2)

# Case studies 
## 1. Parameter mu between 0 < mu < 1
## 2. sd = 1
## Parameter combination
mus <- expand_grid(mu1 = seq(0, 1, by = 0.05), 
                   mu2 = seq(0, 1, by = 0.05)) 

# Adding the estimated divergence/difference
setup <- mus %>% mutate(kl = kl_lognorm(mu2, 1, mu1, 1),  # KL divergence
                           diff = diff_mean_lognorm(mu2, 1, mu1, 1))  # Mean difference


# how is divergence and difference related?
ggplot(setup %>% mutate(bigger=ifelse(mu1>mu2, "mu1", "mu2")), 
       aes(x = diff, y = kl, color = bigger)) + geom_point()

ggplot(setup %>% mutate(MU1=ifelse(mu1>mu2, "mu1", "mu2")), 
       aes(x = mu1, y = mu2, fill= diff)) +
  geom_tile() +
  #facet_wrap(~fct_rev(A1)) +
  scale_fill_viridis_c()

setup %>% mutate(M1=ifelse(mu1>mu2, "mu1", "mu2")) %>% 
  split(.$M1) %>%
  map(~ ggplot(., aes(x = mu1, y = mu2, fill = kl)) +
        geom_tile() +
        facet_wrap(~fct_rev(M1)) +
        scale_fill_viridis_c() +
        theme_linedraw()) %>%
  cowplot::plot_grid(plotlist = .)


## Ploting the shape of the functions
ggplot() +
  stat_function(fun = dlnorm, args = list(meanlog = 1, sdlog = 0.05), aes(color = "Cp"), lwd=1.5) +
  stat_function(fun = dlnorm, args = list(meanlog = 0.05, sdlog = 1), aes(color = "Cost"), lwd=2.5) +
  stat_function(fun = dlnorm, args = list(meanlog = 1, sdlog= 1), aes(color = "Benefit"), lwd=1.5) +
  scale_color_manual("Curve", values = c("Benefit" = "orchid4", "Cost" = "goldenrod", "Cp" = "gray")) +
  theme_minimal()


## Simulations
# Number of Species of each set
nspi <- 50
nspj <- 65
# Expected connectance
connect <- .65
# Number of simulations
nsim <- 10

# Create tibbles to store the results
res <- tibble()
ratio <- tibble()
degrees <- tibble()
# Choose values for each distribution
for(j in 1:nrow(setup)){
  kl_j <- pull(setup[j,"kl"])
  diff_j <- pull(setup[j,"diff"])
  for (i in 1:nsim) {
    model_res <- boolean_model(nspi, nspj, connect, 
                               muC = pull(setup[j,1]), sigmaC = 1, 
                               muB = pull(setup[j,2]), sigmaB = 1,
                               muCp = 1, sigmaCp = 0.05)
    
    toplot <-  tibble(time_steps=1:nrow(model_res$community), 
                      sp_persistent=apply(model_res$community, 1, sum), 
                      prop_sp=sp_persistent/ncol(model_res$community),
                      iteration = i,
                      setup = j,
                      kl = kl_j,
                      diff_distr = diff_j)
    res <- rbind(res,toplot)
    tmp <- estimate_CB_overtime(model_res) %>% add_column(iteration = i)
    ratio <- rbind(ratio, tmp)
    tmp_degree <- tibble(species = names(rowSums(model_res$A)),
                         degree=rowSums(model_res$A), 
                         presence= ifelse(t(tail(model_res$community, 1)) == 1, "present", "extinct"),
                         iteration = i,
                         setup = j)
    degrees <- rbind(degrees, tmp_degree)
  }
  print(j)
}


## Quick graph
p_main <- res %>% group_by(setup, iteration) %>% 
  filter(time_steps == max(time_steps)) %>% 
  ggplot(., aes(x = diff_distr, y=prop_sp, group = setup)) + geom_point(alpha = 0.5) +
  theme_bw() + labs(x = "E[Benefits] - E[Costs]", y = "Proportion of surviving species")


## Inset showing the beta distribution with parameter b=1
# 1. Create a sequence of x values (0 to 1 for lognorm distribution)
x_values <- seq(0, 10, length = 1001)

# 2. Calculate beta densities for different parameters
lognorm_dist_df <- tibble(
  x = x_values,
  "mu=0, sigma=1" = dlnorm(x_values, 0, 1),
  "mu=0.2, sigma=1" = dlnorm(x_values, 0.2, 1),
  "mu=0.5, sigma=1" = dlnorm(x_values, 0.5, 1),
  "mu=0.75, sigma=1" = dlnorm(x_values, 0.75, 1),
  "mu=1, sigma=1" = dlnorm(x_values, 1, 1)
)

# 3. Melt the data frame into a long format
lognorm_dist_long <- melt(lognorm_dist_df, id.vars = "x", variable.name = "Parameters", value.name = "Density")
# Create own color palette
my_palette <- brewer.pal(name = "RdPu", n = 9)[4:9]

# 4. Plot
p_inset <- ggplot(lognorm_dist_long, aes(x = x, y = Density, color = Parameters)) +
  geom_line() +
  labs(x = "Probability",
       y = "Density") +
  theme_minimal() + theme(legend.title = element_text(size = 7), 
                          legend.text = element_text(size = 5),
                          axis.text.x = element_text(size = 6),
                          axis.text.y = element_text(size = 6),
                          axis.title.x = element_text(size = 8),
                          axis.title.y = element_text(size = 8)) + 
  scale_color_manual(values = my_palette)

ggdraw() +
  draw_plot(p_main) +
  draw_plot(p_inset, x = 0.1, y = 0.55, width = 0.4, height = 0.4)

ggsave("./figures/lognorm.pdf", width = 7)
