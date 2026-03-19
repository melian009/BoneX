## Difference/distance between two beta distributions using KL divergence
rm(list = ls())
library(tidyverse)
library(reshape2)
library(RColorBrewer)
library(cowplot)
source("functions.R")
source("functions_betaD.R")

## Simulations to estimate how the difference/distance between costs and benefits
# influences the persistence of species in a community
# We will use two metrics for estimating the divergence between the distributions
# 1. KL divergence (always positive) is a stat. measure that quantifies the 
# difference between two probability distributions
# 2. Difference between the mean of each distribution (first moment)
# E[x] = alpha/(alpha + beta)

# Case studies 
## 1. alpha < beta (with 1.1 < alpha < 5) and beta=5
## Parameter combination
alphas <- expand_grid(a1 = seq(1.1, 5, by = 0.2), 
                      a2 = seq(1.1, 5, by = 0.2)) 

# Adding the estimated divergence/difference
# a2 - a1 so that B-C (positive values survive negative extinct)
setup <- alphas %>% mutate(kl = kl_beta(a2, 5, a1, 5),  # KL divergence
                           diff = diff_mean_beta(a2, 5, a1, 5))  # Diff between means


# how is divergence and difference related?
ggplot(setup %>% mutate(bigger=ifelse(a1>a2, "a1", "a2")), 
       aes(x = diff, y = kl, color = bigger)) + geom_point()

ggplot(setup %>% mutate(A1=ifelse(a1>a2, "a1", "a2")), 
       aes(x = a1, y = a2, fill= diff)) +
  geom_tile() +
  #facet_wrap(~fct_rev(A1)) +
  scale_fill_viridis_c()

setup %>% mutate(A1=ifelse(a1>a2, "a1", "a2")) %>% 
  split(.$A1) %>%
  map(~ ggplot(., aes(x = a1, y = a2, fill = kl)) +
        geom_tile() +
        facet_wrap(~fct_rev(A1)) +
        scale_fill_viridis_c() +
        theme_linedraw()) %>%
  cowplot::plot_grid(plotlist = .)


## Ploting the shape of the functions
ggplot() +
  stat_function(fun = dbeta, args = list(shape1 = 1.1, shape2 = 5), aes(color = "Cp"), lwd=1.5) +
  stat_function(fun = dbeta, args = list(shape1 = 2, shape2 = 5), aes(color = "Cost"), lwd=2.5) +
  stat_function(fun = dbeta, args = list(shape1 = 3, shape2= 5), aes(color = "Benefit"), lwd=1.5) +
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
                               shape1C = pull(setup[j,1]), shape2C = 5, 
                               shape1B = pull(setup[j,2]), shape2B = 5,
                               shape1Cp = 1, shape2Cp = 1)
    
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
  theme_bw() + labs(x = "E[Costs] - E[Benefits]", y = "Proportion of surviving species")


## Inset showing the beta distribution with parameter b=1
# 1. Create a sequence of x values (0 to 1 for beta distribution)
x_values <- seq(0, 1, length = 101)

# 2. Calculate beta densities for different parameters
beta_dist_df <- tibble(
  x = x_values,
  "a=1.1, b=5" = dbeta(x_values, 1.1, 5),
  "a=2, b=5" = dbeta(x_values, 2, 5),
  "a=3, b=5" = dbeta(x_values, 3, 5),
  "a=4, b=5" = dbeta(x_values, 4, 5),
  "a=4.5, b=5" = dbeta(x_values, 4.5, 5)
)

# 3. Melt the data frame into a long format
beta_dist_long <- melt(beta_dist_df, id.vars = "x", variable.name = "Parameters", value.name = "Density")
# Create own color palette
my_palette <- brewer.pal(name = "YlGn", n = 9)[4:9]

# 4. Plot
p_inset <- ggplot(beta_dist_long, aes(x = x, y = Density, color = Parameters)) +
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

ggsave("./figures/beta2.pdf", width = 7)
