## Random networks -- Example code 
rm(list=ls())
library(tidyverse)
 # Code to run random Boolean networks analysis
  # load the functions
source("functions.R") 
set.seed(1234)
# Number of Species of each set
nspi <- 50
nspj <- 65
# Expected connectance
connect <- .65
# Number of simulations
nsim <- 250


# Create tibbles to store the results
res <- tibble()
ratio <- tibble()
# B=C
for (i in 1:nsim) {
  model_res <- boolean_model(nspi, nspj, connect, 
                             dist_B = "beta", dist_C = "beta", dist_Cp = "beta", 
                             shape1 = 5, shape2 = 5)
  
  toplot <-  tibble(time_steps=1:nrow(model_res$community), 
                    sp_persistent=apply(model_res$community, 1, sum), 
                    prop_sp=sp_persistent/ncol(model_res$community),
                    iteration = i)
  res <- rbind(res,toplot)
  tmp <- estimate_CB_overtime(model_res) %>% add_column(iteration = i)
  ratio <- rbind(ratio, tmp)
  
  
  print(i)
}

# Object with the results to plot
res

# Distribution (boxplot) of the proportion of species left in each time step 
  # of the simulation
ggplot(res, aes(x =time_steps, y = prop_sp, group=c(time_steps))) +
  geom_boxplot() + 
  theme_bw()


# How many time steps in each simulation until the community converges/stops changing
res %>% group_by(iteration)%>% tally()  %>% ggplot(aes(x=n)) + 
  geom_histogram() + labs(title = "Number of iterations")


## Testing CB ratio
test <- estimate_CB_overtime(model_res)
test <- ratio %>% filter(iteration == 54)

ggplot(test, aes(x = time_step, y = ratio, group = sp_id, color = sp_k_initial)) +
  geom_line() +
  geom_hline(yintercept = 1, lty = 2) +
  #scale_y_log10() +
  scale_color_viridis_c() +
  theme_bw()


# Final distribution of surviving species for all simulations
 # Filtering the last time step of each iteration
tmp <- res %>% group_by(iteration) %>% filter(time_steps == max(time_steps))
# Plot
ggplot(tmp, aes(prop_sp)) + geom_histogram() + theme_bw()



## Alternatively we can think of the proportion not in relation to the initial 
  # pool but after the first species were removed 


## What is the relationship between initial degree and "probability of extinction"
# For probability of extinction we are using a proxy that is number of time steps 
 # a species survived

res %>% group_by(iteration) %>% filter(time_steps == max(time_steps))
