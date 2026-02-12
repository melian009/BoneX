## Random networks -- Example code 
rm(list=ls())
library(tidyverse)
 # Code to run random Boolean networks analysis
  # load the functions
source("functions_betaD.R") 
set.seed(1234)
# Number of Species of each set
nspi <- 50
nspj <- 65
# Expected connectance
connect <- .65
# Number of simulations
nsim <- 100


## Ploting the shape of the functions
ggplot() +
  stat_function(fun = dbeta, args = list(shape1 = 2.5, shape2 = 5), aes(color = "Cost"), lwd=1.5) +
  stat_function(fun = dbeta, args = list(shape1 = 3, shape2 = 5), aes(color = "Benefit"), lwd=2.5) +
  #stat_function(fun = dbeta, args = list(shape1 = 2, shape2 = 2), aes(color = "Cp"), lwd=1.5) +
  scale_color_manual("Curve", values = c("Benefit" = "orchid4", "Cost" = "goldenrod", "Cp" = "darkgreen")) +
  theme_minimal()

# Create tibbles to store the results
res <- tibble()
ratio <- tibble()
degrees <- tibble()
# Choose values for each distribution
for (i in 1:nsim) {
  model_res <- boolean_model(nspi, nspj, connect, 
                             shape1C = 2.5, shape2C = 5, 
                             shape1B = 3, shape2B = 5,
                             shape1Cp = 2, shape2Cp = 2)
  
  toplot <-  tibble(time_steps=1:nrow(model_res$community), 
                    sp_persistent=apply(model_res$community, 1, sum), 
                    prop_sp=sp_persistent/ncol(model_res$community),
                    iteration = i)
  res <- rbind(res,toplot)
  tmp <- estimate_CB_overtime(model_res) %>% add_column(iteration = i)
  ratio <- rbind(ratio, tmp)
  tmp_degree <- tibble(species = names(rowSums(model_res$A)),
                       degree=rowSums(model_res$A), 
                       presence= ifelse(t(tail(model_res$community, 1)) == 1, "present", "extinct"),
                       iteration = i)
  degrees <- rbind(degrees, tmp_degree)
  
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
tmp2 <- res %>% filter(time_steps>1) %>% group_by(iteration) %>% 
  mutate(prop_sp_t2 = sp_persistent/sp_persistent[1]) %>% 
  filter(time_steps == max(time_steps)) %>% ungroup()

ggplot(tmp2, aes(prop_sp_t2)) + geom_histogram() + theme_bw()

## Is there a relationship between initial degree and "probability of extinction"
  # It doesn't seem so
# For probability of extinction we are using a proxy that is number of time steps 
 # a species survived
ggplot(degrees , aes(x = degree, group = presence, fill = presence)) + 
  geom_histogram()

## How degree distribution changes as species get prunned
## Choose one iteration to see the result (1-100) - they all look similar
ggplot(ratio %>% filter(iteration == 45), aes(x=time_step, y=sp_k_curr, group = time_step)) + 
  geom_violin() +geom_jitter(alpha = 0.5) + theme(legend.position = "none") + theme_bw()

# Another way to look at this data
ggplot(ratio %>% filter(iteration == 45), aes(x=sp_k_curr, group = sp_id, fill = sp_id)) + 
  geom_histogram() + theme_bw() + theme(legend.position = "none") +
  facet_wrap((~time_step))
