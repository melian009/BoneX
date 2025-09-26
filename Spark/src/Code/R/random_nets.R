## Random networks -- Example code 
rm(list=ls())
library(tidyverse)
 # Code to run random Boolean networks analysis
  # load the functions
source("functions.R") 
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
  model_res <- boolean_model(nspi, nspj, connect)
  
  toplot <-  tibble(time_steps=1:nrow(model_res$community), 
                    sp_persistent=apply(model_res$community, 1, sum), 
                    prop_sp=sp_persistent/ncol(model_res$community),
                    iteration = i)
  res <- rbind(res,toplot)
  tmp <- estimate_CB_overtime(model_res) %>% add_column(iteration = i)
  ratio <- rbind(ratio, tmp)
  
  
  print(i)
}

# Object with the results
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
test <- ratio %>% filter(iteration == 7)

ggplot(test, aes(x = time_step, y = ratio, group = sp_id, color = sp_k_initial)) +
  geom_line() +
  geom_hline(yintercept = 1, lty = 2) +
  scale_color_viridis_c() +
  theme_bw()


test %>% filter(sp_id == "sp25")
