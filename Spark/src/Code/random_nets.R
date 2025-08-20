## Random nets
rm(list=ls())
 # Code to run random Boolean networks analysis
 # code where the functions are write
source("functions.R")
# Number of Species
nspi <- 50
nspj <- 65
connect <- .65
nsim <- 500

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

res

ggplot(res, aes(x =time_steps, y = prop_sp, group=c(time_steps))) +
  geom_boxplot() + 
  theme_bw()


# How many time steps in each simulation until the community converges/stops changing
res %>% group_by(iteration)%>% tally()  %>% ggplot(aes(x=n)) + 
  geom_histogram() + labs(title = "Number of iterations")

res

## Testing CB ratio
test <- estimate_CB_overtime(model_res)
test <- ratio %>% filter(iteration == 2)

ggplot(test, aes(x = time_step, y = ratio, group = sp_id, color = sp_k)) +
  geom_line() +
  geom_hline(yintercept = 1, lty = 2) +
  theme_bw()



