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
nsim <- 500
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
                         presence= ifelse(t(tail(model_output$community, 1)) == 1, 
                                          "present", "extinct")[,1],
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
res_degrees <- res_degrees %>% mutate(pars_beta = factor(pars_beta, 
                                                         levels = unique(pars_beta)))

## Is there a relationship between initial degree and "probability of extinction"
# It doesn't seem so
# For probability of extinction we are using a proxy that is number of time steps 
# a species survived
pl_degrees <- ggplot(res_degrees, aes(x = degree, group = presence, alpha = presence,
                         fill = pars_beta)) + 
  geom_histogram(position = "dodge", bins = 18) +
  facet_wrap(~pars_beta) +
  theme_bw() +
  scale_colour_paletteer_d("NatParksPalettes::IguazuFalls") +
  scale_fill_paletteer_d("NatParksPalettes::IguazuFalls") +
  scale_alpha_manual(values = c("extinct" = 0.4, "present" = 1)) +
  guides(fill = "none", color = "none") + 
  theme(legend.position = "bottom", axis.text.x = element_text(size = 5),
        axis.title = element_text(size=7), strip.text = element_text(size=5),
        legend.text = element_text(size = 7))

pl_degrees

#ggsave("./figures/degree_distributions.pdf", pl_degrees, width = 5, height = 4, device = cairo_pdf)


## To actually estimate the probability -- GLM
## Species that went extinct are 1
res_degrees <- res_degrees %>% mutate(status = ifelse(presence == "present", 0, 1))

## Probability of extinction per se
# 1. Fit the logistic regression model for initial degree
model_initial <- glm(status ~ degree, data = res_degrees,
                     family = binomial)

# 2. Check the summary for p-values and coefficients
summary(model_initial)

# 3. Calculate Odds Ratios
exp(coef(model_initial))

## Visualizing
# 1. Calculate empirical extinction rates per degree to avoid overplotting
empirical_data <- res_degrees %>%
  group_by(degree) %>%
  summarise(
    # Proportion of species that went extinct at this specific degree
    empirical_prob = mean(status),
    # Count how many species have this degree (for sizing points)
    sample_size = n()
  )

# 2. Create the visualization
ggplot() +
  # Plot empirical binned points (size adjusted by sample size so rare degrees don't distort trends)
  geom_point(data = empirical_data, aes(x = degree, y = empirical_prob, size = sample_size), 
             alpha = 0.6, color = "darkblue") +
  
  # Overlay the exact logistic regression model curve calculated from the full dataset
  geom_smooth(data = res_degrees, aes(x = degree, y = status),
              method = "glm", method.args = list(family = "binomial"), 
              se = TRUE, color = "red", size = 1.2) +
  
  # Formatting
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(
    title = "Empirical Extinction Probability vs. Species Degree",
    subtitle = "Points show actual extinction rates; Red line shows GLM prediction",
    x = "Species Degree",
    y = "Extinction Probability (%)",
    size = "Number of Species"
  ) +
  theme_minimal()




## The beta parameterization can influence the probability of extinction
model_with_pars <- glm(status ~ degree * pars_beta, data = res_degrees,
                       family = binomial)

# 2. Check the summary for p-values and coefficients
summary(model_with_pars)

# 3. Calculate Odds Ratios
exp(coef(model_with_pars))


# 1. Group and bin data to get empirical proportions without 360k point clutter
empirical_binned <- res_degrees %>%
  group_by(pars_beta, degree) %>%
  summarise(
    empirical_prob = mean(status),
    sample_size = n(),
    .groups = "drop"
  ) %>%
  # Clean up or wrap long label names so they fit nicely in grid headers
  mutate(pars_beta_clean = str_wrap(pars_beta, width = 20))

# Also clean the labels in the main dataset for matching the facets
res_degrees_clean <- res_degrees %>%
  mutate(pars_beta_clean = str_wrap(pars_beta, width = 20))

# 2. Build the Faceted Grid Plot
ggplot() +
  # Empirical binned points (sized by abundance in that bin)
  geom_point(data = empirical_binned, aes(x = degree, y = empirical_prob, size = sample_size), 
             alpha = 0.4, color = "midnightblue") +
  
  # GLM Logistic Curves calculated independently per panel
  geom_smooth(data = res_degrees_clean, aes(x = degree, y = status),
              method = "glm", method.args = list(family = "binomial"), 
              se = TRUE, color = "tomato", size = 1) +
  
  # Grid layout - splits your 6 combinations into clean panels
  facet_wrap(~ pars_beta_clean, scales = "free_x") + 
  
  # Formatting aesthetics
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(
    title = "Context-Dependent Extinction Risks across Parameter Settings",
    subtitle = "Notice how some slopes go downward (protective degree) while the baseline trends upward.",
    x = "Species Degree (k)",
    y = "Empirical Extinction Probability (%)",
    size = "Observations"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 9), # Makes panel titles readable
    panel.spacing = unit(1, "lines")
  )
