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
source("str_functions.R")
source("str_functions_betaD.R")

# Number of Species of each set
nspi <- 60
nspj <- 75
## Proportion that is core/peripheral
core <- 0.3 # percentage that are core
nspi_c <- round(nspi*core)
nspi_p <- nspi-nspi_c #the remaining are peripheral

nspj_c <- round(nspj*core)
nspj_p <- nspi-nspj_c #the remaining are peripheral

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
    model_output <- boolean_model(nspi_c, nspj_c, nspi_p, nspj_p,
                                  connect, 
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

#ggsave("./figures/str_degree_distributions.pdf", pl_degrees, width = 5, height = 4, device = cairo_pdf)


tmp_degree <- tmp_degree %>% mutate(status = ifelse(presence == "present", 0, 1))
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
             alpha = 0.6, color = "midnightblue") +
  
  # Overlay the exact logistic regression model curve calculated from the full dataset
  geom_smooth(data = res_degrees, aes(x = degree, y = status),
              method = "glm", method.args = list(family = "binomial"), 
              se = TRUE, color = "tomato", size = 1.2) +
  
  # Formatting
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(
    title = "Empirical Extinction Probability vs. Species Degree",
    subtitle = "Points show actual extinction rates; Red line shows GLM prediction",
    x = "Species Degree",
    y = "Extinction Probability (%)",
    size = "Number of Species"
  ) +
  theme_bw()


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
  geom_point(data = empirical_binned, aes(x = degree, y = empirical_prob, 
                                          size = sample_size, color = pars_beta, alpha=0.35)) +
  
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



## Classifying core and peripheral species to fit a new model
res_degrees <- res_degrees %>% mutate(species_type = ifelse(str_detect(species, "_c"), "core", "peripheral"))


# A more parsimonious model structure
model_clean <- glm(status ~ (degree * pars_beta) + (degree * type_sp), 
                   family = binomial, 
                   data = res_degrees)

summary(model_clean)


# 1. Compress 360k rows into binned empirical probabilities using your exact columns
empirical_binned_type <- res_degrees %>%
  group_by(pars_beta, type_sp, degree) %>%
  summarise(
    empirical_prob = mean(status),
    sample_size = n(),
    .groups = "drop"
  ) %>%
  mutate(pars_beta_clean = str_wrap(pars_beta, width = 20))

# Clean labels in the master dataset for matching
res_degrees_clean <- res_degrees %>%
  mutate(pars_beta_clean = str_wrap(pars_beta, width = 20))

# 2. Plot the relationship
ggplot() +
  # Empirical points sized by abundance and colored by type_sp
  geom_point(data = empirical_binned_type, 
             aes(x = degree, y = empirical_prob, size = sample_size, color = type_sp), 
             alpha = 0.35) +
  
  # Independent GLM trendlines for central vs peripheral per facet
  geom_smooth(data = res_degrees_clean, 
              aes(x = degree, y = status, color = type_sp, fill = type_sp),
              method = "glm", method.args = list(family = "binomial"), 
              se = TRUE, size = 1) +
  
  # Facet grid wrapping by parameter configurations
  facet_wrap(~ pars_beta_clean, scales = "free_x") + 
  
  # Color schemes & formatting
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_color_manual(values = c("core" = "#D55E00", "peripheral" = "#0072B2")) +
  scale_fill_manual(values = c("core" = "#D55E00", "peripheral" = "#0072B2")) +
  labs(
    title = "Extinction Risk Drivers: Degree, Position, and Dynamics",
    subtitle = "Comparing central vs. peripheral vulnerability slopes across configurations",
    x = "Species Degree (k)",
    y = "Extinction Probability (%)",
    size = "Sample Size",
    color = "Species Type",
    fill = "Species Type"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 9),
    panel.spacing = unit(1, "lines"),
    legend.position = "bottom"
  )




#______________________________________________________#
# 1. Get fitted lines from clean model
res_degrees$predicted_prob <- predict(model_clean, type = "response")

# 2. Extract unique prediction lines for smooth plotting (prevents jagged zig-zag lines)
prediction_lines <- res_degrees %>%
  select(pars_beta, type_sp, degree, predicted_prob) %>%
  distinct() %>%
  mutate(pars_beta_clean = str_wrap(pars_beta, width = 20))

# 3. Compress raw 360k rows into binned empirical data points
empirical_binned <- res_degrees %>%
  group_by(pars_beta, type_sp, degree) %>%
  summarise(
    empirical_prob = mean(status),
    sample_size = n(),
    .groups = "drop"
  ) %>%
  mutate(pars_beta_clean = str_wrap(pars_beta, width = 20))

# 4. Generate the definitive plot
ggplot() +
  # Binned empirical points
  geom_point(data = empirical_binned, 
             aes(x = degree, y = empirical_prob, size = sample_size, color = pars_beta, shape = type_sp),
             alpha = 0.5) +
  
  # Exact model-fit lines (replaces geom_smooth to reflect your actual formula constraints)
  geom_line(data = prediction_lines, 
            aes(x = degree, y = predicted_prob, color = pars_beta, linetype=type_sp), 
            size = 1.2, alpha =0.7) +
  
  facet_wrap(~ pars_beta_clean, scales = "free_x") + 
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_color_paletteer_d("NatParksPalettes::IguazuFalls") +
  scale_fill_paletteer_d("NatParksPalettes::IguazuFalls") +
  scale_shape_manual(values = c("core" = 16, "peripheral" = 17)) +
  #scale_alpha_manual(values = c("core" = 1.0, "peripheral" = 0.4)) +
  labs(
    title = "Model-Fitted Extinction Risk: Degree, Position, and Beta Parameters",
    subtitle = "Colors show parameters for Costs and Benefits, ",
    x = "Species Degree (k)",
    y = "Extinction Probability (%)",
    size = "Sample Size",
    color = "Species Network Type"
  ) +
  theme_minimal() +
  guides(color = "none", linetype = "none") +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
