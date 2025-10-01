# =============================================================================
# MASTER FUNCTION: CREAT ES x sp NETWORKS
# =============================================================================


ecosystem_services_network = function(sp_n, 
                                      services_n,
                                      type = "random",
                                      distribution = "uniform",
                                      connectance = 0.3,
                                      center = 1,
                                      n_modules = 3,
                                      internal_connectance = 0.8,
                                      external_connectance = 0.1,
                                      nested_degree = 0.8,
                                      nested_min_connectance = 0.1,
                                      nested_max_connectance = 0.9,
                                      intensity_min = 0.1,
                                      intensity_max = 1.0,
                                      mean_normal = 0.5,
                                      sd_normal = 0.15,
                                      meanlog = -0.5,
                                      sdlog = 0.5,
                                      shape1_beta = 2,
                                      shape2_beta = 5) {
  
  ES = switch(type,
              
              "random" = {
                ES_binary = matrix(rbinom(sp_n * services_n, 1, connectance), 
                                   nrow = sp_n, ncol = services_n)
                
                # generate values following some distribution
                values = switch(distribution,
                                "uniform" = runif(sp_n * services_n, intensity_min, intensity_max),
                                "normal" = pmax(pmin(rnorm(sp_n * services_n, mean_normal, sd_normal), intensity_max), intensity_min),
                                "lognormal" = {
                                  vals = rlnorm(sp_n * services_n, meanlog, sdlog)
                                  (vals - min(vals)) / (max(vals) - min(vals)) * (intensity_max - intensity_min) + intensity_min
                                },
                                "beta" = rbeta(sp_n * services_n, shape1_beta, shape2_beta) * (intensity_max - intensity_min) + intensity_min
                )
                
                ES = ES_binary * values
                ES
              },
              
              "star" = {
                ES = matrix(0, nrow = sp_n, ncol = services_n)
                
                # centrla species
                ES[center, ] = switch(distribution,
                                      "uniform" = runif(services_n, 0.7, intensity_max),
                                      "normal" = pmax(pmin(rnorm(services_n, mean_normal, sd_normal), intensity_max), 0.7),
                                      "lognormal" = {
                                        vals = rlnorm(services_n, meanlog, sdlog)
                                        (vals - min(vals)) / (max(vals) - min(vals)) * (intensity_max - 0.7) + 0.7
                                      },
                                      "beta" = rbeta(services_n, shape1_beta, shape2_beta) * (intensity_max - 0.7) + 0.7
                )
                
                # Periferal species
                for(i in setdiff(1:sp_n, center)) {
                  n_services_provided = rbinom(1, services_n, 0.2)
                  if(n_services_provided > 0) {
                    services_to_provide = sample(1:services_n, n_services_provided)
                    
                    ES[i, services_to_provide] = switch(distribution,
                                                        "uniform" = runif(n_services_provided, intensity_min, 0.4),
                                                        "normal" = pmax(pmin(rnorm(n_services_provided, mean_normal, sd_normal), 0.4), intensity_min),
                                                        "lognormal" = {
                                                          vals = rlnorm(n_services_provided, meanlog, sdlog)
                                                          (vals - min(vals)) / (max(vals) - min(vals)) * (0.4 - intensity_min) + intensity_min
                                                        },
                                                        "beta" = rbeta(n_services_provided, shape1_beta, shape2_beta) * (0.4 - intensity_min) + intensity_min
                    )
                  }
                }
                ES
              },
              
              "modular" = {
                ES = matrix(0, nrow = sp_n, ncol = services_n)
                
                species_per_module = floor(sp_n / n_modules)
                modules_species = rep(1:n_modules, each = species_per_module)
                if(length(modules_species) < sp_n) {
                  modules_species = c(modules_species, rep(n_modules, sp_n - length(modules_species)))
                }
                
                services_per_module = floor(services_n / n_modules)
                modules_services = rep(1:n_modules, each = services_per_module)
                if(length(modules_services) < services_n) {
                  modules_services = c(modules_services, rep(n_modules, services_n - length(modules_services)))
                }
                
                for(i in 1:sp_n) {
                  for(j in 1:services_n) {
                    if(modules_species[i] == modules_services[j]) {
                      if(runif(1) < internal_connectance) {
                        ES[i, j] = switch(distribution,
                                          "uniform" = runif(1, 0.5, intensity_max),
                                          "normal" = max(min(rnorm(1, mean_normal, sd_normal), intensity_max), 0.5),
                                          "lognormal" = {
                                            val = rlnorm(1, meanlog, sdlog)
                                            (val / max(val, 0.01)) * (intensity_max - 0.5) + 0.5
                                          },
                                          "beta" = rbeta(1, shape1_beta, shape2_beta) * (intensity_max - 0.5) + 0.5
                        )
                      }
                    } else {
                      if(runif(1) < external_connectance) {
                        ES[i, j] = switch(distribution,
                                          "uniform" = runif(1, intensity_min, 0.3),
                                          "normal" = max(min(rnorm(1, mean_normal, sd_normal), 0.3), intensity_min),
                                          "lognormal" = {
                                            val = rlnorm(1, meanlog, sdlog)
                                            (val / max(val, 0.01)) * (0.3 - intensity_min) + intensity_min
                                          },
                                          "beta" = rbeta(1, shape1_beta, shape2_beta) * (0.3 - intensity_min) + intensity_min
                        )
                      }
                    }
                  }
                }
                
                attr(ES, "modules_species") = modules_species
                attr(ES, "modules_services") = modules_services
                ES
              },
              
              "path" = {
                ES = matrix(0, nrow = sp_n, ncol = services_n)
                
                for(j in 1:services_n) {
                  # Boarders
                  ES[1, j] = switch(distribution,
                                    "uniform" = runif(1, 0.6, intensity_max),
                                    "normal" = max(min(rnorm(1, mean_normal, sd_normal), intensity_max), 0.6),
                                    "lognormal" = rlnorm(1, meanlog, sdlog) %% (intensity_max - 0.6) + 0.6,
                                    "beta" = rbeta(1, shape1_beta, shape2_beta) * (intensity_max - 0.6) + 0.6
                  )
                  
                  ES[sp_n, j] = ES[1, j]
                  
                  # Center
                  for(i in 2:(sp_n-1)) {
                    if(runif(1) < 0.3) {
                      ES[i, j] = switch(distribution,
                                        "uniform" = runif(1, intensity_min, 0.4),
                                        "normal" = max(min(rnorm(1, mean_normal, sd_normal), 0.4), intensity_min),
                                        "lognormal" = rlnorm(1, meanlog, sdlog) %% (0.4 - intensity_min) + intensity_min,
                                        "beta" = rbeta(1, shape1_beta, shape2_beta) * (0.4 - intensity_min) + intensity_min
                      )
                    }
                  }
                }
                ES
              },
              
              "nested" = {
                ES = matrix(0, nrow = sp_n, ncol = services_n)
                
                for(i in 1:sp_n) {
                  base_prob_sp = nested_max_connectance - 
                    ((i - 1) / sp_n) * (nested_max_connectance - nested_min_connectance) * nested_degree
                  
                  for(j in 1:services_n) {
                    base_prob_serv = nested_max_connectance - 
                      ((j - 1) / services_n) * (nested_max_connectance - nested_min_connectance) * nested_degree
                    
                    prob_interaction = base_prob_sp * base_prob_serv
                    
                    if(runif(1) < prob_interaction) {
                      intensity = intensity_min + (intensity_max - intensity_min) * (1 - (i - 1) / sp_n)
                      
                      ES[i, j] = switch(distribution,
                                        "uniform" = runif(1, intensity * 0.7, intensity),
                                        "normal" = max(min(rnorm(1, mean_normal, sd_normal), intensity), intensity * 0.7),
                                        "lognormal" = rlnorm(1, meanlog, sdlog) %% (intensity - intensity * 0.7) + intensity * 0.7,
                                        "beta" = rbeta(1, shape1_beta, shape2_beta) * (intensity - intensity * 0.7) + intensity * 0.7
                      )
                    }
                  }
                }
                
                attr(ES, "nested_degree") = nested_degree
                ES
              }
  )
  
  rownames(ES) = paste0("sp", 1:sp_n)
  colnames(ES) = paste0("service", 1:services_n)
  attr(ES, "type") = type
  attr(ES, "distribution") = distribution
  
  return(ES)
}

#=========================================================================
#Exemples

services = ecosystem_services_network(sp_n = 20,services_n = 5,type = "modular", distribution = "lognormal",n_modules = 3,
                                        internal_connectance = 0.8, external_connectance = 0.03)
