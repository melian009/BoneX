library(tidyverse)

#----------------------
# Inputing or creating network
network <- function(N, conectancia) {
  A <- matrix(rbinom(N*N, 1, conectancia), nrow = N, ncol = N)
  diag(A) <- 0
  return(A)
}

#----------------------
# Environmental condition (all times t values)
environment <- function(A_min, A_max, w_min, w_max, t_max = 100){
  t <- seq(1, t_max, by = 1)
  A <- runif(1, A_min, A_max)
  w <- runif(1, w_min, w_max)
  amplitude <- A / 2
  frequency <- (1 + sin(2 * pi * w * t))
  theta <- amplitude * frequency
  return(theta)
}
#----------------------
# Alpha (theta - zi)
alpha_fun <- function(theta, zi) {
  t_max <- length(theta)
  n <- length(zi)
  result <- matrix(NA, nrow = t_max, ncol = n)
  for (time in 1:t_max) {
    result[time, ] <- (theta[time] - zi)
  }
  colnames(result) <- paste0("sp", 1:n)
  rownames(result) <- paste0("t", 1:t_max)
  return(abs(result))
}

#----------------------
# Physiological cost (Cf is just to distinguish from Cp - fisiologico / physiological )
Cf <- function(alpha, Cp){
  t_max = nrow(alpha)
  n <- length(Cp)
  physio <- matrix(NA, nrow = t_max, ncol = n)
  for (time in 1:t_max) {
    physio[time, ] <- (alpha[time, ] * Cp)
  }
  colnames(physio) <- paste0("sp", 1:n)
  rownames(physio) <- paste0("t", 1:t_max)
  return(physio)
}

#----------------------



