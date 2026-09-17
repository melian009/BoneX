## Analytical expectation of the difference between two independent beta distr.

library(tolerance)

pdiffprop

curve(pdiffprop(x, k1 = 10, k2 = 5, n1 = 20, n2 = 20), 
      from = -2, to = 2, 
      col = "gold", lwd = 2,
      main = "CDF of diffprop Distribution",
      xlab = "Difference in Proportions", 
      ylab = "Cumulative Probability")


# Assuming 'my_diffs' is your vector of data
my_diffs <- rdiffprop(n = 500, k1 = 10, k2 = 5, n1 = 20, n2 = 20)
plot(ecdf(my_diffs), 
     main = "Empirical CDF of diffprop Data",
     xlab = "Difference in Proportions",
     col = "red")


plot(ecdf(setup$diff), 
     main = "Empirical CDF of setup Data",
     xlab = "Difference in Proportions",
     col = "violet")


# Theoretical CDF
ggplot() +
  xlim(-1, 1) +
  stat_function(fun = pdiffprop, 
                args = list(k1 = 10, k2 = 5, n1 = 20, n2 = 20),
                color = "darkgreen", linewidth = 1) +
  labs(title = "Theoretical CDF of diffprop",
       x = "Difference", y = "F(x)")

modified_ddiff <- function (x, alpha1, alpha2, beta1, beta2, log = FALSE, 
                            ...) 
{
  d <- x
  ddiffprop2 <- function(d, alpha1, alpha2, beta1, beta2, ...) {
    c1 <-alpha1
    c2 <- alpha2
    b1 <- beta1
    b2 <- beta2
    K <- beta(c1, b1) * beta(c2, b2)
    if (d >= (-1) & d <= 0) {
      out <- try(beta(c1, b2) * F1(b2, c1 + c2 + b1 + b2 -  2, 1 - c2, c1 + b2, 1 + d, 1 - d^2, ...) * 
                   ((-d)^(b1 + b2 - 1) * (1 + d)^(c1 + b2 - 1))/K, silent = TRUE)
    }
    else if (d > 0 & d <= 1) {
      out <- try(beta(c2, b1) * F1(b1, c1 + c2 + b1 + b2 - 
                                     2, 1 - c1, c2 + b1, 1 - d, 1 - d^2, ...) *
                   (d^(b1 + b2 - 1) * (1 - d)^(c2 + b1 - 1))/K, silent = TRUE)
    }
    else out <- 0
    if (inherits(out, "try-error")) 
      out <- -1
    out
  }
  TEMP <- Vectorize(ddiffprop2)
  temp <- TEMP(d = d, alpha1 = alpha1, alpha2 = alpha2, beta1 = beta1, beta2 = beta2, ...)
  ind <- which(temp == -1)
  if (length(ind) > 0) {
    dmin <- pmax(min(d[ind]) - seq(0.1, 0, length = 1000), 
                 -1)
    dmax <- pmin(max(d[ind]) + seq(0, 0.1, length = 1000), 
                 1)
    temp.lower <- TEMP(d = dmin, alpha1 = alpha1, beta1 = beta1, beta2 = beta2, ...)
    temp.upper <- TEMP(d = dmax, alpha1 = alpha1, alpha2 = alpha2, beta1 = beta1, beta2=beta2, ...)
    ind.lower <- min(which(temp.lower == -1))
    ind.lower <- ind.lower - 6:1
    ind.upper <- max(which(temp.upper == -1))
    ind.upper <- ind.upper + 1:6
    TTT <- data.frame(x = c(dmin[ind.lower], dmax[ind.upper]), 
                      d.x = c(temp.lower[ind.lower], temp.upper[ind.upper]))
    tmp.out <- lm(I(log(d.x)) ~ x, data = TTT)
    temp[ind] <- as.numeric(exp(predict(tmp.out, newdata = data.frame(x = d[ind]))))
  }
  if (log) 
    temp <- log(temp)
  temp
}


modified_ddiff(1.5, 0.5, 0.5, 1, 1)



#____________________________________________________________#
# Alternative Solution
dbeta_diff <- function(d, a1, b1, a2, b2) {
  # Normalize input vector and allocate space for results
  res <- numeric(length(d))
  
  # Constant denominator (product of individual beta functions)
  B_prod <- beta(a1, b1) * beta(a2, b2)
  
  for(i in seq_along(d)) {
    val <- d[i]
    
    # 1. Outside the domain (-1, 1)
    if (val <= -1 || val >= 1) {
      res[i] <- 0
      next
    }
    
    # 2. Case: d > 0 (Formula 1)
    if (val > 0) {
      term_coef <- beta(a2, b1) * (val^(b1 + b2 - 1)) * ((1 - val)^(a2 + b1 - 1)) / B_prod
      F1_val <- tolerance::F1(
        a = b1, 
        b = a1 + b1 + a2 + b2 - 2, 
        b.prime = 1 - a1, 
        c = b1 + a2, 
        x = 1 - val, 
        y = 1 - val^2
      )
      res[i] <- term_coef * F1_val
    }
    
    # 3. Case: d < 0 (Formula 2)
    else if (val < 0) {
      term_coef <- beta(a1, b2) * ((-val)^(b1 + b2 - 1)) * ((1 + val)^(a1 + b2 - 1)) / B_prod
      F1_val <- tolerance::F1(
        a = b2, 
        b = 1 - a2, 
        b.prime = a1 + b1 + a2 + b2 - 2, 
        c = a1 + b2, 
        x = 1 - val^2, 
        y = 1 + val
      )
      res[i] <- term_coef * F1_val
    }
    
    # 4. Case: d == 0 
    else {
      if ((a1 + a2 > 1) && (b1 + b2 > 1)) {
        res[i] <- beta(a1 + a2 - 1, b1 + b2 - 1) / B_prod
      } else {
        res[i] <- Inf # Handles the asymptotic limit if parameters are too small
      }
    }
  }
  
  return(res)
}

# Define distribution parameters
a1 <- 2; b1 <- 5   # X ~ Beta(2, 5)
a2 <- 3; b2 <- 2   # Y ~ Beta(3, 2)

# Generate 100,000 simulations for empirical checking
set.seed(42)
sim_X <- rbeta(100000, a1, b1)
sim_Y <- rbeta(100000, a2, b2)
sim_diff <- sim_X - sim_Y

# Plot the empirical distribution
hist(sim_diff, breaks = 60, probability = TRUE, col = "lightgray", border = "white",
     main = "PDF of Difference Between Two Beta Distributions",
     xlab = "d = X - Y", xlim = c(-1, 1))

# Superimpose the analytic PDF using our function
eval_points <- seq(-0.99, 0.99, length.out = 200)
analytic_y <- dbeta_diff(eval_points, a1, b1, a2, b2)

lines(eval_points, analytic_y, col = "royalblue", lwd = 2.5)
legend("topright", legend = c("Simulation (rbeta)", "Analytic (Pham-Gia et al.)"),
       col = c("lightgray", "royalblue"), lwd = c(8, 2.5), bty = "n")



# For the CDF
pbeta_diff <- function(d, a1, b1, a2, b2) {
  res <- numeric(length(d))
  B_prod <- beta(a1, b1) * beta(a2, b2)
  
  for(i in seq_along(d)) {
    val <- d[i]
    
    # Boundary constraints
    if (val <= -1) {
      res[i] <- 0
      next
    }
    if (val >= 1) {
      res[i] <- 1
      next
    }
    
    # Case 1: d <= 0
    if (val <= 0) {
      term_coef <- beta(a1, b2) * ((-val)^(b1 + b2)) * ((1 + val)^(a1 + b2 - 1)) / 
        ((b1 + b2) * B_prod)
      
      F1_val <- tolerance::F1(
        a = b1 + b2, 
        b = 1 - a2, 
        b.prime = a1 + b1 + a2 + b2 - 2, 
        c = b1 + b2 + 1, 
        x = 1 - val^2, 
        y = 1 + val
      )
      res[i] <- term_coef * F1_val
    } 
    
    # Case 2: d > 0 (Using 1 minus the complementary upper probability)
    else {
      term_coef <- beta(a2, b1) * (val^(b1 + b2)) * ((1 - val)^(a2 + b1 - 1)) / 
        ((b1 + b2) * B_prod)
      
      F1_val <- tolerance::F1(
        a = b1 + b2, 
        b = a1 + b1 + a2 + b2 - 2, 
        b.prime = 1 - a1, 
        c = b1 + b2 + 1, 
        x = 1 - val, 
        y = 1 - val^2
      )
      res[i] <- 1 - (term_coef * F1_val)
    }
  }
  
  return(res)
}

# Parameters for X ~ Beta(2, 5) and Y ~ Beta(3, 2)
a1 <- 5; b1 <- 3
a2 <- 5; b2 <- 3.5

# Monte Carlo simulation (100,000 samples)
set.seed(42)
sim_X <- rbeta(100000, a1, b1)
sim_Y <- rbeta(100000, a2, b2)
sim_diff <- sim_X - sim_Y

# Plot the Empirical CDF
plot(ecdf(sim_diff), col = "lightgray", lwd = 5,
     main = "CDF of Difference Between Two Beta Distributions",
     xlab = "d = X - Y", ylab = "P(D <= d)", xlim = c(-1, 1))

# Superimpose our exact analytic CDF calculation
eval_points <- seq(-0.99, 0.99, length.out = 200)
analytic_cdf <- pbeta_diff(eval_points, a1, b1, a2, b2)

lines(eval_points, analytic_cdf, col = "firebrick", lwd = 2)
legend("bottomright", legend = c("Empirical ECDF", "Analytic CDF (Pham-Gia)"),
       col = c("lightgray", "firebrick"), lwd = c(5, 2), bty = "n")
