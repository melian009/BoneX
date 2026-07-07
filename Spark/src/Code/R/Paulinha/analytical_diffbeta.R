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
