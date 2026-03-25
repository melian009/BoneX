## Examples of beta distribution 
 # Figure for the report

library(ggplot2)
library(ggpubr)

p1 <- ggplot() +
  stat_function(fun = dbeta, args = list(shape1 = 5, shape2 = 5), aes(color = "Benefit"), lwd=1.5) +
  stat_function(fun = dbeta, args = list(shape1 = 2.5, shape2 = 3), aes(color = "Cost"), lwd=1.5) +
  scale_color_manual("Curve", values = c("Benefit" = "orchid4", "Cost" = "goldenrod")) +
  theme_minimal() +
  theme(legend.position = "bottom")

p2 <- ggplot() +
  stat_function(fun = dbeta, args = list(shape1 = 2.5, shape2 = 2.5), aes(color = "Benefit"), lwd=1.5) +
  stat_function(fun = dbeta, args = list(shape1 = 0.5, shape2 = 0.5), aes(color = "Cost"), lwd=1.5) +
  scale_color_manual("Curve", values = c("Benefit" = "orchid4", "Cost" = "goldenrod")) +
  theme_minimal() + 
  theme(legend.position = "bottom")

p3 <- ggplot() +
  stat_function(fun = dbeta, args = list(shape1 = 5, shape2 = 2), aes(color = "Benefit"), lwd=1.5) +
  stat_function(fun = dbeta, args = list(shape1 = 2, shape2 = 5), aes(color = "Cost"), lwd=1.5) +
  scale_color_manual("Curve", values = c("Benefit" = "orchid4", "Cost" = "goldenrod")) +
  theme_minimal() + 
  theme(legend.position = "bottom")

p4 <- ggplot() +
  stat_function(fun = dbeta, args = list(shape1 = 1, shape2 = 5), aes(color = "Benefit"), lwd=1.5) +
  stat_function(fun = dbeta, args = list(shape1 = 5, shape2 = 1), aes(color = "Cost"), lwd=1.5) +
  scale_color_manual("Curve", values = c("Benefit" = "orchid4", "Cost" = "goldenrod")) +
  theme_minimal() + 
  theme(legend.position = "bottom")

ggarrange(p1, p2, p3, p4, labels = c("A", "B", "C", "D"),
          ncol=2, nrow =2, common.legend = TRUE, legend = "bottom")

ggsave("example_beta.pdf", width = 5.3, height = 5)
