# Needed packages
library(readr)   # to read_csv
library(dplyr)
library(writexl)
library(ggplot2)
library(readxl)
library(tidyverse)


#we have empirical data but we can use theoretical networks

## the way to upload all emprical networks
main_path <- "path to data"
archives <- list.files(path = main_path, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)

import_bipartite_net <- function(path) {
  data <- read_csv(path, col_names = FALSE)
  
## or we can create the empirical networks

#A_raw
  
  # Substituir NA (incluindo células vazias) por 0
  data[is.na(data)] <- 0
  
  A_raw <- as.matrix(data) #garantee the object is a matrix
  A_raw[A_raw > 0] <- 1 #binarization of the network
  
#now, we transform the networks in square ones.
  #It was done previously to simplify the aplications of costs and benefitis as single vectors of each
  plants <- nrow(A_raw)
  animals <- ncol(A_raw)
  rownames(A_raw) <- paste("P", 1:plants, sep = "")
  colnames(A_raw) <- paste("A", 1:animals, sep = "")
  m1 <- cbind(matrix(0, plants, plants), A_raw)
  m2 <- cbind(t(A_raw), matrix(0, animals, animals))
  A <- rbind(m1, m2)
  diag(A) <- 0
  return(A)
}