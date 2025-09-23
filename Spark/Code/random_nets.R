## Random nets
rm(list=ls())
 # Code to run random Boolean networks analysis
 # code where the functions are write
source("functions.R")
# Number of Species
nspi <- 4
nspj <- 5
connect <- .45

test <- boolean_model(nspi, nspj, connect)


test
