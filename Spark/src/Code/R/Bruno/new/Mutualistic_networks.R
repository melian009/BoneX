# Pacotes necessários
library(readr)   # para read_csv
library(dplyr)
library(writexl)
library(ggplot2)
library(readxl)
library(tidyverse)

caminho_principal <- ""
arquivos <- list.files(path = caminho_principal, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)

importar_rede_bipartida <- function(caminho) {
  # Ler todas as linhas como texto
  linhas <- readLines(caminho)
  
  # Separar por vírgula e converter para numérico
  dados <- lapply(linhas, function(x) as.numeric(strsplit(x, ",")[[1]]))
  
  # Descobrir número máximo de colunas
  ncol_max <- max(sapply(dados, length))
  
  # Completar linhas menores com 0
  dados <- lapply(dados, function(x) c(x, rep(0, ncol_max - length(x))))
  
  # Transformar em matriz
  A_raw <- do.call(rbind, dados)
  
  # Transformar em binário
  A_raw[A_raw > 0] <- 1
  
  plantas <- nrow(A_raw)
  animais <- ncol(A_raw)
  rownames(A_raw) <- paste("P", 1:plantas, sep = "")
  colnames(A_raw) <- paste("A", 1:animais, sep = "")
  
  # Construir matriz bipartida simétrica
  m1 <- cbind(matrix(0, plantas, plantas), A_raw)
  m2 <- cbind(t(A_raw), matrix(0, animais, animais))
  A <- rbind(m1, m2)
  diag(A) <- 0
  
  return(A)
}




