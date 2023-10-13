#' ------------------------------------------------------
#' @author Thiago Cordeiro Almeida
#' @last-update 2023-10-13
#' @description Análises exploratórias
#' -----------------------------------------------------
options(scipen = 9999999)
rm(list = ls())
gc()

# bibliotecas -------------------------------------------------------------

if(!require("pacman")) install.packages(("pacman"))
pacman::p_load(tidyverse, arrow)


# Importacao dos dados ----------------------------------------------------

# leitura dos dados
df <- read_parquet(file = paste0("./dados/cad_indicadores_municipais.parquet"))


# Exploratória da distribuição --------------------------------------------

