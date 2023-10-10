#' ------------------------------------------------------
#' @author Thiago Cordeiro Almeida
#' @last-update 2023-10-08
#' @description Criacao de variaveis a nivel municipal
#' -----------------------------------------------------
options(scipen = 9999999)
rm(list = ls())
gc()

# bibliotecas -------------------------------------------------------------

if(!require("pacman")) install.packages(("pacman"))
pacman::p_load(tidyverse, arrow)

# funcoes auxiliares ------------------------------------------------------

source("./codigos/X_func_ind_municipais.R")

# Criacao de variaveis derivadas ---------------------------------------

# definicao de diretorio de exportacao e parametros da funcao

anos <- 2013:2018

OUT_DIR <- "./dados"

# aplicacao da funcao

for(i in seq_along(anos)){
  # defina ano de trabalho
  ano <- anos[i]

  #aplique a funcao
  df <- indicadores_municipais_cad(ano = ano, vars = vars)

  # exportar dados
  write_parquet(df, sink = file.path(OUT_DIR, paste0("cad_",ano,"_municipal.parquet")))

  # proximo loop
  rm(df)
  print(paste0("Finalizamos o ano: ", ano,"!!!"))
  gc()
}

