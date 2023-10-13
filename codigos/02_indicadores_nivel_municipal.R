#' ------------------------------------------------------
#' @author Thiago Cordeiro Almeida
#' @last-update 2023-10-13
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

  #aplique a funcao para indicadores em geral
  df <- indicadores_municipais_cad(ano = ano, vars = vars)

  #aplique a funcao para indicadores das mulheres com filhos
  df_mulheres <- indicadores_mulheres_municipais_cad(ano = ano, vars = vars)

  # juncao das bases
  df <- df |>
    left_join(
      df_mulheres,
      by = c("cd_ibge"),
      keep = FALSE
    )

  # exportar dados
  write_parquet(df, sink = file.path(OUT_DIR, paste0("cad_",ano,"_municipal.parquet")))

  # proximo loop
  rm(df, df_mulheres)
  print(paste0("Finalizamos o ano: ", ano,"!!!"))
  gc()
}

# Criacao de variaveis derivadas para variaveis individuais --------------------

# # definicao de diretorio de exportacao e parametros da funcao
#
# anos <- 2013:2018
#
# OUT_DIR <- "./dados"
#
# # aplicacao da funcao
#
# for(i in seq_along(anos)){
#   # defina ano de trabalho
#   ano <- anos[i]
#
#   #aplique a funcao para indicadores das mulheres com filhos
#   df_individual <- indicadores_municipais_cad(ano = ano, vars = vars, indicador = "individual")
#
#   df_individual <- df_individual |>
#     select(cd_ibge, jovens_15_29_desocupados,jovens_negros_15_29_desocupados,jovens_brancos_15_29_desocupados,
#            jovens_15_29_ocupados_informalidade,jovens_15_29_ocupados,jovens_negros_15_29_ocupados_informalidade,
#            jovens_negros_15_29_ocupados,jovens_brancos_15_29_ocupados_informalidade,jovens_brancos_15_29_ocupados)
#
#   # importacao da base com os outros indicadores
#   df <- read_parquet(
#     file.path(OUT_DIR, paste0("cad_",ano,"_municipal.parquet"))
#   ) |>
#     select(-c(jovens_15_29_desocupados,jovens_negros_15_29_desocupados,jovens_brancos_15_29_desocupados,
#               jovens_15_29_ocupados_informalidade,jovens_15_29_ocupados,jovens_negros_15_29_ocupados_informalidade,
#               jovens_negros_15_29_ocupados,jovens_brancos_15_29_ocupados_informalidade,jovens_brancos_15_29_ocupados))
#
#   # juncao das bases
#   df <- df |>
#     left_join(
#       df_individual,
#       by = c("cd_ibge"),
#       keep = FALSE
#     )
#
#   # exportar dados
#   write_parquet(df, sink = file.path(OUT_DIR, paste0("cad_",ano,"_municipal.parquet")))
#
#   # proximo loop
#   rm(df, df_individual)
#   print(paste0("Finalizamos o ano: ", ano,"!!!"))
#   gc()
# }
