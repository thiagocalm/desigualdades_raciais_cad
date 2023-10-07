#' ------------------------------------------------------
#' @project Diagnóstico para o Programa Juventude Negra Viva
#' @author Thiago Cordeiro Almeida
#' @last-update 2023-10-07
#' @description Função de importação e selecao de variaveis
#' -----------------------------------------------------

anos <- 2013:2018
# ano de analise
ano <- anos[i]

vars = TRUE
vars_fam = vars_fam
vars_pes = vars_pes

# Funcao ------------------------------------------------------------------

importacao_tratamento_cad <- function(ano = ano,vars = TRUE,vars_fam = vars_fam,vars_pes = vars_pes){
  # pacotes exigidos
  if(!require("pacman")) install.packages("pacman")
  pacman::p_load(readr, tidyverse)

  # variaveis necessarias

  if(vars == TRUE){
    vars_fam <- c(
      "id_familia", "vlr_renda_media_fam", "cod_local_domic_fam",
      "cod_escoa_sanitario_domic_fam", "qtde_pessoas", "marc_pbf"
    )
    vars_pes <- c(
      "id_familia","peso.fam","peso.pes","estrato","classf","cd_ibge","cod_sexo_pessoa",
      "idade","cod_parentesco_rf_pessoa","cod_raca_cor_pessoa","cod_curso_frequenta_memb",
      "cod_curso_frequentou_pessoa_memb","cod_trabalhou_memb","cod_afastado_trab_memb",
      "cod_principal_trab_memb","val_remuner_emprego_memb","val_renda_bruta_12_meses_memb",
      "val_renda_doacao_memb","val_renda_aposent_memb","val_renda_seguro_desemp_memb",
      "val_renda_pensao_alimen_memb","val_outras_rendas_memb"
    )
  }

  # diretorio de importacao dos dados brutos
  DIR <- "./dados"
  DIR_FAM <- file.path(DIR,"brutos",paste0("base_amostra_familia_",ano,".csv"))
  DIR_PES <- file.path(DIR,"brutos",paste0("base_amostra_pessoa_",ano,".csv"))

  # funcao auxiliar para leitura

  leitura_quebrada_pes <- function(x, pos){
    df <- x |>
      select(all_of(vars_pes)) |>
      mutate_all(~as.numeric(.)) |>
      group_by(id_familia) |>
      mutate(
        ordem = row_number()
      ) |>
      ungroup() |>
      mutate(
        id_pessoa = as.numeric(paste0(id_familia, ordem))
      ) |>
      select(-ordem) |>
      select(id_familia, id_pessoa, everything())

    return(df)
  }

  leitura_quebrada_fam <- function(x, pos){
    df <- x |>
      select(all_of(vars_fam)) |>
      mutate_all(~as.numeric(.))

    return(df)
  }

  # importacao da base de pessoas

  df_pes <- read_csv2_chunked(
    file = DIR_PES,
    chunk_size = 1000000,
    callback = DataFrameCallback$new(leitura_quebrada_pes)
  )

  df_pes <- df_pes |>
    select(id_pessoa) |> distinct()

  # importacao da base de pessoas

  df_fam <- read_csv2_chunked(
    file = DIR_FAM,
    chunk_size = 1000000,
    callback = DataFrameCallback$new(leitura_quebrada_fam)
  )

  # juncao dos dados

  df <- df_pes |>
    left_join(
      df_fam,
      by = c("id_familia"),
      keep = FALSE
    )

  rm(df_pes, df_fam)

  return(df)

}
