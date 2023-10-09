#' ------------------------------------------------------
#' @author Thiago Cordeiro Almeida
#' @last-update 2023-10-08
#' @description Função de criacao de variaveis derivadas
#' -----------------------------------------------------

# Funcao ------------------------------------------------------------------

processamento_cad <- function(ano,vars){
  # pacotes exigidos
  if(!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse, arrow)

  # variaveis necessarias

  vars <- c(
    "id_familia","id_pessoa","peso.fam","peso.pes","estrato","classf","cd_ibge","cod_sexo_pessoa",
    "idade","cod_parentesco_rf_pessoa","cod_raca_cor_pessoa","cod_curso_frequenta_memb",
    "cod_trabalhou_memb","cod_afastado_trab_memb","cod_principal_trab_memb",
    "val_remuner_emprego_memb","val_renda_doacao_memb","val_renda_aposent_memb",
    "val_renda_seguro_desemp_memb","val_renda_pensao_alimen_memb",
    "val_outras_rendas_memb","cod_escoa_sanitario_domic_fam", "marc_pbf"
  )

  # diretorio de importacao dos dados
  DIR <- "./dados/brutos"
  DIR_DATA <- file.path(DIR,"parquet",paste0("cad_",ano,".parquet"))

  # importacao da base

  df <- read_parquet(
    file = DIR_DATA,
    col_select = all_of(vars),
    as_data_frame = TRUE
  )

  # criando variaveis derivadas

  df <- df |>
    mutate(
      ind_feminino = case_when(cod_sexo_pessoa == 2 ~ 1, TRUE ~ 0),
      ind_jovem_15_29 = case_when(idade %in% 15:29 ~ 1, TRUE ~ 0),
      ind_jovem_18_29 = case_when(idade %in% 18:29 ~ 1, TRUE ~ 0),
      ind_jovem_15_17 = case_when(idade %in% 15:17 ~ 1, TRUE ~ 0),
      ind_negro = case_when(cod_raca_cor_pessoa %in% c(2,4) ~ 1, TRUE ~ 0),
      ind_branco = case_when(cod_raca_cor_pessoa == 1 ~ 1, TRUE ~ 0),
      ind_frequenta_em = case_when(cod_curso_frequenta_memb %in% c(7,8,11) ~ 1, TRUE ~ 0),
      ind_esgoto = case_when(cod_escoa_sanitario_domic_fam == 1 ~ 1, TRUE ~ 0),
      ind_pbf = case_when(marc_pbf == 1 ~ 1, TRUE ~ 0),
      ind_desocupado = case_when(
         cod_trabalhou_memb == 0 & cod_afastado_trab_memb == 0 ~ 1,
        TRUE ~ 0),
      ind_ocupado = case_when(
        cod_trabalhou_memb == 1 | cod_trabalhou_memb == 0 & cod_afastado_trab_memb == 1 ~ 1,
        TRUE ~ 0),
      ind_informal = case_when(cod_principal_trab_memb %in% c(1,3,5,7) ~ 1, TRUE ~ 0)
    )

  # rendas

  df <- df |>
    mutate(across(starts_with("val_"),~replace_na(.x,0))) |>
    mutate(
      renda_individual = val_remuner_emprego_memb + val_renda_doacao_memb +
        val_renda_aposent_memb + val_renda_seguro_desemp_memb + val_renda_pensao_alimen_memb + val_outras_rendas_memb,
      renda_outras_fontes = renda_individual - val_remuner_emprego_memb,
      indicador_prop_renda_outras_fontes = renda_outras_fontes/renda_individual,
      indicador_prop_renda_outras_fontes = case_when(is.nan(indicador_prop_renda_outras_fontes) ~ 0,
                                                     TRUE ~ indicador_prop_renda_outras_fontes))

  # selecionando variaveis remanescentes
  df <- df |>
    select(-c(
      cod_sexo_pessoa,idade,cod_raca_cor_pessoa,cod_curso_frequenta_memb,marc_pbf,
      cod_escoa_sanitario_domic_fam,cod_trabalhou_memb,cod_afastado_trab_memb,
      cod_principal_trab_memb,val_remuner_emprego_memb,val_renda_doacao_memb,
      val_renda_aposent_memb,val_renda_seguro_desemp_memb,
      val_renda_pensao_alimen_memb,val_outras_rendas_memb
    ))

  return(df)

}
