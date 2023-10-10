#' ------------------------------------------------------
#' @author Thiago Cordeiro Almeida
#' @last-update 2023-10-08
#' @description Função de construcao dos indicadores a nivel municipal
#' -----------------------------------------------------

# Funcao ------------------------------------------------------------------

indicadores_municipais_cad <- function(ano,vars,indicador = "ambos"){
  # pacotes exigidos
  if(!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse, arrow)

  # diretorio de importacao dos dados
  DIR <- "./dados"
  DIR_DATA <- file.path(DIR,paste0("cad_",ano,".parquet"))

  # importacao da base

  df <- read_parquet(
    file = DIR_DATA,
    as_data_frame = TRUE
  )

  # criando variaveis a nivel municipal

  if(indicador == "individual"){

    ## criando indicadores a nivel do individuo
    df_individual <- df |>
      group_by(cd_ibge) |>
      reframe(
        peso = peso.pes,
        id_familia = id_familia,
        id_pessoa = id_pessoa,
        # ensino medio
        jovens_15_17_frequentam_em = case_when(
          ind_jovem_15_17 == 1 & ind_frequenta_em == 1 ~ 1, TRUE ~ 0
        ),
        jovens_15_17 = ind_jovem_15_17,
        jovens_negros_15_17_frequentam_em = case_when(
          jovens_15_17_frequentam_em == 1 & ind_negro == 1 ~ 1, TRUE ~ 0
        ),
        jovens_negros_15_17 = case_when(
          jovens_15_17 == 1 & ind_negro == 1 ~ 1, TRUE ~ 0
        ),
        jovens_brancos_15_17_frequentam_em = case_when(
          jovens_15_17_frequentam_em == 1 & ind_branco == 1 ~ 1, TRUE ~ 0
        ),
        jovens_brancos_15_17 = case_when(
          jovens_15_17 == 1 & ind_branco == 1 ~ 1, TRUE ~ 0
        ),

        # desocupados ou inativos
        jovens_15_29_desocupados = case_when(
          ind_jovem_15_29 == 1 & ind_desocupado == 1 ~ 1, TRUE ~ 0
        ),
        jovens_15_29 = case_when(
          ind_jovem_15_29 == 1 ~ 1, TRUE ~ 0
        ),
        jovens_negros_15_29_desocupados = case_when(
          jovens_15_29_desocupados == 1 & ind_negro == 1 ~ 1, TRUE ~ 0
        ),
        jovens_negros_15_29 = case_when(
          jovens_15_29 == 1 & ind_negro == 1 ~ 1, TRUE ~ 0
        ),
        jovens_brancos_15_29_desocupados = case_when(
          jovens_15_29_desocupados == 1 & ind_branco == 1 ~ 1, TRUE ~ 0
        ),
        jovens_brancos_15_29 = case_when(
          jovens_15_29 == 1 & ind_branco == 1 ~ 1, TRUE ~ 0
        ),

        #  informalidade
        jovens_15_29_ocupados_informalidade = case_when(
          ind_jovem_15_29 == 1 & ind_ocupado == 1 & ind_informal == 1 ~ 1, TRUE ~ 0
        ),
        jovens_15_29_ocupados = case_when(
          ind_jovem_15_29 == 1 & ind_ocupado == 1 ~ 1, TRUE ~ 0
        ),
        jovens_negros_15_29_ocupados_informalidade = case_when(
          jovens_15_29_ocupados_informalidade == 1 & ind_negro == 1 ~ 1, TRUE ~ 0
        ),
        jovens_negros_15_29_ocupados = case_when(
          jovens_15_29_ocupados == 1 & ind_negro == 1 ~ 1, TRUE ~ 0
        ),
        jovens_brancos_15_29_ocupados_informalidade = case_when(
          jovens_15_29_ocupados_informalidade == 1 & ind_branco == 1 ~ 1, TRUE ~ 0
        ),
        jovens_brancos_15_29_ocupados = case_when(
          jovens_15_29_ocupados == 1 & ind_branco == 1 ~ 1, TRUE ~ 0
        )
      )

    gc()

    # indicador prop renda
    df_renda <- df |>
      filter(ind_jovem_15_29 == 1) |>
      select(cd_ibge, peso = peso.pes, indicador_prop_renda_outras_fontes) |>
      group_by(cd_ibge) |>
      reframe(
        indicador_prop_renda_outras_fontes = weighted.mean(indicador_prop_renda_outras_fontes, peso, na.rm = TRUE)
      )

    df_renda_negros <- df |>
      filter(ind_jovem_15_29 == 1 & ind_negro == 1) |>
      select(cd_ibge, peso = peso.pes, indicador_prop_renda_outras_fontes) |>
      group_by(cd_ibge) |>
      reframe(
        indicador_prop_renda_outras_fontes_negros = weighted.mean(indicador_prop_renda_outras_fontes, peso, na.rm = TRUE)
      )

    df_renda_brancos <- df |>
      filter(ind_jovem_15_29 == 1 & ind_branco == 1) |>
      select(cd_ibge, peso = peso.pes, indicador_prop_renda_outras_fontes) |>
      group_by(cd_ibge) |>
      reframe(
        indicador_prop_renda_outras_fontes_branco = weighted.mean(indicador_prop_renda_outras_fontes, peso, na.rm = TRUE)
      )

    df_renda <- df_renda |>
      left_join(df_renda_negros, by = c("cd_ibge"), keep = FALSE) |>
      left_join(df_renda_brancos, by = c("cd_ibge"), keep = FALSE)

    rm(df_renda_negros, df_renda_brancos)

    # agregando valores individuais a nivel municipal

    df_individual <- df_individual |>
      group_by(cd_ibge) |>
      reframe(
        # ensino medio
        jovens_15_17_frequentam_em = sum(jovens_15_17_frequentam_em * peso),
        jovens_15_17 = sum(jovens_15_17 * peso),
        jovens_negros_15_17_frequentam_em = sum(jovens_negros_15_17_frequentam_em  * peso),
        jovens_negros_15_17 = sum(jovens_negros_15_17  * peso),
        jovens_brancos_15_17_frequentam_em = sum(jovens_brancos_15_17_frequentam_em  * peso),
        jovens_brancos_15_17 = sum(jovens_brancos_15_17  * peso),

        # desocupados ou inativos
        jovens_15_29_desocupados = sum(jovens_15_29_desocupados * peso),
        jovens_15_29 = sum(jovens_15_29 * peso),
        jovens_negros_15_29_desocupados = sum(jovens_negros_15_29_desocupados * peso),
        jovens_negros_15_29 = sum(jovens_negros_15_29 * peso),
        jovens_brancos_15_29_desocupados = sum(jovens_brancos_15_29_desocupados * peso),
        jovens_brancos_15_29 = sum(jovens_brancos_15_29 * peso),

        #  informalidade
        jovens_15_29_ocupados_informalidade = sum(jovens_15_29_ocupados_informalidade * peso),
        jovens_15_29_ocupados = sum(jovens_15_29_ocupados * peso),
        jovens_negros_15_29_ocupados_informalidade = sum(jovens_negros_15_29_ocupados_informalidade * peso),
        jovens_negros_15_29_ocupados = sum(jovens_negros_15_29_ocupados * peso),
        jovens_brancos_15_29_ocupados_informalidade = sum(jovens_brancos_15_29_ocupados_informalidade * peso),
        jovens_brancos_15_29_ocupados = sum(jovens_brancos_15_29_ocupados * peso)
      )

    # juntando renda à base dos individuos

    df_individual <- df_individual |>
      left_join(df_renda,  by = c("cd_ibge"), keep = FALSE)

    # exportacao

    return(df_individual)
  }
  if(indicador == "familiar"){
    df_familiar <- df |>
      group_by(cd_ibge,id_familia) |>
      reframe(
        peso = peso.fam,
        # esgoto
        familias_com_jovens_15_29_esgoto = case_when(
          ind_jovem_15_29 == 1 & ind_esgoto == 1 ~ 1, TRUE ~ 0
        ),
        familias_com_jovens_15_29 =  case_when(
          ind_jovem_15_29 == 1 ~ 1, TRUE ~ 0
        ),
        familias_com_jovens_negros_15_29_esgoto = case_when(
          familias_com_jovens_15_29_esgoto == 1 & ind_negro == 1 ~ 1, TRUE ~ 0
        ),
        familias_com_jovens_negros_15_29 =  case_when(
          familias_com_jovens_15_29 == 1 & ind_negro == 1 ~ 1, TRUE ~ 0
        ),
        familias_com_jovens_brancos_15_29_esgoto = case_when(
          familias_com_jovens_15_29_esgoto == 1 & ind_branco == 1 ~ 1, TRUE ~ 0
        ),
        familias_com_jovens_brancos_15_29 =  case_when(
          familias_com_jovens_15_29 == 1 & ind_branco == 1 ~ 1, TRUE ~ 0
        ),

        #PBF
        familias_com_jovens_15_29_pbf = case_when(
          ind_jovem_15_29 == 1 & ind_pbf == 1 ~ 1, TRUE ~ 0
        ),
        familias_com_jovens_negros_15_29_pbf = case_when(
          familias_com_jovens_15_29_pbf == 1 & ind_negro == 1 ~ 1, TRUE ~ 0
        ),
        familias_com_jovens_brancos_15_29_pbf = case_when(
          familias_com_jovens_15_29_pbf == 1 & ind_branco == 1 ~ 1, TRUE ~ 0
        )
      )

    # agregando valores familiares a nivel do municipio

    df_familiar <- df_familiar |>
      group_by(cd_ibge) |>
      reframe(

        # esgoto
        familias_com_jovens_15_29_esgoto = sum(familias_com_jovens_15_29_esgoto * peso),
        familias_com_jovens_15_29 =  sum(familias_com_jovens_15_29 * peso),
        familias_com_jovens_negros_15_29_esgoto = sum(familias_com_jovens_negros_15_29_esgoto * peso),
        familias_com_jovens_negros_15_29 =  sum(familias_com_jovens_negros_15_29 * peso),
        familias_com_jovens_brancos_15_29_esgoto = sum(familias_com_jovens_brancos_15_29_esgoto * peso),
        familias_com_jovens_brancos_15_29 =  sum(familias_com_jovens_brancos_15_29 * peso),

        #PBF
        familias_com_jovens_15_29_pbf = sum(familias_com_jovens_15_29_pbf * peso),
        familias_com_jovens_negros_15_29_pbf = sum(familias_com_jovens_negros_15_29_pbf * peso),
        familias_com_jovens_brancos_15_29_pbf = sum(familias_com_jovens_brancos_15_29_pbf * peso),
      )

    # retornar valor da nivel municipal para familia

    return(df_familiar)
  }

  if(indicador == "ambos"){

    ## criando indicadores a nivel do individuo
    df_individual <- df |>
      group_by(cd_ibge) |>
      reframe(
        peso = peso.pes,
        id_familia = id_familia,
        id_pessoa = id_pessoa,
        # ensino medio
        jovens_15_17_frequentam_em = case_when(
          ind_jovem_15_17 == 1 & ind_frequenta_em == 1 ~ 1, TRUE ~ 0
        ),
        jovens_15_17 = ind_jovem_15_17,
        jovens_negros_15_17_frequentam_em = case_when(
          jovens_15_17_frequentam_em == 1 & ind_negro == 1 ~ 1, TRUE ~ 0
        ),
        jovens_negros_15_17 = case_when(
          jovens_15_17 == 1 & ind_negro == 1 ~ 1, TRUE ~ 0
        ),
        jovens_brancos_15_17_frequentam_em = case_when(
          jovens_15_17_frequentam_em == 1 & ind_branco == 1 ~ 1, TRUE ~ 0
        ),
        jovens_brancos_15_17 = case_when(
          jovens_15_17 == 1 & ind_branco == 1 ~ 1, TRUE ~ 0
        ),

        # desocupados ou inativos
        jovens_15_29_desocupados = case_when(
          ind_jovem_15_29 == 1 & ind_desocupado == 1 ~ 1, TRUE ~ 0
        ),
        jovens_15_29 = case_when(
          ind_jovem_15_29 == 1 ~ 1, TRUE ~ 0
        ),
        jovens_negros_15_29_desocupados = case_when(
          jovens_15_29_desocupados == 1 & ind_negro == 1 ~ 1, TRUE ~ 0
        ),
        jovens_negros_15_29 = case_when(
          jovens_15_29 == 1 & ind_negro == 1 ~ 1, TRUE ~ 0
        ),
        jovens_brancos_15_29_desocupados = case_when(
          jovens_15_29_desocupados == 1 & ind_branco == 1 ~ 1, TRUE ~ 0
        ),
        jovens_brancos_15_29 = case_when(
          jovens_15_29 == 1 & ind_branco == 1 ~ 1, TRUE ~ 0
        ),

        #  informalidade
        jovens_15_29_ocupados_informalidade = case_when(
          ind_jovem_15_29 == 1 & ind_ocupado == 1 & ind_informal == 1 ~ 1, TRUE ~ 0
        ),
        jovens_15_29_ocupados = case_when(
          ind_jovem_15_29 == 1 & ind_ocupado == 1 ~ 1, TRUE ~ 0
        ),
        jovens_negros_15_29_ocupados_informalidade = case_when(
          jovens_15_29_ocupados_informalidade == 1 & ind_negro == 1 ~ 1, TRUE ~ 0
        ),
        jovens_negros_15_29_ocupados = case_when(
          jovens_15_29_ocupados == 1 & ind_negro == 1 ~ 1, TRUE ~ 0
        ),
        jovens_brancos_15_29_ocupados_informalidade = case_when(
          jovens_15_29_ocupados_informalidade == 1 & ind_branco == 1 ~ 1, TRUE ~ 0
        ),
        jovens_brancos_15_29_ocupados = case_when(
          jovens_15_29_ocupados == 1 & ind_branco == 1 ~ 1, TRUE ~ 0
        )
      )
    print("Finalizamos a parte dos indivíduos!!!")
    gc()
    #'------------------------------------------------------------------------
    # indicador prop renda
    df_renda <- df |>
      filter(ind_jovem_15_29 == 1) |>
      select(cd_ibge, peso = peso.pes, indicador_prop_renda_outras_fontes) |>
      group_by(cd_ibge) |>
      reframe(
        indicador_prop_renda_outras_fontes = weighted.mean(indicador_prop_renda_outras_fontes, peso, na.rm = TRUE)
      )

    df_renda_negros <- df |>
      filter(ind_jovem_15_29 == 1 & ind_negro == 1) |>
      select(cd_ibge, peso = peso.pes, indicador_prop_renda_outras_fontes) |>
      group_by(cd_ibge) |>
      reframe(
        indicador_prop_renda_outras_fontes_negros = weighted.mean(indicador_prop_renda_outras_fontes, peso, na.rm = TRUE)
      )

    df_renda_brancos <- df |>
      filter(ind_jovem_15_29 == 1 & ind_branco == 1) |>
      select(cd_ibge, peso = peso.pes, indicador_prop_renda_outras_fontes) |>
      group_by(cd_ibge) |>
      reframe(
        indicador_prop_renda_outras_fontes_branco = weighted.mean(indicador_prop_renda_outras_fontes, peso, na.rm = TRUE)
      )

    df_renda <- df_renda |>
      left_join(df_renda_negros, by = c("cd_ibge"), keep = FALSE) |>
      left_join(df_renda_brancos, by = c("cd_ibge"), keep = FALSE)

    rm(df_renda_negros, df_renda_brancos)

    # agregando valores individuais a nivel municipal

    df_individual <- df_individual |>
      group_by(cd_ibge) |>
      reframe(
        # ensino medio
        jovens_15_17_frequentam_em = sum(jovens_15_17_frequentam_em * peso),
        jovens_15_17 = sum(jovens_15_17 * peso),
        jovens_negros_15_17_frequentam_em = sum(jovens_negros_15_17_frequentam_em  * peso),
        jovens_negros_15_17 = sum(jovens_negros_15_17  * peso),
        jovens_brancos_15_17_frequentam_em = sum(jovens_brancos_15_17_frequentam_em  * peso),
        jovens_brancos_15_17 = sum(jovens_brancos_15_17  * peso),

        # desocupados ou inativos
        jovens_15_29_desocupados = sum(jovens_15_29_desocupados * peso),
        jovens_15_29 = sum(jovens_15_29 * peso),
        jovens_negros_15_29_desocupados = sum(jovens_negros_15_29_desocupados * peso),
        jovens_negros_15_29 = sum(jovens_negros_15_29 * peso),
        jovens_brancos_15_29_desocupados = sum(jovens_brancos_15_29_desocupados * peso),
        jovens_brancos_15_29 = sum(jovens_brancos_15_29 * peso),

        #  informalidade
        jovens_15_29_ocupados_informalidade = sum(jovens_15_29_ocupados_informalidade * peso),
        jovens_15_29_ocupados = sum(jovens_15_29_ocupados * peso),
        jovens_negros_15_29_ocupados_informalidade = sum(jovens_negros_15_29_ocupados_informalidade * peso),
        jovens_negros_15_29_ocupados = sum(jovens_negros_15_29_ocupados * peso),
        jovens_brancos_15_29_ocupados_informalidade = sum(jovens_brancos_15_29_ocupados_informalidade * peso),
        jovens_brancos_15_29_ocupados = sum(jovens_brancos_15_29_ocupados * peso)
      )

    # juntando renda à base dos individuos

    df_individual <- df_individual |>
      left_join(df_renda,  by = c("cd_ibge"), keep = FALSE)

    rm(df_renda)

    print("Finalizamos a parte da renda!!!")
    gc()

    #'------------------------------------------------------------------------
    ## INDICADORES FAMILIARES
    df_familiar <- df |>
      reframe(
        peso = peso.fam,
        id_familia = id_familia,
        id_pessoa = id_pessoa,

        # esgoto
        familias_com_jovens_15_29_esgoto = case_when(
          ind_jovem_15_29 == 1 & ind_esgoto == 1 ~ 1, TRUE ~ 0
        ),
        familias_com_jovens_15_29 =  case_when(
          ind_jovem_15_29 == 1 ~ 1, TRUE ~ 0
        ),
        familias_com_jovens_negros_15_29_esgoto = case_when(
          familias_com_jovens_15_29_esgoto == 1 & ind_negro == 1 ~ 1, TRUE ~ 0
        ),
        familias_com_jovens_negros_15_29 =  case_when(
          familias_com_jovens_15_29 == 1 & ind_negro == 1 ~ 1, TRUE ~ 0
        ),
        familias_com_jovens_brancos_15_29_esgoto = case_when(
          familias_com_jovens_15_29_esgoto == 1 & ind_branco == 1 ~ 1, TRUE ~ 0
        ),
        familias_com_jovens_brancos_15_29 =  case_when(
          familias_com_jovens_15_29 == 1 & ind_branco == 1 ~ 1, TRUE ~ 0
        ),

        #PBF
        familias_com_jovens_15_29_pbf = case_when(
          ind_jovem_15_29 == 1 & ind_pbf == 1 ~ 1, TRUE ~ 0
        ),
        familias_com_jovens_negros_15_29_pbf = case_when(
          familias_com_jovens_15_29_pbf == 1 & ind_negro == 1 ~ 1, TRUE ~ 0
        ),
        familias_com_jovens_brancos_15_29_pbf = case_when(
          familias_com_jovens_15_29_pbf == 1 & ind_branco == 1 ~ 1, TRUE ~ 0
        )
      ) |>
      group_by(id_familia) |>
      reframe(
        peso = peso,
        # esgoto
        familias_com_jovens_15_29_esgoto = sum(familias_com_jovens_15_29_esgoto),
        familias_com_jovens_15_29 =  sum(familias_com_jovens_15_29),
        familias_com_jovens_negros_15_29_esgoto = sum(familias_com_jovens_negros_15_29_esgoto),
        familias_com_jovens_negros_15_29 =  sum(familias_com_jovens_negros_15_29),
        familias_com_jovens_brancos_15_29_esgoto = sum(familias_com_jovens_brancos_15_29_esgoto),
        familias_com_jovens_brancos_15_29 =  sum(familias_com_jovens_brancos_15_29),

        #PBF
        familias_com_jovens_15_29_pbf = sum(familias_com_jovens_15_29_pbf),
        familias_com_jovens_negros_15_29_pbf = sum(familias_com_jovens_negros_15_29_pbf),
        familias_com_jovens_brancos_15_29_pbf = sum(familias_com_jovens_brancos_15_29_pbf)
      )

    rm(df)
    print("Finalizamos a parte 1/3 da familia!!!")

    # criando binarias para as familias
    df_familiar <- df_familiar |>
      mutate(
        reframe(
          peso = peso,
          # esgoto
          familias_com_jovens_15_29_esgoto = case_when(
            familias_com_jovens_15_29_esgoto > 0 ~ 1, TRUE ~ 0
          ),
          familias_com_jovens_15_29 =  case_when(
            familias_com_jovens_15_29 > 0 ~ 1, TRUE ~ 0
          ),
          familias_com_jovens_negros_15_29_esgoto = case_when(
            familias_com_jovens_negros_15_29_esgoto > 0 ~ 1, TRUE ~ 0
          ),
          familias_com_jovens_negros_15_29 =  case_when(
            familias_com_jovens_negros_15_29 > 0 ~ 1, TRUE ~ 0
          ),
          familias_com_jovens_brancos_15_29_esgoto = case_when(
            familias_com_jovens_brancos_15_29_esgoto > 0 ~ 1, TRUE ~ 0
          ),
          familias_com_jovens_brancos_15_29 =  case_when(
            familias_com_jovens_brancos_15_29 > 0 ~ 1, TRUE ~ 0
          ),

          #PBF
          familias_com_jovens_15_29_pbf = case_when(
            familias_com_jovens_15_29_pbf > 0 ~ 1, TRUE ~ 0
          ),
          familias_com_jovens_negros_15_29_pbf = case_when(
            familias_com_jovens_negros_15_29_pbf > 0 ~ 1, TRUE ~ 0
          ),
          familias_com_jovens_brancos_15_29_pbf = case_when(
            familias_com_jovens_brancos_15_29_pbf > 0 ~ 1, TRUE ~ 0
          )
        )
      )

    print("Finalizamos a parte 2/3 da familia!!!")

    # agregando valores familiares a nivel do municipio

    df_familiar <- df_familiar |>
      group_by(cd_ibge) |>
      reframe(
        # esgoto
        familias_com_jovens_15_29_esgoto = sum(familias_com_jovens_15_29_esgoto * peso),
        familias_com_jovens_15_29 =  sum(familias_com_jovens_15_29 * peso),
        familias_com_jovens_negros_15_29_esgoto = sum(familias_com_jovens_negros_15_29_esgoto * peso),
        familias_com_jovens_negros_15_29 =  sum(familias_com_jovens_negros_15_29 * peso),
        familias_com_jovens_brancos_15_29_esgoto = sum(familias_com_jovens_brancos_15_29_esgoto * peso),
        familias_com_jovens_brancos_15_29 =  sum(familias_com_jovens_brancos_15_29 * peso),

        #PBF
        familias_com_jovens_15_29_pbf = sum(familias_com_jovens_15_29_pbf * peso),
        familias_com_jovens_negros_15_29_pbf = sum(familias_com_jovens_negros_15_29_pbf * peso),
        familias_com_jovens_brancos_15_29_pbf = sum(familias_com_jovens_brancos_15_29_pbf * peso),
      )

    print("Finalizamos a parte 3/3 da familia!!!")

    # juntando familia à base dos individuos

    df <- df_individual |>
      left_join(df_familiar,  by = c("cd_ibge"), keep = FALSE)

    # retorno da base completa a nivel dos municipios

    return(df)

  }
  print(paste0("Finalizamos as bases a nível municipal para o ano: ",ano,"!!!"))

}
