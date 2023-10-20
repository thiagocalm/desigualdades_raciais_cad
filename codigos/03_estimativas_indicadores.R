#' ------------------------------------------------------
#' @author Thiago Cordeiro Almeida
#' @last-update 2023-10-16
#' @description Estimativas dos indicadores a nível municipal
#' -----------------------------------------------------
options(scipen = 9999999)
rm(list = ls())
gc()

# bibliotecas -------------------------------------------------------------

if(!require("pacman")) install.packages(("pacman"))
pacman::p_load(tidyverse, arrow)

# Importacao dos dados ------------------------------------

anos <- 2013:2018

for(i in seq_along(anos)){
  # importacao de cada ano
  ano <- anos[i]

  DIR <- paste0("./dados/cad_",ano,"_municipal.parquet")

  # leitura dos dados
  df <- read_parquet(file = DIR)

  # criando variavel ano
  df <- df |>
    mutate(ano = ano)

  # juncao das bases de dados
  if(i == 1){
    indicadores_municipais <- df
  } else{
    indicadores_municipais <- indicadores_municipais |>
      bind_rows(df)
  }

  # proximo loop
  rm(df)
  gc()
  print(paste0("Finalizamos a importacao para o ano: ",ano,"!!!"))
}

# Calculo dos indicadores -------------------------------------------------

indicadores_municipais <- indicadores_municipais |>
  group_by(ano, cd_ibge) |>
  reframe(
    # Ensino Medio
    indicador_em = round(100*(jovens_15_17_frequentam_em/jovens_15_17),2),
    indicador_em_negros = round(100*(jovens_negros_15_17_frequentam_em/jovens_negros_15_17),2),
    indicador_em_brancos = round(100*(jovens_brancos_15_17_frequentam_em/jovens_brancos_15_17),2),

    # infraestrutura
    indicador_acesso_esgoto = round(100*(familias_com_jovens_15_29_esgoto/familias_com_jovens_15_29),2),
    indicador_acesso_esgoto_negros = round(100*(familias_com_jovens_negros_15_29_esgoto/familias_com_jovens_negros_15_29),2),
    indicador_acesso_esgoto_brancos = round(100*(familias_com_jovens_brancos_15_29_esgoto/familias_com_jovens_brancos_15_29),2),

    # PBF
    indicador_pbf = round(100*(familias_com_jovens_15_29_pbf/familias_com_jovens_15_29),2),
    indicador_pbf_negros = round(100*(familias_com_jovens_negros_15_29_pbf/familias_com_jovens_negros_15_29),2),
    indicador_pbf_brancos = round(100*(familias_com_jovens_brancos_15_29_pbf/familias_com_jovens_brancos_15_29),2),

    # renda de outras fontes
    indicador_renda_outras_fontes = round(100*indicador_prop_renda_outras_fontes,2),
    indicador_renda_outras_fontes_negros = round(100*indicador_prop_renda_outras_fontes_negros,2),
    indicador_renda_outras_fontes_brancos = round(100*indicador_prop_renda_outras_fontes_branco,2),

    # Maes adolescentes
    indicador_maes_adolescentes = round(100*(mulheres_15_19anos_com_filho/mulheres_15_19anos),2),
    indicador_maes_adolescentes_negras = round(100*(mulheres_negras_15_19anos_com_filho/mulheres_negras_15_19anos),2),
    indicador_maes_adolescentes_brancas = round(100*(mulheres_brancas_15_19anos_com_filho/mulheres_brancas_15_19anos),2),

    # Desocupação ou inatividade
    indicador_desocupados = round(100*(jovens_15_29_desocupados/jovens_15_29),2),
    indicador_desocupados_negros = round(100*(jovens_negros_15_29_desocupados/jovens_negros_15_29),2),
    indicador_desocupados_brancos = round(100*(jovens_brancos_15_29_desocupados/jovens_brancos_15_29),2),

    # Informalidade
    indicador_informalidade = round(100*(jovens_15_29_ocupados_informalidade/jovens_15_29_ocupados),2),
    indicador_informalidade_negros = round(100*(jovens_negros_15_29_ocupados_informalidade/jovens_negros_15_29_ocupados),2),
    indicador_informalidade_brancos = round(100*(jovens_brancos_15_29_ocupados_informalidade/jovens_brancos_15_29_ocupados),2)
  ) |>
  mutate(
    uf = substr(cd_ibge,1,2),
    regiao = substr(uf,1,1)
  ) |>
  select(ano,regiao, uf, everything()) |>
  mutate_all(~ replace_na(.x,0))


# Tratamento de casos especificos --------------------------------------------
# Nota: valores acima de 100 foram tratados como a média
# Nota2: valores "Inf' foram tratados como 0.

# calculo da media por UF para imputacao
media = indicadores_municipais |>
  filter(!is.infinite(indicador_maes_adolescentes_brancas) & indicador_maes_adolescentes_brancas <= 100) |>
  group_by(uf) |>
  summarise(media_maes_brancas = mean(indicador_maes_adolescentes_brancas)) |>
  ungroup()

# filtro dos casos selecionados para serem tratados
casos_especificos <- indicadores_municipais |>
  select(uf, cd_ibge, ano, indicador_maes_adolescentes_brancas) |>
  filter(indicador_maes_adolescentes_brancas > 100) |>
  mutate(indicador_maes_adolescentes_brancas = case_when(
    is.infinite(indicador_maes_adolescentes_brancas) ~ 0,
    TRUE ~ indicador_maes_adolescentes_brancas
  ))

# processo de construcao da base com os casos selecionados ja tratados
ufs = casos_especificos |> select(uf) |> distinct() |> as_vector()

for(i in seq_along(ufs)){
  estado = ufs[i]

  media_mulheres <- media |> filter(uf == estado)

  casos <- casos_especificos |>
    filter(uf == estado) |>
    mutate(indicador_maes_adolescentes_brancas = case_when(
      indicador_maes_adolescentes_brancas > 100 ~ media_mulheres[1,2][[1]],
      TRUE ~ indicador_maes_adolescentes_brancas
    ))

  if(i == 1){
    casos_tratados <-  casos
  } else{
    casos_tratados <- casos_tratados |>
      bind_rows(casos)
  }
}

# juncao dos casos selecionados tratados à base completa

indicadores_municipais <- indicadores_municipais |>
  left_join(
    casos_tratados,
    by = c("ano","uf","cd_ibge"),
    keep = FALSE
  ) |>
  mutate(indicador_maes_adolescentes_brancas = round(case_when(
    is.na(indicador_maes_adolescentes_brancas.y) ~ indicador_maes_adolescentes_brancas.x,
    TRUE ~ indicador_maes_adolescentes_brancas.y),2)
    ) |>
  select(
    all_of(
      c("ano","regiao","uf","cd_ibge",
      "indicador_em","indicador_em_negros","indicador_em_brancos",
      "indicador_acesso_esgoto","indicador_acesso_esgoto_negros","indicador_acesso_esgoto_brancos",
      "indicador_pbf","indicador_pbf_negros","indicador_pbf_brancos",
      "indicador_renda_outras_fontes","indicador_renda_outras_fontes_negros","indicador_renda_outras_fontes_brancos",
      "indicador_maes_adolescentes","indicador_maes_adolescentes_negras","indicador_maes_adolescentes_brancas",
      "indicador_desocupados","indicador_desocupados_negros","indicador_desocupados_brancos",
      "indicador_informalidade","indicador_informalidade_negros","indicador_informalidade_brancos")
  ))

# Exportacao dos dados ----------------------------------------------------

write_parquet(indicadores_municipais, sink = paste0("./output/cad_indicadores_municipais.parquet"))
write_csv2(indicadores_municipais, file = paste0("./output/cad_indicadores_municipais.csv"))
