#' ------------------------------------------------------
#' @author Thiago Cordeiro Almeida
#' @last-update 2023-10-19
#' @description Juncao das bases de desigualdade e homicidios
#' -----------------------------------------------------
options(scipen = 9999999)
rm(list = ls())
gc()

# bibliotecas -------------------------------------------------------------

if(!require("pacman")) install.packages(("pacman"))
pacman::p_load(tidyverse, arrow, haven, readxl)

# Importacao dos dados ----------------------------------------------------

# leitura dos dados
df_desigualdade <- read_parquet(file = paste0("./output/cad_indicadores_municipais.parquet"))

df_homicidios <- read_xlsx("./input/dados homicidio/HomicidiosMunicipio.xlsx")
df_intervencao <- read_xlsx("./input/dados homicidio/IntervencaoLegalMunicipio.xlsx")


# Tratamento das bases de obitos -------------------------------------------

# Homicidios

df_homicidios <- df_homicidios |>
  rename(
    ano = Ano,
    cd_ibge = MunicípiodeResidência,
    cd_ibge_nome = MunicipioNome,
    homicidios_brancos = BrancaH,
    homicidios_negros = PretaPardaH,
    homicidios_geral = TotalRacaH,
    indicador_homicidios_brancos = PercBrancaH,
    indicador_homicidios_negros = PercPretaPardaH,
    homicidios_masculinos = MasculinoH,
    homicidios_femininos = FemininoH,
    indicador_homicidios_masculino = PercMasculinoH,
    indicador_homicidios_feminino = PercFemininoH,
    indicador_homicidios_negros_maior_brancos = PretaMaiorBrancaH,
    indicador_homicidios_masc_maior_fem = MascMaiorFemH
  )

# Intervencao Legal

df_intervencao <- df_intervencao |>
  select(-MunicipioNome) |>
  rename(
    ano = Ano,
    cd_ibge = MunicípiodeResidência,
    intervencao_brancos = BrancaIL,
    intervencao_negros = PretaPardaIL,
    intervencao_geral = TotalRacaIL,
    indicador_intervencao_brancos = PercBrancaIL,
    indicador_intervencao_negros = PercPretaPardaIL,
    intervencao_masculino = MasculinoIL,
    intervencao_feminino = FemininoIL,
    indicador_intervencao_masculino = PercMasculinoIL,
    indicador_intervencao_feminino = PercFemininoIL,
    indicador_intervencao_negros_maior_brancos = PretaMaiorBrancaIL,
    indicador_intervencao_masc_maior_fem = MasculinaMaiorFemininoIL
  )

#Selecao somente dos indicadores para compor a base unificada

df_homicidios <- df_homicidios |>
  select(
    ano,cd_ibge,cd_ibge_nome,
    indicador_homicidios_brancos,indicador_homicidios_negros,
    indicador_homicidios_masculino,indicador_homicidios_feminino,
    indicador_homicidios_negros_maior_brancos,
    indicador_homicidios_masc_maior_fem
  )

df_intervencao <- df_intervencao |>
  select(
    ano,cd_ibge,
    indicador_intervencao_brancos,indicador_intervencao_negros,
    indicador_intervencao_masculino,indicador_intervencao_feminino,
    indicador_intervencao_negros_maior_brancos,
    indicador_intervencao_masc_maior_fem
  )

# Juncao das bases --------------------------------------------------------

# violencia

df_violencia <- df_homicidios |>
  left_join(
    df_intervencao,
    by = c("ano","cd_ibge"),
    keep = FALSE
  )

# juncao com a base de desigualdade

df_desigualdade_violencia <- df_desigualdade |>
  mutate(cd_municipio = as.numeric(substr(cd_ibge,1,6))) |>
  right_join(
    df_violencia,
    by = c("ano","cd_municipio" = "cd_ibge"),
    keep = FALSE
  ) |>
  rename(
    cd_municipio_7digitos = cd_ibge,
    cd_municipio_6digitos = cd_municipio,
    nome_municipio = cd_ibge_nome
  ) |>
  select(all_of(c(
    "ano","regiao","uf","cd_municipio_7digitos","cd_municipio_6digitos","nome_municipio",
    "indicador_em","indicador_em_negros","indicador_em_brancos",
    "indicador_acesso_esgoto","indicador_acesso_esgoto_negros","indicador_acesso_esgoto_brancos",
    "indicador_pbf","indicador_pbf_negros","indicador_pbf_brancos",
    "indicador_renda_outras_fontes","indicador_renda_outras_fontes_negros","indicador_renda_outras_fontes_brancos",
    "indicador_maes_adolescentes","indicador_maes_adolescentes_negras","indicador_maes_adolescentes_brancas",
    "indicador_desocupados","indicador_desocupados_negros","indicador_desocupados_brancos",
    "indicador_informalidade","indicador_informalidade_negros","indicador_informalidade_brancos",
    "indicador_homicidios_brancos","indicador_homicidios_negros",
    "indicador_homicidios_masculino","indicador_homicidios_feminino",
    "indicador_homicidios_negros_maior_brancos","indicador_homicidios_masc_maior_fem",
    "indicador_intervencao_brancos","indicador_intervencao_negros",
    "indicador_intervencao_masculino","indicador_intervencao_feminino",
    "indicador_intervencao_negros_maior_brancos","indicador_intervencao_masc_maior_fem"
  )))

# Exportacao dos dados ----------------------------------------------------

write_csv2(df_desigualdade_violencia, file = "./output/base_violencia_desigualdade.csv")
write_parquet(df_desigualdade_violencia, sink = "./output/base_violencia_desigualdade.parquet")
haven::write_sav(df_desigualdade_violencia, path = "./output/base_violencia_desigualdade.sav")
