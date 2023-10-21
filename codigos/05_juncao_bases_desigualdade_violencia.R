#' ------------------------------------------------------
#' @author Thiago Cordeiro Almeida
#' @last-update 2023-10-20
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
df_desigualdade <- read_parquet(file = paste0("./output/base de dados - desigualdade/cad_indicadores_municipais.parquet"))

df_homicidios <- read_xlsx("./output/base de dados - violencia/HomicidiosMunicipio.xlsx")
df_intervencao <- read_xlsx("./output/base de dados - violencia/IntervencaoLegalMunicipio.xlsx")

df_pop_municipios <- read_xlsx(
  "./input/CD2022_Populacao_Coletada_Imputada_e_Total_Municipio_e_UF.xlsx",
  sheet = 1,
  range = "B3:H5573"
)


# Tratamento da base de populacao -----------------------------------------

df_pop_municipios <- df_pop_municipios |>
  mutate(cd_municipio_6digitos = as.numeric(paste0(`COD. UF`, `COD. MUNIC`))) |>
  mutate(cd_municipio_6digitos = as.numeric(substr(cd_municipio_6digitos, 1, 6))) |>
  rename(
    nome_municipio = `NOME DO MUNICÍPIO`,
    pop_total = `POP. TOTAL`
  ) |>
  select(cd_municipio_6digitos, nome_municipio, pop_total) |>
  mutate(
    pop_faixas = case_when(
      pop_total <= 5000 ~ 1,
      pop_total <= 10000 ~ 2,
      pop_total <= 25000 ~ 3,
      pop_total <= 50000 ~ 4,
      pop_total <= 100000 ~ 5,
      TRUE ~ 6
    )
  )

# avaliacao de qual a parcela dos municipios que perderiamos

df_pop_municipios |>
  mutate(menor_5k = case_when(pop_total <= 5000 ~ 1, TRUE ~ 0)) |>
  group_by(menor_5k) |>
  count() |>
  ungroup() |>
  summarise(menor_5k = menor_5k, n = n, prop = n/sum(n))

df_pop_municipios |>
  group_by(pop_faixas) |>
  count() |>
  ungroup() |>
  summarise(
    pop_faixas = pop_faixas, n = n, prop = 100*n/sum(n)
  )


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
    homicidios_brancos,homicidios_negros,
    homicidios_masculinos,homicidios_femininos,homicidios_geral,
    indicador_homicidios_brancos,indicador_homicidios_negros,
    indicador_homicidios_masculino,indicador_homicidios_feminino,
    indicador_homicidios_negros_maior_brancos,
    indicador_homicidios_masc_maior_fem
  )

df_intervencao <- df_intervencao |>
  select(
    ano,cd_ibge,
    intervencao_brancos,intervencao_negros,
    intervencao_masculino,intervencao_feminino,intervencao_geral,
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
    "ano","regiao","uf","cd_municipio_7digitos","cd_municipio_6digitos",
    "indicador_em","indicador_em_negros","indicador_em_brancos",
    "indicador_acesso_esgoto","indicador_acesso_esgoto_negros","indicador_acesso_esgoto_brancos",
    "indicador_pbf","indicador_pbf_negros","indicador_pbf_brancos",
    "indicador_renda_outras_fontes","indicador_renda_outras_fontes_negros","indicador_renda_outras_fontes_brancos",
    "indicador_maes_adolescentes","indicador_maes_adolescentes_negras","indicador_maes_adolescentes_brancas",
    "indicador_desocupados","indicador_desocupados_negros","indicador_desocupados_brancos",
    "indicador_informalidade","indicador_informalidade_negros","indicador_informalidade_brancos",
    "homicidios_brancos","homicidios_negros",
    "homicidios_masculinos","homicidios_femininos","homicidios_geral",
    "indicador_homicidios_brancos","indicador_homicidios_negros",
    "indicador_homicidios_masculino","indicador_homicidios_feminino",
    "indicador_homicidios_negros_maior_brancos","indicador_homicidios_masc_maior_fem",
    "intervencao_brancos",'intervencao_negros',
    "intervencao_masculino","intervencao_feminino","intervencao_geral",
    "indicador_intervencao_brancos","indicador_intervencao_negros",
    "indicador_intervencao_masculino","indicador_intervencao_feminino",
    "indicador_intervencao_negros_maior_brancos","indicador_intervencao_masc_maior_fem"
  )))

# juncao com a base de populacao

df_desigualdade_violencia <- df_desigualdade_violencia |>
  left_join(
    df_pop_municipios,
    by = c("cd_municipio_6digitos"),
    keep = FALSE
  ) |>
  select(
    ano,regiao,uf,cd_municipio_7digitos,cd_municipio_6digitos,
    nome_municipio,pop_total,pop_faixas,everything()
  ) |>
  filter(!is.na(nome_municipio))

# Exportacao dos dados ----------------------------------------------------

write_csv2(df_desigualdade_violencia, file = "./output/base de dados - violencia e desigualdade/base_violencia_desigualdade.csv")
write_parquet(df_desigualdade_violencia, sink = "./output/base de dados - violencia e desigualdade/base_violencia_desigualdade.parquet")
haven::write_sav(df_desigualdade_violencia, path = "./output/base de dados - violencia e desigualdade/base_violencia_desigualdade.sav")
