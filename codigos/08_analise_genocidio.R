#' ------------------------------------------------------
#' @author Thiago Cordeiro Almeida
#' @last-update 2023-11-15
#' @description Análises do genocídio de jovens negros (100 principais municípios)
#' -----------------------------------------------------
options(scipen = 9999999)
rm(list = ls())
gc()

# bibliotecas -------------------------------------------------------------

if(!require("pacman")) install.packages(("pacman"))
pacman::p_load(tidyverse, arrow, openxlsx, Hmisc, corrplot)

# Importacao dos dados ----------------------------------------------------
DIR <- "./output/base de dados - violencia e desigualdade"

# leitura dos dados
df <- read_parquet(file = file.path(DIR, "base_violencia_desigualdade.parquet"))
# df_uf <- read_parquet(file = "./output/base de dados - desigualdade/cad_indicadores_uf.parquet")

# Definicao da base de trabalho -------------------------------------------

cods_uf <- tibble(cod = c("11","12","13","14","15","16","17","21","22","23","24","25","26","27",
                          "28","29","31","32","33","35","41","42","43","50","51","52","53"),
                  names = c("Rondônia","Acre","Amazonas", "Roraima","Pará","Amapá","Tocantins","Maranhão",
                            "Piauí", "Ceará","Rio Grande do Norte","Paraíba","Pernambuco","Alagoas","Sergipe",
                            "Bahia","Minas Gerais","Espírito Santo","Rio de Janeiro","São Paulo","Paraná","Santa Catarina",
                            "Rio Grande do Sul","Mato Grosso do Sul","Mato Grosso","Goiás","Distrito Federal"))

# Nota: vamos trabalhar somente com os municipios com populacao acima de 5000 habitantes
df <- df |>
  mutate(uf = factor(uf, levels = cods_uf$cod, labels = cods_uf$names))

# analise do top X ao longo do tempo --------------------------------------

top100_homicidios <- df |>
  group_by(cd_municipio_6digitos) |>
  reframe(
    nome_municipio = nome_municipio,
    ordem = row_number(),
    ano = ano,
    regiao = regiao,
    uf = uf,
    homicidios = sum(homicidios_geral),
    # intervencoes = sum(intervencao_geral),
    pop_total = pop_total,
    indicador_homicidios = homicidios/pop_total*1000,
    # indicador_intervencoes = intervencoes/pop_total*1000
  ) |>
  filter(ordem == 1) %>%
  select(-c(ano, ordem)) %>%
  arrange(desc(indicador_homicidios)) %>%
  mutate(ranking = row_number())

top100_intervencoes <- df |>
  group_by(cd_municipio_6digitos) |>
  reframe(
    nome_municipio = nome_municipio,
    ordem = row_number(),
    ano = ano,
    regiao = regiao,
    uf = uf,
    # homicidios = sum(homicidios_geral),
    intervencoes = sum(intervencao_geral),
    pop_total = pop_total,
    # indicador_homicidios = homicidios/pop_total*1000,
    indicador_intervencoes = intervencoes/pop_total*1000
  ) |>
  filter(ordem == 1) %>%
  select(-c(ano, ordem)) %>%
  arrange(desc(indicador_intervencoes)) %>%
  mutate(ranking = row_number())

# Homicidios e intervencoes da populacao negra ----------------------------

top100_homicidios_negros <- df |>
  group_by(cd_municipio_6digitos) |>
  reframe(
    nome_municipio = nome_municipio,
    ordem = row_number(),
    ano = ano,
    regiao = regiao,
    uf = uf,
    homicidios = sum(homicidios_negros),
    # intervencoes = sum(intervencao_geral),
    pop_total = pop_total,
    indicador_homicidios_negros = homicidios/pop_total*1000,
    # indicador_intervencoes = intervencoes/pop_total*1000
  ) |>
  filter(ordem == 1) %>%
  select(-c(ano, ordem)) %>%
  arrange(desc(indicador_homicidios_negros)) %>%
  mutate(ranking = row_number()) %>%
  filter(ranking <= 100)

top100_intervencoes_negros <- df |>
  group_by(cd_municipio_6digitos) |>
  reframe(
    nome_municipio = nome_municipio,
    ordem = row_number(),
    ano = ano,
    regiao = regiao,
    uf = uf,
    # homicidios = sum(homicidios_geral),
    intervencoes = sum(intervencao_negros),
    pop_total = pop_total,
    # indicador_homicidios = homicidios/pop_total*1000,
    indicador_intervencoes_negros = intervencoes/pop_total*1000
  ) |>
  filter(ordem == 1) %>%
  select(-c(ano, ordem)) %>%
  arrange(desc(indicador_intervencoes_negros)) %>%
  mutate(ranking = row_number()) %>%
  filter(ranking <= 100)

# Exportacao dos dados de homicidio e intervencao -------------------------

write_csv(top100_homicidios_negros, file = "./output/resultados/resultados - ranking genocidio populacao negra - homicidios.csv")
write_csv(top100_intervencoes_negros, file = "./output/resultados/resultados - ranking genocidio populacao negra - intervencao.csv")

write_parquet(top100_homicidios_negros, sink = "./output/resultados/resultados - ranking genocidio populacao negra - homicidios.parquet")
write_parquet(top100_intervencoes_negros, sink = "./output/resultados/resultados - ranking genocidio populacao negra - intervencao.parquet")
