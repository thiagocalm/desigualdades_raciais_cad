#' ------------------------------------------------------
#' @author Thiago Cordeiro Almeida
#' @last-update 2023-11-25
#' @description Análises do genocídio de jovens negros (100 principais municípios)
#' @update-description Ponderação dos homicídios por população negra
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
df_pop_2010 <- read_parquet(file = "./output/base de dados - desigualdade/pop_2010_indicadores.parquet")

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

# Juncao dos dados de 2010 com os dados de desigualdade e violencia -------

df <- df %>%
  left_join(
    df_pop_2010 %>%
      mutate(
        cd_municipio_6digitos = as.numeric(str_sub(codigo_geografico, 1,6)),
        regiao = str_sub(codigo_geografico, 1,1),
        uf = str_sub(codigo_geografico, 1,2),
        uf = factor(uf, levels = cods_uf$cod, labels = cods_uf$names),
        prop_pop_negra_jovem = prop_pop_negra_jovem/100
      ) %>%
      select(-c(nivel_geografico, codigo_geografico, starts_with("indicador_"))),
    by = c("regiao", "uf","cd_municipio_6digitos"),
    keep = FALSE
  )

# criando variavel de populacao jovem negra = pop_total * prop_pop_jovem_negra

df <- df %>%
  mutate(pop_jovem_negra = pop_total * prop_pop_negra_jovem)

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

top100_homicidios_negros_maior5k <- df |>
  filter(pop_faixas >= 2) %>%
  group_by(cd_municipio_6digitos) |>
  reframe(
    nome_municipio = nome_municipio,
    ordem = row_number(),
    ano = ano,
    regiao = regiao,
    uf = uf,
    homicidios = sum(homicidios_negros),
    pop_total = pop_total,
    pop_jovem_negra = pop_jovem_negra,
    indicador_homicidios_negros = homicidios/pop_jovem_negra*1000,
  ) |>
  filter(ordem == 1) %>%
  select(-c(ano, ordem)) %>%
  arrange(desc(indicador_homicidios_negros)) %>%
  mutate(ranking = row_number()) %>%
  filter(ranking <= 102)

top100_intervencoes_negros_maior5k <- df |>
  group_by(cd_municipio_6digitos) |>
  reframe(
    nome_municipio = nome_municipio,
    ordem = row_number(),
    ano = ano,
    regiao = regiao,
    uf = uf,
    intervencoes = sum(intervencao_negros),
    pop_total = pop_total,
    pop_jovem_negra = pop_jovem_negra,
    indicador_intervencoes_negros = intervencoes/pop_jovem_negra*1000
  ) |>
  filter(ordem == 1) %>%
  select(-c(ano, ordem)) %>%
  arrange(desc(indicador_intervencoes_negros)) %>%
  mutate(ranking = row_number()) %>%
  filter(ranking <= 101)

# Exportacao dos dados de homicidio e intervencao -------------------------

write_csv(top100_homicidios_negros_maior5k, file = "./output/resultados/resultados - ranking genocidio populacao negra - homicidios por pop negra jovem em munic maior 5k.csv")
write_csv(top100_intervencoes_negros_maior5k, file = "./output/resultados/resultados - ranking genocidio populacao negra - intervencaopor pop negra jovem em munic maior 5k.csv")

write_parquet(top100_homicidios_negros_maior5k, sink = "./output/resultados/resultados - ranking genocidio populacao negra - homicidios por pop negra jovem em munic maior 5k.parquet")
write_parquet(top100_intervencoes_negros_maior5k, sink = "./output/resultados/resultados - ranking genocidio populacao negra - intervencao por pop negra jovem em munic maior 5k.parquet")
