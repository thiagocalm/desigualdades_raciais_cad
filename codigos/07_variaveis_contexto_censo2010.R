#' ------------------------------------------------------
#' @author Thiago Cordeiro Almeida
#' @last-update 2023-11-15
#' @description Estimativasde proporção da população negra e rural por municipio
#' -----------------------------------------------------
options(scipen = 9999999)
rm(list = ls())
gc()

# bibliotecas -------------------------------------------------------------

if(!require("pacman")) install.packages(("pacman"))
pacman::p_load(tidyverse, arrow, censobr)

# Selecionando variaveis de interesse -------------------------------------

censobr::data_dictionary(year = 2010, dataset = "population")

# Importacao dos dados de interesse ---------------------------------------

df_pop_2010 <- read_population(
  year = 2010,
  columns = c("V0001","V0002","V0010","V0606","V1006"),
  as_data_frame = TRUE,
  showProgress = TRUE,
  cache = FALSE
)

# Manipulação dos dados

df_pop_2010 <- df_pop_2010 %>%
  as_tibble() %>%
  rename(
    uf = V0001,
    peso = V0010,
    raca = V0606,
    situacao_dom = V1006
  ) %>%
  mutate(
    cod_munic_7digitos = as.numeric(paste0(uf,V0002)),
    pop_negra = case_when(raca %in% c(2,4) ~ 1, TRUE ~ 0),
    pop_rural = case_when(situacao_dom == 2 ~ 1, TRUE ~ 0)
  ) %>%
  mutate_all(., ~as.numeric(.x))

# Indicadores por municipio -----------------------------------------------

df_pop_munic <- df_pop_2010 %>%
  group_by(cod_munic_7digitos) %>%
  reframe(
    pop_negra = 9,
    n = sum(peso)
  ) %>%
  bind_rows(
    df_pop_2010 %>%
      group_by(cod_munic_7digitos, pop_negra) %>%
      reframe(n = sum(peso))
  ) %>%
  pivot_wider(names_from = pop_negra, values_from = n) %>%
  mutate(
    prop_pop_negra = round((`1`/`9`)*100,2)
  ) %>%
  select(cod_munic_7digitos, prop_pop_negra) %>%
  left_join(
    df_pop_munic <- df_pop_2010 %>%
      group_by(cod_munic_7digitos) %>%
      reframe(
        pop_rural = 9,
        n = sum(peso)
      ) %>%
      bind_rows(
        df_pop_2010 %>%
          group_by(cod_munic_7digitos, pop_rural) %>%
          reframe(n = sum(peso))
      ) %>%
      pivot_wider(names_from = pop_rural, values_from = n) %>%
      mutate(
        prop_pop_rural = round((`1`/`9`)*100,2)
      ) %>%
      select(cod_munic_7digitos, prop_pop_rural),
    by = c("cod_munic_7digitos"),
    keep = FALSE
  ) %>%
  mutate(
    nivel_geografico = "Municipio",
    codigo_geografico = cod_munic_7digitos
  ) %>%
  select(-cod_munic_7digitos) %>%
  select(nivel_geografico, codigo_geografico, everything())

# Indicadores por UF ------------------------------------------------------

df_pop_uf <- df_pop_2010 %>%
  group_by(uf) %>%
  reframe(
    pop_negra = 9,
    n = sum(peso)
  ) %>%
  bind_rows(
    df_pop_2010 %>%
      group_by(uf, pop_negra) %>%
      reframe(n = sum(peso))
  ) %>%
  pivot_wider(names_from = pop_negra, values_from = n) %>%
  mutate(
    prop_pop_negra = round((`1`/`9`)*100,2)
  ) %>%
  select(uf, prop_pop_negra) %>%
  left_join(
    df_pop_munic <- df_pop_2010 %>%
      group_by(uf) %>%
      reframe(
        pop_rural = 9,
        n = sum(peso)
      ) %>%
      bind_rows(
        df_pop_2010 %>%
          group_by(uf, pop_rural) %>%
          reframe(n = sum(peso))
      ) %>%
      pivot_wider(names_from = pop_rural, values_from = n) %>%
      mutate(
        prop_pop_rural = round((`1`/`9`)*100,2)
      ) %>%
      select(uf, prop_pop_rural),
    by = c("uf"),
    keep = FALSE
  ) %>%
  mutate(
    nivel_geografico = "UF",
    codigo_geografico = uf
  ) %>%
  select(-uf) %>%
  select(nivel_geografico, codigo_geografico, everything())

# Indicadores para Brasil -------------------------------------------------

df_pop_br <- df_pop_2010 %>%
  reframe(
    pop_negra = 9,
    n = sum(peso)
  ) %>%
  bind_rows(
    df_pop_2010 %>%
      group_by(pop_negra) %>%
      reframe(n = sum(peso))
  ) %>%
  pivot_wider(names_from = pop_negra, values_from = n) %>%
  mutate(
    prop_pop_negra = round((`1`/`9`)*100,2)
  ) %>%
  select(prop_pop_negra) %>%
  bind_cols(
    df_pop_munic <- df_pop_2010 %>%
      reframe(
        pop_rural = 9,
        n = sum(peso)
      ) %>%
      bind_rows(
        df_pop_2010 %>%
          group_by(pop_rural) %>%
          reframe(n = sum(peso))
      ) %>%
      pivot_wider(names_from = pop_rural, values_from = n) %>%
      mutate(
        prop_pop_rural = round((`1`/`9`)*100,2)
      ) %>%
      select(prop_pop_rural)
  ) %>%
  mutate(
    nivel_geografico = "Brasil",
    codigo_geografico = 0
  ) %>%
  select(nivel_geografico, codigo_geografico, everything())


# Juncao das bases --------------------------------------------------------

df_pop_2010_indicadores <- df_pop_munic %>%
  bind_rows(df_pop_uf) %>%
  bind_rows(df_pop_br) %>%
  rename(
    indicador_pop_negra = prop_pop_negra,
    indicador_pop_rural = prop_pop_rural,
  )

# Exportacao dos dados ----------------------------------------------------

write_parquet(df_pop_2010_indicadores, sink = paste0("./output/base de dados - desigualdade/pop_2010_indicadores.parquet"))
write_csv2(df_pop_2010_indicadores, file = paste0("./output/base de dados - desigualdade/pop_2010_indicadores.csv"))
