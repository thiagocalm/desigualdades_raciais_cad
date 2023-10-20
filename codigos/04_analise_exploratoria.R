#' ------------------------------------------------------
#' @author Thiago Cordeiro Almeida
#' @last-update 2023-10-18
#' @description Análises exploratórias
#' -----------------------------------------------------
options(scipen = 9999999)
rm(list = ls())
gc()

# bibliotecas -------------------------------------------------------------

if(!require("pacman")) install.packages(("pacman"))
pacman::p_load(tidyverse, arrow)


# Importacao dos dados ----------------------------------------------------

# leitura dos dados
df_desigualdade <- read_parquet(file = paste0("./output/cad_indicadores_municipais.parquet"))

# Exploratória da distribuição --------------------------------------------

# Boxplot

df_desigualdade |>
  pivot_longer(cols = indicador_em:indicador_informalidade_brancos, names_to = "indicador", values_to = "valor") |>
  mutate(
    cor = str_sub(indicador, start= -5,-3),
    cor = as.factor(case_when(cor == "egr" ~ "Negros",cor == "anc" ~ "Brancos", TRUE ~ "Total"))
  ) |>
  mutate(
    indicador = str_sub(indicador, 11,12),
    indicador = as.factor(case_when(
      indicador == "em" ~ "Ensino Médio",
      indicador == "ac" ~ "Acesso ao Esgoto",
      indicador == "pb" ~ "PBF",
      indicador == "re" ~ "Renda de outras fontes",
      indicador == "ma" ~ "Mães adolescentes",
      indicador == "de" ~ "Desocupados",
      indicador == "in" ~ "Informalidade"
    ))) |>
  ggplot() +
  aes(x = as.factor(ano), y = valor, color = cor) +
  geom_boxplot() +
  facet_wrap(.~ indicador) +
  scale_color_brewer(palette = "Accent") +
  theme_light()

# Scatter plot

df_desigualdade |>
  pivot_longer(cols = indicador_em:indicador_informalidade_brancos, names_to = "indicador", values_to = "valor") |>
  mutate(
    cor = str_sub(indicador, start= -5,-3),
    cor = as.factor(case_when(cor == "egr" ~ "Negros",cor == "anc" ~ "Brancos", TRUE ~ "Total"))
  ) |>
  mutate(
    indicador = str_sub(indicador, 11,12),
    indicador = as.factor(case_when(
      indicador == "em" ~ "Ensino Médio",
      indicador == "ac" ~ "Acesso ao Esgoto",
      indicador == "pb" ~ "PBF",
      indicador == "re" ~ "Renda de outras fontes",
      indicador == "ma" ~ "Mães adolescentes",
      indicador == "de" ~ "Desocupados",
      indicador == "in" ~ "Informalidade"
    ))) |>
  filter(valor > 0 & valor < 100) |>
  pivot_wider(names_from = cor, values_from = valor) |>
  ggplot() +
  aes(x = Negros, y = Brancos, color = as.factor(ano)) +
  geom_point(alpha = .1) +
  geom_segment(aes(x = 0, xend = 100, y = 0, yend = 100,linetype = "dashed"), color = "red") +
  geom_smooth(method = loess, show.legend = FALSE) +
  facet_wrap(.~ indicador) +
  scale_color_brewer(palette = "PuBuGn") +
  theme_light()
