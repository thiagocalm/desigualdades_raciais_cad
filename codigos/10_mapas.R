#' ------------------------------------------------------
#' @author Thiago Cordeiro Almeida
#' @last-update 2023-11-17
#' @description Mapas
#' -----------------------------------------------------
options(scipen = 9999999)
rm(list = ls())
gc()

# bibliotecas -------------------------------------------------------------

if(!require("pacman")) install.packages(("pacman"))
pacman::p_load(tidyverse, arrow, openxlsx, geobr, sf)

# Importacao dos dados ----------------------------------------------------
DIR_top100 <- "./output/resultados"
DIR <- "./output/base de dados - violencia e desigualdade"
DIR_output <- "./output/resultados/mapas"

# leitura dos dados

df <- read_parquet(file = file.path(DIR, "base_violencia_desigualdade.parquet"))
#df_uf <- read_parquet(file = "./output/base de dados - desigualdade/cad_indicadores_uf.parquet")
#df_br <- read_parquet(file = "./output/base de dados - desigualdade/cad_indicadores_br.parquet")
df_pop_2010 <- read_parquet(file = "./output/base de dados - desigualdade/pop_2010_indicadores.parquet")
top100_df <- read_parquet(file = file.path(DIR_top100,"base de dados - Top 100 municipios violencia e desigualdade.parquet"))


# Importação dos shapes ---------------------------------------------------

shape_br <- read_country()
shape_uf <- read_state()
shape_munic <- read_municipality()
shape_munic_point <- read_municipal_seat()

# criando variavel de municipios com 6 digitos

shape_munic <- shape_munic %>%
  mutate(code_muni_6d = as.numeric(str_sub(code_muni, 1, 6))) %>%
  select(code_muni, code_muni_6d, everything())

shape_munic_point <- shape_munic_point %>%
  mutate(code_muni_6d = as.numeric(str_sub(code_muni, 1, 6))) %>%
  select(code_muni, code_muni_6d, everything())


# Tratamentos das bases ---------------------------------------------------

# Juncao dos dados de população negra e rural nos dados de violencia e desigualdade

df <- df %>%
  left_join(
    df_pop_2010 %>%
      filter(nivel_geografico == "Municipio") %>%
      mutate(
        cd_municipio_6digitos = as.numeric(substr(codigo_geografico, 1,6)),
        regiao = substr(codigo_geografico, 1,1)
      ) %>%
      select(-c(nivel_geografico, codigo_geografico)),
    by = c("regiao","cd_municipio_6digitos"),
    keep = FALSE
  )

# Juncao dos dados de violencia e desigualdade aos shapes de municipio

df_shape_munic <- shape_munic %>%
  left_join(
    df,
    by = c("code_state" = "uf", "code_muni_6d" = "cd_municipio_6digitos"),
    keep = FALSE
  )

df_shape_munic_point <- shape_munic_point %>%
  left_join(
    df,
    by = c("code_state" = "uf", "code_muni_6d" = "cd_municipio_6digitos"),
    keep = FALSE
  )

# Juncao dos dados de pontos somente para os municipios TOP 100

df_top100_shape_point <- shape_munic_point %>%
  left_join(
    top100_df %>% filter(nivel_geografico == "Municipio"),
    by = c("code_muni_6d" = "cd_municipio_6digitos"),
    keep = FALSE
  ) %>%
  filter(ranking %in% 1:100) %>%
  mutate(ranking_factor = case_when(
    ranking <= 10 ~ 1,
    ranking <= 20 ~ 2,
    ranking <= 30 ~ 3,
    ranking <= 40 ~ 4,
    ranking <= 50 ~ 5,
    ranking <= 60 ~ 6,
    ranking <= 70 ~ 7,
    ranking <= 80 ~ 8,
    ranking %in% 81:100 ~ 9
  ),
  ranking_factor = factor(
    ranking_factor,
    levels = seq(1,9,1L),
    labels = c("1 - 10","11 - 20","21 - 30","31 - 40","51 - 60","61 - 70","71 - 80","81 - 90","91 - 100")
  ))

# Elaboração de mapas -----------------------------------------------------

# 1 - Proporção de pessoas negras por municipio
df_shape_munic |>
  ggplot() +
  geom_sf(aes(fill = indicador_pop_negra),
          lwd = 0) +
  geom_sf(data = shape_uf,
          fill = "transparent",
          colour = "black", size = 0.5) +
  geom_sf(data = shape_br,
          fill = "transparent",
          colour = "black", size = 0.9) +
  scale_fill_viridis_c(option = 2, direction = -1) +
  # lemon::facet_rep_grid(.~sexo, repeat.tick.labels = TRUE) +
  guides(fill = guide_colourbar(title = "Proporção (x100) da população negra no município")) +
  labs(
    # title = "Razão entre o grau de cobertura dos nascidos-vivos 2010/2000, por UF e sexo.",
    caption = "Fonte: IBGE, Censo Demográfico 2010."
  ) +
  # tira sistema cartesiano
  theme(
    # plot.title = element_text(face = "bold", size = 12, hjust = 0),
    plot.caption = element_text(size = 8),
    legend.title = element_text(face = "bold", size = 9, hjust = 0, vjust = .5),
    legend.text = element_text(size = 8, hjust = 0, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(face = "bold", size = 8, color = "#636363", hjust = .5, vjust = .5),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 9, color = "#636363", hjust = .9, vjust = .5),
    panel.grid = element_line(color = "#f0f0f0",linewidth = .01),
    # panel.grid = element_line(colour = "grey"),
    panel.background = element_blank())

#'---------------------------------------------------------------------------------

# 2 - Proporção da população residente em área rural por municipio

df_shape_munic |>
  ggplot() +
  geom_sf(aes(fill = indicador_pop_rural),
          lwd = 0) +
  geom_sf(data = shape_uf,
          fill = "transparent",
          colour = "black", size = 0.5) +
  geom_sf(data = shape_br,
          fill = "transparent",
          colour = "black", size = 0.9) +
  scale_fill_viridis_c(option = 2, direction = -1) +
  # lemon::facet_rep_grid(.~sexo, repeat.tick.labels = TRUE) +
  guides(fill = guide_colourbar(title = "Proporção (x100) da população rural no município")) +
  labs(
    # title = "Razão entre o grau de cobertura dos nascidos-vivos 2010/2000, por UF e sexo.",
    caption = "Fonte: IBGE, Censo Demográfico 2010."
  ) +
  # tira sistema cartesiano
  theme(
    # plot.title = element_text(face = "bold", size = 12, hjust = 0),
    plot.caption = element_text(size = 8),
    legend.title = element_text(face = "bold", size = 9, hjust = 0, vjust = .5),
    legend.text = element_text(size = 8, hjust = 0, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(face = "bold", size = 8, color = "#636363", hjust = .5, vjust = .5),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 9, color = "#636363", hjust = .9, vjust = .5),
    panel.grid = element_line(color = "#f0f0f0",linewidth = .01),
    # panel.grid = element_line(colour = "grey"),
    panel.background = element_blank())

#'---------------------------------------------------------------------------------

# 3.1 - Top 100 municipios de desigualdade (sem gradiente de cores nos pontos)

df_shape_munic |>
  ggplot() +
  geom_sf(fill = "#f0f0f0",
          lwd = 0) +
  geom_sf(data = shape_uf,
          fill = "transparent",
          colour = "black", size = 0.5) +
  geom_sf(data = shape_br,
          fill = "transparent",
          colour = "black", size = 0.8) +
  geom_sf(data = df_top100_shape_point,
          fill = "#e6550d",
          alpha = .8,
          shape = 21) +
  # scale_fill_viridis_c(option = 2, direction = -1) +
  # lemon::facet_rep_grid(.~sexo, repeat.tick.labels = TRUE) +
  guides(fill = guide_colourbar(title = "Top 100 municípios no ranking de homicídios")) +
  labs(
    # title = "Razão entre o grau de cobertura dos nascidos-vivos 2010/2000, por UF e sexo.",
    caption = "Fonte: MS, Sistema de Informações sobre Mortalidade. IBGE, Censo Demográfico 2022."
  ) +
  # tira sistema cartesiano
  theme(
    # plot.title = element_text(face = "bold", size = 12, hjust = 0),
    plot.caption = element_text(size = 8),
    legend.title = element_text(face = "bold", size = 9, hjust = 0, vjust = .5),
    legend.text = element_text(size = 8, hjust = 0, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(face = "bold", size = 8, color = "#636363", hjust = .5, vjust = .5),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 9, color = "#636363", hjust = .9, vjust = .5),
    panel.grid = element_line(color = "#f0f0f0",linewidth = .01),
    # panel.grid = element_line(colour = "grey"),
    panel.background = element_blank()
  )

#'---------------------------------------------------------------------------------

# 3.2 - Top 100 municipios de desigualdade (com gradiente de cores nos pontos)

df_shape_munic |>
  ggplot() +
  geom_sf(fill = "#f0f0f0",
          lwd = 0) +
  geom_sf(data = shape_uf,
          fill = "transparent",
          colour = "black", size = 0.5) +
  geom_sf(data = shape_br,
          fill = "transparent",
          colour = "black", size = 0.8) +
  geom_sf(data = df_top100_shape_point,
          aes(fill = as.factor(ranking_factor)),
          alpha = .8,
          shape = 21,
          colour = "#f7f7f7") +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = guide_legend(title = "Top 100 municípios no ranking de homicídios")) +
  labs(
    # title = "Razão entre o grau de cobertura dos nascidos-vivos 2010/2000, por UF e sexo.",
    caption = "Fonte: MS, Sistema de Informações sobre Mortalidade. IBGE, Censo Demográfico 2022."
  ) +
  # tira sistema cartesiano
  theme(
    # plot.title = element_text(face = "bold", size = 12, hjust = 0),
    plot.caption = element_text(size = 8),
    legend.title = element_text(face = "bold", size = 9, hjust = 0, vjust = .5),
    legend.text = element_text(size = 8, hjust = 0, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(face = "bold", size = 8, color = "#636363", hjust = .5, vjust = .5),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 9, color = "#636363", hjust = .9, vjust = .5),
    panel.grid = element_line(color = "#f0f0f0",linewidth = .01),
    # panel.grid = element_line(colour = "grey"),
    panel.background = element_blank()
  )
# Inverter a ordem do factor, tirar fundo da legenda e mudar paleta de cores!

#'---------------------------------------------------------------------------------
## PAREI AQUIII!
# 4 - Top 100 municipios de desigualdade

df_shape_munic |>
  ggplot() +
  geom_sf(fill = "#f0f0f0",
          lwd = 0) +
  geom_sf(data = shape_uf,
          fill = "transparent",
          colour = "black", size = 0.5) +
  geom_sf(data = shape_br,
          fill = "transparent",
          colour = "black", size = 0.9) +
  geom_sf(data = df_top100_shape_point,
          fill = "#e6550d",
          alpha = .8,
          shape = 21) +
  # scale_fill_viridis_c(option = 2, direction = -1) +
  # lemon::facet_rep_grid(.~sexo, repeat.tick.labels = TRUE) +
  guides(fill = guide_colourbar(title = "Top 100 municípios no ranking de homicídios")) +
  labs(
    # title = "Razão entre o grau de cobertura dos nascidos-vivos 2010/2000, por UF e sexo.",
    caption = "Fonte: MS, Sistema de Informações sobre Mortalidade. IBGE, Censo Demográfico 2022."
  ) +
  # tira sistema cartesiano
  theme(
    # plot.title = element_text(face = "bold", size = 12, hjust = 0),
    plot.caption = element_text(size = 8),
    legend.title = element_text(face = "bold", size = 9, hjust = 0, vjust = .5),
    legend.text = element_text(size = 8, hjust = 0, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(face = "bold", size = 8, color = "#636363", hjust = .5, vjust = .5),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 9, color = "#636363", hjust = .9, vjust = .5),
    panel.grid = element_line(color = "#f0f0f0",linewidth = .01),
    # panel.grid = element_line(colour = "grey"),
    panel.background = element_blank()
  )
