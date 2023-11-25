#' ------------------------------------------------------
#' @author Thiago Cordeiro Almeida
#' @last-update 2023-11-25
#' @description Mapas
#' @update-description Mapas para o ranking ajustado para a pop. negra jovem em municipios com ao menos 5mil hab.
#' -----------------------------------------------------
options(scipen = 9999999)
rm(list = ls())
gc()

# bibliotecas -------------------------------------------------------------

if(!require("pacman")) install.packages(("pacman"))
pacman::p_load(tidyverse, arrow, openxlsx, geobr, sf, ggspatial, ggrepel)

# Importacao dos dados ----------------------------------------------------
DIR_top100 <- "./output/resultados"
DIR <- "./output/base de dados - violencia e desigualdade"
DIR_output <- file.path("./output/resultados/mapas","Top 100 ponderado pela pop. jovem negra em munic acima de 5mil hab")

# leitura dos dados

df <- read_parquet(file = file.path(DIR, "base_violencia_desigualdade.parquet"))
#df_uf <- read_parquet(file = "./output/base de dados - desigualdade/cad_indicadores_uf.parquet")
#df_br <- read_parquet(file = "./output/base de dados - desigualdade/cad_indicadores_br.parquet")
df_pop_2010 <- read_parquet(file = "./output/base de dados - desigualdade/pop_2010_indicadores.parquet")
top100_df <- read_parquet(file = file.path(
  DIR_top100,
  "tabelas complementares",
  "base de dados - Top 100 municipios violencia e desigualdade  - ajustado por pop jovem negra em munic acima de 5mil hab [2023.11.25].parquet"
))

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

df_total <- df %>%
  left_join(
    df_pop_2010 %>%
      filter(nivel_geografico == "Municipio") %>%
      mutate(
        cd_municipio_6digitos = as.numeric(substr(codigo_geografico, 1,6)),
        regiao = substr(codigo_geografico, 1,1)
      ) %>%
      select(-c(nivel_geografico, codigo_geografico, starts_with("prop"))),
    by = c("regiao","cd_municipio_6digitos"),
    keep = FALSE
  ) %>%
  group_by(regiao, uf, cd_municipio_6digitos, nome_municipio) %>%
  mutate_at(
    vars(c(indicador_em:indicador_pop_rural)),
    list(~replace(., is.na(.), 0))
  ) %>%
  reframe(
    across(c(indicador_em:indicador_pop_rural),
           ~mean(.x))
    )

# Juncao dos dados de violencia e desigualdade aos shapes de municipio

df_shape_munic <- shape_munic %>%
  left_join(
    df_total,
    by = c("code_state" = "uf", "code_muni_6d" = "cd_municipio_6digitos"),
    keep = FALSE
  ) %>%
  mutate_at(
    vars(regiao:indicador_pop_rural),
    list(~replace(., is.na(.), 0))
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

# Lidando com os casos de homicídio

df_homicidios <- df %>%
  group_by(ano, regiao, uf, cd_municipio_6digitos, nome_municipio) %>%
  mutate_at(
    vars(c(homicidios_brancos:indicador_intervencao_masc_maior_fem)),
    list(~replace(., is.na(.), 0))
  ) %>%
  select(-c(indicador_intervencao_masc_maior_fem, indicador_intervencao_negros_maior_brancos,
            indicador_intervencao_feminino, indicador_intervencao_masculino, intervencao_masculino,
            intervencao_feminino, indicador_homicidios_negros_maior_brancos, indicador_homicidios_masc_maior_fem,
            indicador_homicidios_feminino, indicador_homicidios_masculino, indicador_homicidios_brancos,
            indicador_homicidios_negros, homicidios_femininos, homicidios_masculinos, homicidios_geral,
            intervencao_geral, indicador_intervencao_negros, indicador_intervencao_brancos)) %>%
  select(homicidios_brancos:intervencao_negros) %>%
  ungroup() %>%
  pivot_longer(homicidios_brancos:intervencao_negros, names_to = "indicador", values_to = "value") %>%
  mutate(
    cor = str_sub(indicador, -3, -1),
    cor = case_when(cor == "cos" ~ "Brancos", TRUE ~ "Negros"),
    tipo = str_sub(indicador, 1, 4),
    tipo = case_when(tipo == "homi" ~ "Homicídio", TRUE ~ "Intervenção Legal")
  )

# Juncao dos dados de homicídio aos shapes de municipio

df_shape_munic_homicidio <- shape_munic %>%
  left_join(
    df_homicidios,
    by = c("code_state" = "uf", "code_muni_6d" = "cd_municipio_6digitos"),
    keep = FALSE
  ) %>%
  mutate_at(
    vars(ano:tipo),
    list(~replace(., is.na(.), 0))
  )

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
    guides(fill = guide_colourbar(title = "Proporção (x100) da população negra no município")) +
  labs(
    caption = "Fonte: IBGE, Censo Demográfico 2010."
  ) +
  # tira sistema cartesiano
  theme(
    plot.caption = element_text(size = 8),
    legend.title = element_text(face = "bold", size = 9, hjust = 0, vjust = .5),
    legend.text = element_text(size = 8, hjust = 0, vjust = .5),
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_line(color = "#f0f0f0",linewidth = .01),
    panel.background = element_blank()) +
  annotation_scale(
    location = "bl",
    pad_x = unit(0.0, "in"),
    width_hint = 0.2
  ) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
    style = north_arrow_fancy_orienteering
  )

ggsave(
  filename = "mapa_proporcao_populacao_negra_por_municipio.jpeg",
  device = "jpeg",
  path = DIR_output,
  width = 13,
  height = 13,
  units = "in"
)

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
  guides(fill = guide_colourbar(title = "Proporção (x100) da população rural no município")) +
  labs(
    caption = "Fonte: IBGE, Censo Demográfico 2010."
  ) +
  # tira sistema cartesiano
  theme(
    plot.caption = element_text(size = 8),
    legend.title = element_text(face = "bold", size = 9, hjust = 0, vjust = .5),
    legend.text = element_text(size = 8, hjust = 0, vjust = .5),
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_line(color = "#f0f0f0",linewidth = .01),
    panel.background = element_blank()) +
  annotation_scale(
    location = "bl",
    pad_x = unit(0.0, "in"),
    width_hint = 0.2
  ) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
    style = north_arrow_fancy_orienteering
  )

ggsave(
  filename = "mapa_proporcao_populacao_rural_por_municipio.jpeg",
  device = "jpeg",
  path = DIR_output,
  width = 13,
  height = 13,
  units = "in"
)

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
  guides(fill = guide_colourbar(title = "Top 100 municípios \n(ranking de homicídios de jovens negros)")) +
  labs(
    caption = "Fonte: MS, Sistema de Informações sobre Mortalidade. IBGE, Censo Demográfico 2022."
  ) +
  # tira sistema cartesiano
  theme(
    plot.caption = element_text(size = 8),
    legend.title = element_text(face = "bold", size = 9, hjust = 0, vjust = .5),
    legend.text = element_text(size = 8, hjust = 0, vjust = .5),
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_line(color = "#f0f0f0",linewidth = .01),
    panel.background = element_blank()
  )  +
  annotation_scale(
    location = "bl",
    pad_x = unit(0.0, "in"),
    width_hint = 0.2
  ) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
    style = north_arrow_fancy_orienteering
  )

ggsave(
  filename = "mapa_top100_municipio_pontos_sem_gradiente.jpeg",
  device = "jpeg",
  path = DIR_output,
  width = 13,
  height = 13,
  units = "in"
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
          alpha = .7,
          shape = 21,
          size = 2,
          colour = "#f7f7f7") +
  scale_fill_discrete(limits = rev(levels(as.factor(df_top100_shape_point$ranking_factor)))) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = guide_legend(title = "Top 100 municípios \n(ranking de homicídios de jovens negros)")) +
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
    legend.key = element_blank(),
    legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_line(color = "#f0f0f0",linewidth = .01),
    panel.background = element_blank()
  )  +
  annotation_scale(
    location = "bl",
    pad_x = unit(0.0, "in"),
    width_hint = 0.2
  ) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
    style = north_arrow_fancy_orienteering
  )

ggsave(
  filename = "mapa_top100_municipio_pontos_COM_gradiente.jpeg",
  device = "jpeg",
  path = DIR_output,
  width = 13,
  height = 13,
  units = "in"
)

#'---------------------------------------------------------------------------------
# 4 - Top 100 municipios de homicídio de jovens negros com poligono de proporção de negros

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
  geom_sf(data = df_top100_shape_point,
          fill = "#e6550d",
          alpha = .8,
          shape = 21) +
  scale_fill_viridis_c(direction = -1, alpha = .8) +
  guides(fill = guide_colourbar(title = "Proporção (x100) da população negra no município")) +
  labs(
    caption = "Fonte: MS, Sistema de Informações sobre Mortalidade. IBGE, Censo Demográfico 2010, 2022, MDS, Cadastro Único."
  ) +
  # tira sistema cartesiano
  theme(
    # plot.title = element_text(face = "bold", size = 12, hjust = 0),
    plot.caption = element_text(size = 8),
    legend.title = element_text(face = "bold", size = 9, hjust = 0, vjust = .5),
    legend.text = element_text(size = 8, hjust = 0, vjust = .5),
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_line(color = "#f0f0f0",linewidth = .01),
    panel.background = element_blank()) +
  annotation_scale(
    location = "bl",
    pad_x = unit(0.0, "in"),
    width_hint = 0.2
  ) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
    style = north_arrow_fancy_orienteering
  )

ggsave(
  filename = "mapa_top100_municipio_pontos_COM_gradiente_poligonos_com_prop_pop_negra.jpeg",
  device = "jpeg",
  path = DIR_output,
  width = 13,
  height = 13,
  units = "in"
)

#'---------------------------------------------------------------------------------
# 5 - Top 100 municipios de homicídio de jovens negros com poligono de proporção de população rural

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
  geom_sf(data = df_top100_shape_point,
          fill = "#e6550d",
          alpha = .8,
          shape = 21) +
  scale_fill_viridis_c(direction = -1, alpha = .8) +
  guides(fill = guide_colourbar(title = "Proporção (x100) da população rural no município")) +
  labs(
    caption = "Fonte: MS, Sistema de Informações sobre Mortalidade. IBGE, Censo Demográfico 2010, 2022, MDS, Cadastro Único."
  ) +
  # tira sistema cartesiano
  theme(
    # plot.title = element_text(face = "bold", size = 12, hjust = 0),
    plot.caption = element_text(size = 8),
    legend.title = element_text(face = "bold", size = 9, hjust = 0, vjust = .5),
    legend.text = element_text(size = 8, hjust = 0, vjust = .5),
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_line(color = "#f0f0f0",linewidth = .01),
    panel.background = element_blank()) +
  annotation_scale(
    location = "bl",
    pad_x = unit(0.0, "in"),
    width_hint = 0.2
  ) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
    style = north_arrow_fancy_orienteering
  )

ggsave(
  filename = "mapa_top100_municipio_pontos_COM_gradiente_poligonos_com_prop_pop_rural.jpeg",
  device = "jpeg",
  path = DIR_output,
  width = 13,
  height = 13,
  units = "in"
)

#'---------------------------------------------------------------------------------

# 6 - Casos de homicídio (absolutos) para 2013, 2021

df_shape_munic_homicidio |>
  filter(ano %in% c(2013, 2021)) %>%
  filter(tipo == "Homicídio") %>%
  filter(value > 0) %>%
  mutate(ano_fct = as.factor(ano)) %>%
  ggplot() +
  geom_sf(aes(fill = value),
          lwd = 0) +
  geom_sf(data = shape_uf,
          fill = "transparent",
          colour = "black", size = 0.5) +
  geom_sf(data = shape_br,
          fill = "transparent",
          colour = "black", size = 0.9) +
  lemon::facet_rep_grid(cor ~ ano_fct) +
  scale_fill_viridis_c(direction = -1) +
  guides(fill = guide_colorbar(title = "Número de homicídios no município")) +
  labs(
    caption = "Fonte: MS, Sistema de Informações sobre Mortalidade, 2013-2021."
  ) +
  # tira sistema cartesiano
  theme(
    plot.caption = element_text(size = 8),
    legend.title = element_text(face = "bold", size = 9, hjust = 0, vjust = .5),
    legend.text = element_text(size = 8, hjust = 0, vjust = .5),
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_line(color = "#f0f0f0",linewidth = .01),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 9, hjust = .9, vjust = .5)
  ) +
  annotation_scale(
    location = "bl",
    pad_x = unit(0.0, "in"),
    width_hint = 0.2
  ) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
    style = north_arrow_fancy_orienteering
  )

ggsave(
  filename = "mapa_homicidios_absoluto_2013_2017.jpeg",
  device = "jpeg",
  path = DIR_output,
  width = 13,
  height = 13,
  units = "in"
)

#'---------------------------------------------------------------------------------

# 7 - Casos de intervenção legal (absolutos) para 2013, 2021

df_shape_munic_homicidio |>
  filter(ano %in% c(2013, 2021)) %>%
  filter(tipo != "Homicídio") %>%
  filter(value > 0) %>%
  mutate(ano_fct = as.factor(ano)) %>%
  ggplot() +
  geom_sf(aes(fill = value),
          lwd = 0) +
  geom_sf(data = shape_uf,
          fill = "transparent",
          colour = "black", size = 0.5) +
  geom_sf(data = shape_br,
          fill = "transparent",
          colour = "black", size = 0.9) +
  lemon::facet_rep_grid(cor ~ ano_fct) +
  scale_fill_viridis_c(direction = -1) +
  guides(fill = guide_colorbar(title = "Número de Intervenções Legais no município")) +
  labs(
    caption = "Fonte: MS, Sistema de Informações sobre Mortalidade, 2013-2021."
  ) +
  # tira sistema cartesiano
  theme(
    plot.caption = element_text(size = 8),
    legend.title = element_text(face = "bold", size = 9, hjust = 0, vjust = .5),
    legend.text = element_text(size = 8, hjust = 0, vjust = .5),
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_line(color = "#f0f0f0",linewidth = .01),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 9, hjust = .9, vjust = .5)
  ) +
  annotation_scale(
    location = "bl",
    pad_x = unit(0.0, "in"),
    width_hint = 0.2
  ) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
    style = north_arrow_fancy_orienteering
  )

ggsave(
  filename = "mapa_intervencoes_legais_absoluto_2013_2017.jpeg",
  device = "jpeg",
  path = DIR_output,
  width = 13,
  height = 13,
  units = "in"
)
