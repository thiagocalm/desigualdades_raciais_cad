#' ------------------------------------------------------
#' @author Thiago Cordeiro Almeida
#' @last-update 2023-12-07
#' @description Análises exploratórias dos dados de homicídio
#' -----------------------------------------------------
options(scipen = 9999999)
rm(list = ls())
gc()

# bibliotecas -------------------------------------------------------------

if(!require("pacman")) install.packages(("pacman"))
pacman::p_load(tidyverse, arrow, haven)

# Importacao dos dados ----------------------------------------------------

# leitura dos dados
df_homicidios <- read_sav(
  file = file.path("./output","base de dados - violencia","Homicidios.sav")
)


# Descritivas - Numeros absolutos -----------------------------------------

# Número de homicídios registrados

df_homicidios %>%
  summarise(absoluto = n(), .by = homicidio)

# Número de homicídios registrados - POR ANO

df_homicidios %>%
  reframe(absoluto = n(), .by = Ano) %>%
  mutate(relativo = 100*(absoluto/sum(absoluto)))

# Número de homicídios com RAÇA não registrada

df_homicidios %>%
  mutate(raca_registrada = case_when(racacor2 %in% 1:8 ~ 1, TRUE ~ 0)) %>%
  reframe(absoluto = n(), .by = raca_registrada) %>%
  mutate(relativo = 100*(absoluto/sum(absoluto)))

# Número de homicídios com RAÇA não registrada - POR ANO

df_homicidios %>%
  mutate(raca_registrada = case_when(racacor2 %in% 1:8 ~ 1, TRUE ~ 0)) %>%
  reframe(absoluto = n(), .by = c(raca_registrada, Ano)) %>%
  mutate(relativo = 100*(absoluto/sum(absoluto)), .by = c(Ano))

# Distribuição dos homicidios por sexo

df_homicidios %>%
  mutate(sexo = case_when(sexo %in% c("0","9") ~ "9", TRUE ~ sexo)) %>%
  reframe(absoluto = n(), .by = c(sexo)) %>%
  mutate(relativo = 100*(absoluto/sum(absoluto)))

# Distribuição dos homicidios por sexo - POR ANO

df_homicidios %>%
  mutate(sexo = case_when(sexo %in% c("0","9") ~ "9", TRUE ~ sexo)) %>%
  filter(sexo != "9") %>%
  reframe(absoluto = n(), .by = c(Ano, sexo)) %>%
  mutate(relativo = 100*(absoluto/sum(absoluto)), .by = c(Ano))

# Distribuição dos homicidios etária dos homicídios - POR SEXO E RAÇA

df_homicidios %>%
  mutate(
    sexo2 = case_when(sexo == "1" ~ "Masculino", sexo == "2" ~ "Feminino", TRUE ~ "9"),
    raca = case_when(racacor2 == 1 ~ "Branca", racacor2 == 2 ~ "Negra", TRUE ~ "9"),
    idade = case_when(
      idade2 %in% 0:4 ~ 1,
      idade2 %in% 5:9 ~ 2,
      idade2 %in% 10:14 ~ 3,
      idade2 %in% 15:19 ~ 4,
      idade2 %in% 20:24 ~ 5,
      idade2 %in% 25:29 ~ 6,
      idade2 %in% 30:34 ~ 7,
      idade2 %in% 35:39 ~ 8,
      idade2 %in% 40:44 ~ 9,
      idade2 %in% 45:49 ~ 10,
      idade2 %in% 50:54 ~ 11,
      idade2 %in% 55:59 ~ 12,
      idade2 %in% 60:64 ~ 13,
      idade2 %in% 65:69 ~ 14,
      idade2 %in% 70:74 ~ 15,
      idade2 %in% 75:79 ~ 16,
      is.na(idade2) ~ NA_real_,
      TRUE ~ 17
    )
  ) %>%
  filter(sexo2 != "9") %>%
  filter(raca != "9") %>%
  filter(!is.na(idade)) %>%
  mutate(
    idade = factor(
      idade,
      levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17),
      labels = seq(0,80,5)
    )
  ) %>%
  reframe(absoluto = n(), .by = c(sexo2, raca, idade)) %>%
  mutate(relativo = 100*(absoluto/sum(absoluto)), .by = c(sexo2, raca)) %>%
  bind_rows(
    df_homicidios %>%
      mutate(
        sexo2 = case_when(sexo == "1" ~ "Masculino", sexo == "2" ~ "Feminino", TRUE ~ "9"),
        raca = case_when(racacor2 == 1 ~ "Branca", racacor2 == 2 ~ "Negra", TRUE ~ "9"),
        idade = case_when(
          idade2 %in% 0:4 ~ 1,
          idade2 %in% 5:9 ~ 2,
          idade2 %in% 10:14 ~ 3,
          idade2 %in% 15:19 ~ 4,
          idade2 %in% 20:24 ~ 5,
          idade2 %in% 25:29 ~ 6,
          idade2 %in% 30:34 ~ 7,
          idade2 %in% 35:39 ~ 8,
          idade2 %in% 40:44 ~ 9,
          idade2 %in% 45:49 ~ 10,
          idade2 %in% 50:54 ~ 11,
          idade2 %in% 55:59 ~ 12,
          idade2 %in% 60:64 ~ 13,
          idade2 %in% 65:69 ~ 14,
          idade2 %in% 70:74 ~ 15,
          idade2 %in% 75:79 ~ 16,
          is.na(idade2) ~ NA_real_,
          TRUE ~ 17
        )
      ) %>%
      filter(sexo2 != "9") %>%
      filter(!is.na(idade)) %>%
      mutate(
        idade = factor(
          idade,
          levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17),
          labels = seq(0,80,5)
          )
      ) %>%
      reframe(absoluto = n(), .by = c(sexo2, idade)) %>%
      mutate(raca = "Total",relativo = 100*(absoluto/sum(absoluto)), .by = c(sexo2)) %>%
      select(sexo2, raca, idade, absoluto, relativo)
  ) %>%
  arrange(sexo2, raca,idade) %>%
  ggplot() +
  aes(x = ifelse(sexo2 == "Masculino", -relativo,relativo), y = idade, group = raca) +
  geom_line(aes(color = raca), linewidth = 1.05) +
  theme_light()

# Exploratória da distribuição --------------------------------------------

# Boxplot

df_homicidios |>
  pivot_longer(cols = indicador_em:indicador_informalidade_brancos, names_to = "indicador", values_to = "valor") |>
  mutate(
    cor = str_sub(indicador, start= -5,-3),
    cor = as.factor(case_when(cor == "egr" ~ "Negros",cor == "anc" ~ "Brancos", TRUE ~ "Total"))
  ) |>
  filter(valor > 0 & valor < 100) %>%
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
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  geom_smooth(method = loess, show.legend = FALSE) +
  facet_wrap(.~ indicador) +
  scale_color_brewer(palette = "PuBuGn") +
  theme_light()
