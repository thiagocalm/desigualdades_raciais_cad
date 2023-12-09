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
pacman::p_load(tidyverse, arrow, haven, grid)

# Importacao dos dados ----------------------------------------------------

# leitura dos dados
df_homicidios <- read_sav(
  file = file.path("./output","base de dados - violencia","Homicidios.sav")
)

# Descritivas - homicidios -----------------------------------------

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

# Número de homicídios - POR RACA

df_homicidios %>%
  mutate(raca = case_when(racacor2 == 1 ~ "Branca", racacor2 == 2 ~ "Negra", TRUE ~ "9")) %>%
  filter(raca != "9") %>%
  reframe(absoluto = n(), .by = raca) %>%
  mutate(relativo = (absoluto/sum(absoluto)*100))

# Número de homicídios registrados - POR RACA E ANO

df_homicidios %>%
  mutate(raca = case_when(racacor2 == 1 ~ "Branca", racacor2 == 2 ~ "Negra", TRUE ~ "9")) %>%
  filter(raca != "9") %>%
  reframe(absoluto = n(), .by = c(Ano, raca)) %>%
  mutate(relativo = (absoluto/sum(absoluto)*100), .by = Ano) %>%
  arrange(raca)

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

# Distribuição etária dos homicídios - POR SEXO E RAÇA

df_homicidios %>%
  mutate(
    sexo2 = case_when(sexo == "1" ~ "Masculino", sexo == "2" ~ "Feminino", TRUE ~ "9"),
    raca = case_when(racacor2 == 1 ~ "Branca", racacor2 == 2 ~ "Negra", TRUE ~ "9"),
    idade = as.factor(case_when(
      idade2 %in% 0:4 ~ "0-4",
      idade2 %in% 5:9 ~ "5-9",
      idade2 %in% 10:14 ~ "10-14",
      idade2 %in% 15:19 ~ "15-19",
      idade2 %in% 20:24 ~ "20-24",
      idade2 %in% 25:29 ~ "25-29",
      idade2 %in% 30:34 ~ "30-34",
      idade2 %in% 35:39 ~ "35-39",
      idade2 %in% 40:44 ~ "40-44",
      idade2 %in% 45:49 ~ "45-49",
      idade2 %in% 50:54 ~ "50-54",
      idade2 %in% 55:59 ~ "55-59",
      idade2 %in% 60:64 ~ "60-64",
      idade2 %in% 65:69 ~ "65-69",
      idade2 %in% 70:74 ~ "70-74",
      idade2 %in% 75:79 ~ "75-79",
      is.na(idade2) ~ NA_character_,
      TRUE ~ "80+"
    ))
  ) %>%
  filter(sexo2 != "9") %>%
  filter(raca != "9") %>%
  filter(!is.na(idade)) %>%
  mutate(
    idade = factor(
      idade,
      levels = c(
        "0-4","5-9","10-14",
        "15-19","20-24","25-29",
        "30-34","35-39","40-44",
        "45-49","50-54","55-59",
        "60-64","65-69","70-74",
        "75-79","80+")
    )) %>%
  reframe(absoluto = n(), .by = c(sexo2, raca, idade)) %>%
  mutate(relativo = 100*(absoluto/sum(absoluto)), .by = c(sexo2, raca)) %>%
  arrange(sexo2, raca,idade) %>%
  ggplot() +
  aes(x = ifelse(sexo2 == "Masculino", -relativo,relativo), y = idade) +
  geom_col(aes(fill = raca), position = position_dodge()) +
  geom_vline(xintercept = 0, linewidth = .8, color = "black") +
  scale_fill_brewer(palette = "Accent") +
  coord_cartesian(xlim = c(-25,25)) +
  annotate("text", x=-15, y="60-64", label= "Masculino",color = "grey") +
  annotate("text", x=15, y="60-64", label= "Feminino",color = "grey") +
  theme_light() +
  labs(
    y = "Grupos etários (quinquenais)"
  ) +
  theme(
    legend.title = element_blank(),
    axis.title.y = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )

# Distribuição dos homicídios entre jovens e restante - GERAL

df_homicidios %>%
  mutate(
    raca = case_when(racacor2 == 1 ~ "Branca", racacor2 == 2 ~ "Negra", TRUE ~ "9"),
    jovem = case_when(idade2 %in% 15:29 ~ "Jovem", is.na(idade2) ~ NA_character_, TRUE ~ "Restante")
  ) %>%
  filter(!is.na(jovem)) %>%
  reframe(absoluto = n(), .by = c(jovem)) %>%
  mutate(relativo = 100*(absoluto/sum(absoluto)))

# Distribuição dos homicídios entre jovens e restante - POR ANO

df_homicidios %>%
  mutate(
    raca = case_when(racacor2 == 1 ~ "Branca", racacor2 == 2 ~ "Negra", TRUE ~ "9"),
    jovem = case_when(idade2 %in% 15:29 ~ "Jovem", is.na(idade2) ~ NA_character_, TRUE ~ "Restante")
  ) %>%
  filter(!is.na(jovem)) %>%
  reframe(absoluto = n(), .by = c(Ano,jovem)) %>%
  mutate(relativo = 100*(absoluto/sum(absoluto)), .by = Ano) %>%
  arrange(jovem, Ano)

# Distribuição dos homicídios entre jovens e restante - POR RAÇA

df_homicidios %>%
  mutate(
    raca = case_when(racacor2 == 1 ~ "Branca", racacor2 == 2 ~ "Negra", TRUE ~ "9"),
    jovem = case_when(idade2 %in% 15:29 ~ "Jovem", is.na(idade2) ~ NA_character_, TRUE ~ "Restante")
  ) %>%
  filter(!is.na(jovem)) %>%
  filter(raca != "9") %>%
  reframe(absoluto = n(), .by = c(raca,jovem)) %>%
  mutate(relativo = 100*(absoluto/sum(absoluto)), .by = c(raca))

# Distribuição dos homicídios entre jovens e restante - POR RAÇA

df_homicidios %>%
  mutate(
    raca = case_when(racacor2 == 1 ~ "Branca", racacor2 == 2 ~ "Negra", TRUE ~ "9"),
    jovem = case_when(idade2 %in% 15:29 ~ "Jovem", is.na(idade2) ~ NA_character_, TRUE ~ "Restante")
  ) %>%
  filter(!is.na(jovem)) %>%
  filter(raca != "9") %>%
  reframe(absoluto = n(), .by = c(raca,jovem, Ano)) %>%
  mutate(relativo = 100*(absoluto/sum(absoluto)), .by = c(Ano, raca)) %>%
  arrange(raca, jovem, Ano) %>%
  filter(jovem == "Jovem") %>%
  print(n = 20)

# Distribuição dos homicídios entre jovens e restante - POR RAÇA E SEXO

df_homicidios %>%
  mutate(
    raca = case_when(racacor2 == 1 ~ "Branca", racacor2 == 2 ~ "Negra", TRUE ~ "9"),
    sexo2 = case_when(sexo == "1" ~ "Masculino", sexo == "2" ~ "Feminino", TRUE ~ "9"),
    jovem = case_when(idade2 %in% 15:29 ~ "Jovem", is.na(idade2) ~ NA_character_, TRUE ~ "Restante")
  ) %>%
  filter(!is.na(jovem)) %>%
  filter(raca != "9") %>%
  filter(sexo2 != "9") %>%
  reframe(absoluto = n(), .by = c(raca,jovem,sexo2, Ano)) %>%
  arrange(raca, jovem, sexo2, Ano)

# Descritivas - intervencioes -----------------------------------------

# Número de homicídios registrados

df_homicidios %>%
  filter(InterLegal == 1) %>%
  summarise(absoluto = n(), .by = homicidio)

# Número de homicídios registrados - POR ANO

df_homicidios %>%
  filter(InterLegal == 1) %>%
  reframe(absoluto = n(), .by = Ano) %>%
  mutate(relativo = 100*(absoluto/sum(absoluto)))

# Número de homicídios registrados - POR TIPO

df_homicidios %>%
  reframe(absoluto = n(), .by = InterLegal) %>%
  mutate(relativo = 100*(absoluto/sum(absoluto)))

# Número de homicídios com RAÇA não registrada

df_homicidios %>%
  filter(InterLegal == 1) %>%
  mutate(raca_registrada = case_when(racacor2 %in% 1:8 ~ 1, TRUE ~ 0)) %>%
  reframe(absoluto = n(), .by = raca_registrada) %>%
  mutate(relativo = 100*(absoluto/sum(absoluto)))

# Número de homicídios com RAÇA não registrada - POR ANO

df_homicidios %>%
  filter(InterLegal == 1) %>%
  mutate(raca_registrada = case_when(racacor2 %in% 1:8 ~ 1, TRUE ~ 0)) %>%
  reframe(absoluto = n(), .by = c(raca_registrada, Ano)) %>%
  mutate(relativo = 100*(absoluto/sum(absoluto)), .by = c(Ano))

# Número de homicídios - POR RACA

df_homicidios %>%
  filter(InterLegal == 1) %>%
  mutate(raca = case_when(racacor2 == 1 ~ "Branca", racacor2 == 2 ~ "Negra", TRUE ~ "9")) %>%
  filter(raca != "9") %>%
  reframe(absoluto = n(), .by = raca) %>%
  mutate(relativo = (absoluto/sum(absoluto)*100))

# Número de homicídios registrados - POR RACA E ANO

df_homicidios %>%
  filter(InterLegal == 1) %>%
  mutate(raca = case_when(racacor2 == 1 ~ "Branca", racacor2 == 2 ~ "Negra", TRUE ~ "9")) %>%
  filter(raca != "9") %>%
  reframe(absoluto = n(), .by = c(Ano, raca)) %>%
  mutate(relativo = (absoluto/sum(absoluto)*100), .by = Ano) %>%
  arrange(raca)

# Distribuição dos homicidios por sexo

df_homicidios %>%
  filter(InterLegal == 1) %>%
  mutate(sexo = case_when(sexo %in% c("0","9") ~ "9", TRUE ~ sexo)) %>%
  reframe(absoluto = n(), .by = c(sexo)) %>%
  mutate(relativo = 100*(absoluto/sum(absoluto)))

# Distribuição dos homicidios por sexo - POR ANO

df_homicidios %>%
  filter(InterLegal == 1) %>%
  mutate(sexo = case_when(sexo %in% c("0","9") ~ "9", TRUE ~ sexo)) %>%
  filter(sexo != "9") %>%
  reframe(absoluto = n(), .by = c(Ano, sexo)) %>%
  mutate(relativo = 100*(absoluto/sum(absoluto)), .by = c(Ano))

# Distribuição etária dos homicídios - POR SEXO E RAÇA

df_homicidios %>%
  filter(InterLegal == 1) %>%
  mutate(
    sexo2 = case_when(sexo == "1" ~ "Masculino", sexo == "2" ~ "Feminino", TRUE ~ "9"),
    raca = case_when(racacor2 == 1 ~ "Branca", racacor2 == 2 ~ "Negra", TRUE ~ "9"),
    idade = as.factor(case_when(
      idade2 %in% 0:4 ~ "0-4",
      idade2 %in% 5:9 ~ "5-9",
      idade2 %in% 10:14 ~ "10-14",
      idade2 %in% 15:19 ~ "15-19",
      idade2 %in% 20:24 ~ "20-24",
      idade2 %in% 25:29 ~ "25-29",
      idade2 %in% 30:34 ~ "30-34",
      idade2 %in% 35:39 ~ "35-39",
      idade2 %in% 40:44 ~ "40-44",
      idade2 %in% 45:49 ~ "45-49",
      idade2 %in% 50:54 ~ "50-54",
      idade2 %in% 55:59 ~ "55-59",
      idade2 %in% 60:64 ~ "60-64",
      idade2 %in% 65:69 ~ "65-69",
      idade2 %in% 70:74 ~ "70-74",
      idade2 %in% 75:79 ~ "75-79",
      is.na(idade2) ~ NA_character_,
      TRUE ~ "80+"
    ))
  ) %>%
  filter(sexo2 != "9") %>%
  filter(raca != "9") %>%
  filter(!is.na(idade)) %>%
  mutate(
    idade = factor(
      idade,
      levels = c(
        "0-4","5-9","10-14",
        "15-19","20-24","25-29",
        "30-34","35-39","40-44",
        "45-49","50-54","55-59",
        "60-64","65-69","70-74",
        "75-79","80+")
    )) %>%
  reframe(absoluto = n(), .by = c(sexo2, raca, idade)) %>%
  mutate(relativo = 100*(absoluto/sum(absoluto)), .by = c(sexo2, raca)) %>%
  arrange(sexo2, raca,idade) %>%
  ggplot() +
  aes(x = ifelse(sexo2 == "Masculino", -absoluto,absoluto), y = idade) +
  geom_col(aes(fill = raca), position = position_dodge()) +
  geom_vline(xintercept = 0, linewidth = .8, color = "black") +
  scale_fill_brewer(palette = "Accent") +
  coord_cartesian(xlim = c(-3500,3500)) +
  scale_x_continuous(breaks = seq(-3500,3500,500)) +
  annotate("text", x=-2000, y="60-64", label= "Masculino",color = "grey") +
  annotate("text", x=2000, y="60-64", label= "Feminino",color = "grey") +
  theme_light() +
  labs(
    y = "Grupos etários (quinquenais)",
    x = "Número (absoluto) de homicídios por intervenção legal"
  ) +
  theme(
    legend.title = element_blank(),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12)
  )

# Distribuição dos homicídios entre jovens e restante - GERAL

df_homicidios %>%
  filter(InterLegal == 1) %>%
  mutate(
    raca = case_when(racacor2 == 1 ~ "Branca", racacor2 == 2 ~ "Negra", TRUE ~ "9"),
    jovem = case_when(idade2 %in% 15:29 ~ "Jovem", is.na(idade2) ~ NA_character_, TRUE ~ "Restante")
  ) %>%
  filter(!is.na(jovem)) %>%
  reframe(absoluto = n(), .by = c(jovem)) %>%
  mutate(relativo = 100*(absoluto/sum(absoluto)))

# Distribuição dos homicídios entre jovens e restante - POR RAÇA

df_homicidios %>%
  filter(InterLegal == 1) %>%
  mutate(
    raca = case_when(racacor2 == 1 ~ "Branca", racacor2 == 2 ~ "Negra", TRUE ~ "9"),
    jovem = case_when(idade2 %in% 15:29 ~ "Jovem", is.na(idade2) ~ NA_character_, TRUE ~ "Restante")
  ) %>%
  filter(!is.na(jovem)) %>%
  filter(raca != "9") %>%
  reframe(absoluto = n(), .by = c(raca,jovem)) %>%
  mutate(relativo = 100*(absoluto/sum(absoluto)), .by = c(raca))

# Distribuição dos homicídios entre jovens e restante - POR RAÇA

df_homicidios %>%
  filter(InterLegal == 1) %>%
  mutate(
    raca = case_when(racacor2 == 1 ~ "Branca", racacor2 == 2 ~ "Negra", TRUE ~ "9"),
    jovem = case_when(idade2 %in% 15:29 ~ "Jovem", is.na(idade2) ~ NA_character_, TRUE ~ "Restante")
  ) %>%
  filter(!is.na(jovem)) %>%
  filter(raca != "9") %>%
  reframe(absoluto = n(), .by = c(raca,jovem, Ano)) %>%
  mutate(relativo = 100*(absoluto/sum(absoluto)), .by = c(Ano, raca)) %>%
  arrange(raca, jovem, Ano) %>%
  filter(jovem == "Jovem") %>%
  print(n = 20)

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
