#' ------------------------------------------------------
#' @author Thiago Cordeiro Almeida
#' @last-update 2023-10-24
#' @description Análises exploratórias dos dados completos
#' -----------------------------------------------------
options(scipen = 9999999)
rm(list = ls())
gc()

# bibliotecas -------------------------------------------------------------

if(!require("pacman")) install.packages(("pacman"))
pacman::p_load(tidyverse, arrow, openxlsx)

# Importacao dos dados ----------------------------------------------------
DIR <- "./output/base de dados - violencia e desigualdade"

# leitura dos dados
df <- read_parquet(file = file.path(DIR, "base_violencia_desigualdade.parquet"))
df_uf <- read_parquet(file = "./output/base de dados - desigualdade/cad_indicadores_uf.parquet")

# Definicao da base de trabalho -------------------------------------------

cods_uf <- tibble(cod = c("11","12","13","14","15","16","17","21","22","23","24","25","26","27",
                          "28","29","31","32","33","35","41","42","43","50","51","52","53"),
                  names = c("Rondônia","Acre","Amazonas", "Roraima","Pará","Amapá","Tocantins","Maranhão",
                            "Piauí", "Ceará","Rio Grande do Norte","Paraíba","Pernambuco","Alagoas","Sergipe",
                            "Bahia","Minas Gerais","Espírito Santo","Rio de Janeiro","São Paulo","Paraná","Santa Catarina",
                            "Rio Grande do Sul","Mato Grosso do Sul","Mato Grosso","Goiás","Distrito Federal"))

# Nota: vamos trabalhar somente com os municipios com populacao acima de 5000 habitantes
df_analises <- df |>
  filter(pop_faixas >= 2) |>
  mutate(uf = factor(uf, levels = cods_uf$cod, labels = cods_uf$names))

df_uf <- df_uf |>
  mutate(uf = factor(uf, levels = cods_uf$cod, labels = cods_uf$names))


# Exploratoria de desigualdade ------------------------------------------------------------

# Boxplot

df |>
  filter(pop_faixas >= 2) |>
  select(1:5,9:29) |>
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

df |>
  filter(pop_faixas >= 2) |>
  select(1:5,9:29) |>
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


# Exploratoria de violencia -----------------------------------------------

# Boxplot

df |>
  filter(pop_faixas >= 2) |>
  select(1:5,35:38,46:49) |>
  pivot_longer(cols = indicador_homicidios_brancos:indicador_intervencao_feminino, names_to = "indicador", values_to = "valor") |>
  mutate(
    cor_sexo = str_sub(indicador, start= -5,-3),
    cor_sexo = as.factor(case_when(
      cor_sexo == "egr" ~ "Negros",
      cor_sexo == "anc" ~ "Brancos",
      cor_sexo == "ini" ~ "Feminino",
      cor_sexo == "uli" ~ "Masculino",
      TRUE ~ "Total"
    ))
  ) |>
  mutate(
    indicador = str_sub(indicador, 11,12),
    indicador = as.factor(case_when(
      indicador == "ho" ~ "Homicidios",
      indicador == "in" ~ "Intervenção Legal"
    ))) |>
  ggplot() +
  aes(x = as.factor(ano), y = valor, color = cor_sexo) +
  geom_boxplot() +
  facet_wrap(.~ indicador) +
  scale_color_brewer(palette = "Dark2") +
  theme_light()

# Scatter plot

df |>
  filter(pop_faixas >= 2) |>
  select(1:5,35:38,46:49) |>
  pivot_longer(cols = indicador_homicidios_brancos:indicador_intervencao_feminino, names_to = "indicador", values_to = "valor") |>
  mutate(
    cor_sexo = str_sub(indicador, start= -5,-3),
    cor_sexo = as.factor(case_when(
      cor_sexo == "egr" ~ "Negros",
      cor_sexo == "anc" ~ "Brancos",
      cor_sexo == "ini" ~ "Feminino",
      cor_sexo == "uli" ~ "Masculino",
      TRUE ~ "Total"
    ))
  ) |>
  mutate(
    indicador = str_sub(indicador, 11,12),
    indicador = as.factor(case_when(
      indicador == "ho" ~ "Homicidios",
      indicador == "in" ~ "Intervenção Legal"
    ))) |>
  filter(valor > 0 & valor < 100) |>
  filter(cor_sexo %in% c("Negros", "Masculino")) |>
  pivot_wider(names_from = cor_sexo, values_from = valor) |>
  ggplot() +
  aes(x = Negros, y = Masculino, color = as.factor(ano)) +
  geom_point(alpha = .1) +
  geom_segment(aes(x = 0, xend = 100, y = 0, yend = 100,linetype = "dashed"), color = "red") +
  geom_smooth(method = loess, show.legend = FALSE) +
  facet_wrap(.~ indicador) +
  scale_color_brewer(palette = "PuBuGn") +
  theme_light()


# Analises do ranking - desigualdade ---------------------------------------

#'--------------------------------------------------------------------
# 1 - Ensino médio

# Brasil como um todo

t1_br <- df_analises |>
  filter(ano %in% 2013:2018) |>
  mutate(razao = indicador_em_negros/indicador_em_brancos) |>
  select(ano, cd_municipio_6digitos, nome_municipio, indicador_em_negros, indicador_em_brancos, razao) |>
  filter(!is.infinite(razao)) |>
  arrange(desc(razao)) |>
  group_by(ano) |>
  slice(1:10) |>
  pivot_wider(names_from = ano, values_from = c(indicador_em_negros, indicador_em_brancos, razao)) |>
  select(cd_municipio_6digitos, nome_municipio, ends_with("_2013"),ends_with("_2014"),
         ends_with("_2015"),ends_with("_2016"),ends_with("_2017"),ends_with("_2018"))

# Por UF

t1_uf <- df_uf |>
  filter(ano %in% 2013:2018) |>
  mutate(razao = indicador_em_negros/indicador_em_brancos) |>
  select(ano, uf, indicador_em_negros, indicador_em_brancos, razao) |>
  filter(!is.infinite(razao)) |>
  arrange(desc(razao)) |>
  group_by(ano,uf) |>
  slice(1:10) |>
  pivot_wider(names_from = ano, values_from = c(indicador_em_negros, indicador_em_brancos, razao)) |>
  select(uf, ends_with("_2013"),ends_with("_2014"),
         ends_with("_2015"),ends_with("_2016"),ends_with("_2017"),ends_with("_2018"))

# Por UF e munic

t1_uf_munic <- df_analises |>
  filter(ano %in% 2013:2018) |>
  mutate(razao = indicador_em_negros/indicador_em_brancos) |>
  select(ano, uf, cd_municipio_6digitos, nome_municipio, indicador_em_negros, indicador_em_brancos, razao) |>
  filter(!is.infinite(razao)) |>
  arrange(desc(razao)) |>
  group_by(ano,uf) |>
  slice(1:10) |>
  pivot_wider(names_from = ano, values_from = c(indicador_em_negros, indicador_em_brancos, razao)) |>
  select(uf, cd_municipio_6digitos, nome_municipio, ends_with("_2013"),ends_with("_2014"),
         ends_with("_2015"),ends_with("_2016"),ends_with("_2017"),ends_with("_2018"))

#'--------------------------------------------------------------------
# 2 - Acesso ao esgoto

# Brasil como um todo

t2_br <- df_analises |>
  filter(ano %in% 2013:2018) |>
  mutate(razao = indicador_acesso_esgoto_negros/indicador_acesso_esgoto_brancos) |>
  select(ano, cd_municipio_6digitos, nome_municipio, indicador_acesso_esgoto_negros,indicador_acesso_esgoto_brancos, razao) |>
  filter(!is.infinite(razao)) |>
  arrange(desc(razao)) |>
  group_by(ano) |>
  slice(1:10) |>
  pivot_wider(names_from = ano, values_from = c(indicador_acesso_esgoto_negros,indicador_acesso_esgoto_brancos, razao)) |>
  select(cd_municipio_6digitos, nome_municipio, ends_with("_2013"),ends_with("_2014"),
         ends_with("_2015"),ends_with("_2016"),ends_with("_2017"),ends_with("_2018"))

# Por UF

t2_uf <- df_uf |>
  filter(ano %in% 2013:2018) |>
  mutate(razao = indicador_acesso_esgoto_negros/indicador_acesso_esgoto_brancos) |>
  select(ano, uf, indicador_acesso_esgoto_negros,indicador_acesso_esgoto_brancos, razao) |>
  filter(!is.infinite(razao)) |>
  arrange(desc(razao)) |>
  group_by(ano, uf) |>
  slice(1:10) |>
  pivot_wider(names_from = ano, values_from = c(indicador_acesso_esgoto_negros,indicador_acesso_esgoto_brancos, razao)) |>
  select(uf, ends_with("_2013"),ends_with("_2014"),
         ends_with("_2015"),ends_with("_2016"),ends_with("_2017"),ends_with("_2018"))

# Por UF e munic

t2_uf_munic <- df_analises |>
  filter(ano %in% 2013:2018) |>
  mutate(razao = indicador_acesso_esgoto_negros/indicador_acesso_esgoto_brancos) |>
  select(ano, uf, cd_municipio_6digitos, nome_municipio, indicador_acesso_esgoto_negros,indicador_acesso_esgoto_brancos, razao) |>
  filter(!is.infinite(razao)) |>
  arrange(desc(razao)) |>
  group_by(ano, uf) |>
  slice(1:10) |>
  pivot_wider(names_from = ano, values_from = c(indicador_acesso_esgoto_negros,indicador_acesso_esgoto_brancos, razao)) |>
  select(uf, cd_municipio_6digitos, nome_municipio, ends_with("_2013"),ends_with("_2014"),
         ends_with("_2015"),ends_with("_2016"),ends_with("_2017"),ends_with("_2018"))

#'--------------------------------------------------------------------
# 3 - PBF

# Brasil como um todo

t3_br <- df_analises |>
  filter(ano %in% 2013:2018) |>
  mutate(razao = indicador_pbf_negros/indicador_pbf_brancos) |>
  select(ano, cd_municipio_6digitos, nome_municipio, indicador_pbf_negros,indicador_pbf_brancos, razao) |>
  filter(!is.infinite(razao)) |>
  arrange(desc(razao)) |>
  group_by(ano) |>
  slice(1:10) |>
  pivot_wider(names_from = ano, values_from = c(indicador_pbf_negros,indicador_pbf_brancos, razao)) |>
  select(cd_municipio_6digitos, nome_municipio, ends_with("_2013"),ends_with("_2014"),
         ends_with("_2015"),ends_with("_2016"),ends_with("_2017"),ends_with("_2018"))

# Por UF

t3_uf <- df_uf |>
  filter(ano %in% 2013:2018) |>
  mutate(razao = indicador_pbf_negros/indicador_pbf_brancos) |>
  select(ano, uf, indicador_pbf_negros,indicador_pbf_brancos, razao) |>
  filter(!is.infinite(razao)) |>
  arrange(desc(razao)) |>
  group_by(ano, uf) |>
  slice(1:10) |>
  pivot_wider(names_from = ano, values_from = c(indicador_pbf_negros,indicador_pbf_brancos, razao)) |>
  select(uf, ends_with("_2013"),ends_with("_2014"),
         ends_with("_2015"),ends_with("_2016"),ends_with("_2017"),ends_with("_2018"))

# Por UF e munic

t3_uf_munic <- df_analises |>
  filter(ano %in% 2013:2018) |>
  mutate(razao = indicador_pbf_negros/indicador_pbf_brancos) |>
  select(ano, uf, cd_municipio_6digitos, nome_municipio, indicador_pbf_negros,indicador_pbf_brancos, razao) |>
  filter(!is.infinite(razao)) |>
  arrange(desc(razao)) |>
  group_by(ano, uf) |>
  slice(1:10) |>
  pivot_wider(names_from = ano, values_from = c(indicador_pbf_negros,indicador_pbf_brancos, razao)) |>
  select(uf, cd_municipio_6digitos, nome_municipio, ends_with("_2013"),ends_with("_2014"),
         ends_with("_2015"),ends_with("_2016"),ends_with("_2017"),ends_with("_2018"))

#'--------------------------------------------------------------------
# 4 - Renda de outras fontes

# Brasil como um todo

t4_br <- df_analises |>
  filter(ano %in% 2013:2018) |>
  mutate(razao = indicador_renda_outras_fontes_negros/indicador_renda_outras_fontes_brancos) |>
  select(ano, cd_municipio_6digitos, nome_municipio, indicador_renda_outras_fontes_negros,indicador_renda_outras_fontes_brancos, razao) |>
  filter(!is.infinite(razao)) |>
  arrange(desc(razao)) |>
  group_by(ano) |>
  slice(1:10) |>
  pivot_wider(names_from = ano, values_from = c(indicador_renda_outras_fontes_negros,indicador_renda_outras_fontes_brancos, razao)) |>
  select(cd_municipio_6digitos, nome_municipio, ends_with("_2013"),ends_with("_2014"),
         ends_with("_2015"),ends_with("_2016"),ends_with("_2017"),ends_with("_2018"))

# Por UF

t4_uf <- df_uf |>
  filter(ano %in% 2013:2018) |>
  mutate(razao = indicador_renda_outras_fontes_negros/indicador_renda_outras_fontes_brancos) |>
  select(ano, uf, indicador_renda_outras_fontes_negros,indicador_renda_outras_fontes_brancos, razao) |>
  filter(!is.infinite(razao)) |>
  arrange(desc(razao)) |>
  group_by(ano, uf) |>
  slice(1:10) |>
  pivot_wider(names_from = ano, values_from = c(indicador_renda_outras_fontes_negros,indicador_renda_outras_fontes_brancos, razao)) |>
  select(uf, ends_with("_2013"),ends_with("_2014"),
         ends_with("_2015"),ends_with("_2016"),ends_with("_2017"),ends_with("_2018"))

# Por UF e munic

t4_uf_munic <- df_analises |>
  filter(ano %in% 2013:2018) |>
  mutate(razao = indicador_renda_outras_fontes_negros/indicador_renda_outras_fontes_brancos) |>
  select(ano, uf, cd_municipio_6digitos, nome_municipio, indicador_renda_outras_fontes_negros,indicador_renda_outras_fontes_brancos, razao) |>
  filter(!is.infinite(razao)) |>
  arrange(desc(razao)) |>
  group_by(ano, uf) |>
  slice(1:10) |>
  pivot_wider(names_from = ano, values_from = c(indicador_renda_outras_fontes_negros,indicador_renda_outras_fontes_brancos, razao)) |>
  select(uf, cd_municipio_6digitos, nome_municipio, ends_with("_2013"),ends_with("_2014"),
         ends_with("_2015"),ends_with("_2016"),ends_with("_2017"),ends_with("_2018"))

#'--------------------------------------------------------------------
# 5 - Mães adolescentes

# Brasil como um todo

t5_br <- df_analises |>
  filter(ano %in% 2013:2018) |>
  mutate(razao = indicador_maes_adolescentes_negras/indicador_maes_adolescentes_brancas) |>
  select(ano, cd_municipio_6digitos, nome_municipio, indicador_maes_adolescentes_negras,indicador_maes_adolescentes_brancas, razao) |>
  filter(!is.infinite(razao)) |>
  arrange(desc(razao)) |>
  group_by(ano) |>
  slice(1:10) |>
  pivot_wider(names_from = ano, values_from = c(indicador_maes_adolescentes_negras,indicador_maes_adolescentes_brancas, razao)) |>
  select(cd_municipio_6digitos, nome_municipio, ends_with("_2013"),ends_with("_2014"),
         ends_with("_2015"),ends_with("_2016"),ends_with("_2017"),ends_with("_2018"))

# Por UF

t5_uf <- df_uf |>
  filter(ano %in% 2013:2018) |>
  mutate(razao = indicador_maes_adolescentes_negras/indicador_maes_adolescentes_brancas) |>
  select(ano, uf, indicador_maes_adolescentes_negras,indicador_maes_adolescentes_brancas, razao) |>
  filter(!is.infinite(razao)) |>
  arrange(desc(razao)) |>
  group_by(ano, uf) |>
  slice(1:10) |>
  pivot_wider(names_from = ano, values_from = c(indicador_maes_adolescentes_negras,indicador_maes_adolescentes_brancas, razao)) |>
  select(uf, ends_with("_2013"),ends_with("_2014"),
         ends_with("_2015"),ends_with("_2016"),ends_with("_2017"),ends_with("_2018"))


# Por UF e munic

t5_uf_munic <- df_analises |>
  filter(ano %in% 2013:2018) |>
  mutate(razao = indicador_maes_adolescentes_negras/indicador_maes_adolescentes_brancas) |>
  select(ano, uf, cd_municipio_6digitos, nome_municipio, indicador_maes_adolescentes_negras,indicador_maes_adolescentes_brancas, razao) |>
  filter(!is.infinite(razao)) |>
  arrange(desc(razao)) |>
  group_by(ano, uf) |>
  slice(1:10) |>
  pivot_wider(names_from = ano, values_from = c(indicador_maes_adolescentes_negras,indicador_maes_adolescentes_brancas, razao)) |>
  select(uf, cd_municipio_6digitos, nome_municipio, ends_with("_2013"),ends_with("_2014"),
         ends_with("_2015"),ends_with("_2016"),ends_with("_2017"),ends_with("_2018"))

#'--------------------------------------------------------------------
# 6 - Desocupação

# Brasil como um todo

t6_br <- df_analises |>
  filter(ano %in% 2013:2018) |>
  mutate(razao = indicador_desocupados_negros/indicador_desocupados_brancos) |>
  select(ano, cd_municipio_6digitos, nome_municipio, indicador_desocupados_negros,indicador_desocupados_brancos, razao) |>
  filter(!is.infinite(razao)) |>
  arrange(desc(razao)) |>
  group_by(ano) |>
  slice(1:10) |>
  pivot_wider(names_from = ano, values_from = c(indicador_desocupados_negros,indicador_desocupados_brancos, razao)) |>
  select(cd_municipio_6digitos, nome_municipio, ends_with("_2013"),ends_with("_2014"),
         ends_with("_2015"),ends_with("_2016"),ends_with("_2017"),ends_with("_2018"))

# Por UF

t6_uf <- df_uf |>
  filter(ano %in% 2013:2018) |>
  mutate(razao = indicador_desocupados_negros/indicador_desocupados_brancos) |>
  select(ano, uf, indicador_desocupados_negros,indicador_desocupados_brancos, razao) |>
  filter(!is.infinite(razao)) |>
  arrange(desc(razao)) |>
  group_by(ano, uf) |>
  slice(1:10) |>
  pivot_wider(names_from = ano, values_from = c(indicador_desocupados_negros,indicador_desocupados_brancos, razao)) |>
  select(uf, ends_with("_2013"),ends_with("_2014"),
         ends_with("_2015"),ends_with("_2016"),ends_with("_2017"),ends_with("_2018"))


# Por UF e munic

t6_uf_munic <- df_analises |>
  filter(ano %in% 2013:2018) |>
  mutate(razao = indicador_desocupados_negros/indicador_desocupados_brancos) |>
  select(ano, uf, cd_municipio_6digitos, nome_municipio, indicador_desocupados_negros,indicador_desocupados_brancos, razao) |>
  filter(!is.infinite(razao)) |>
  arrange(desc(razao)) |>
  group_by(ano, uf) |>
  slice(1:10) |>
  pivot_wider(names_from = ano, values_from = c(indicador_desocupados_negros,indicador_desocupados_brancos, razao)) |>
  select(uf, cd_municipio_6digitos, nome_municipio, ends_with("_2013"),ends_with("_2014"),
         ends_with("_2015"),ends_with("_2016"),ends_with("_2017"),ends_with("_2018"))

#'--------------------------------------------------------------------
# 7 - Informalidade

# Brasil como um todo

t7_br <- df_analises |>
  filter(ano %in% 2013:2018) |>
  mutate(razao = indicador_informalidade_negros/indicador_informalidade_brancos) |>
  select(ano, cd_municipio_6digitos, nome_municipio, indicador_informalidade_negros,indicador_informalidade_brancos, razao) |>
  filter(!is.infinite(razao)) |>
  arrange(desc(razao)) |>
  group_by(ano) |>
  slice(1:10) |>
  pivot_wider(names_from = ano, values_from = c(indicador_informalidade_negros,indicador_informalidade_brancos, razao)) |>
  select(cd_municipio_6digitos, nome_municipio, ends_with("_2013"),ends_with("_2014"),
         ends_with("_2015"),ends_with("_2016"),ends_with("_2017"),ends_with("_2018"))

# Por UF

t7_uf <- df_uf |>
  filter(ano %in% 2013:2018) |>
  mutate(razao = indicador_informalidade_negros/indicador_informalidade_brancos) |>
  select(ano, uf, indicador_informalidade_negros,indicador_informalidade_brancos, razao) |>
  filter(!is.infinite(razao)) |>
  arrange(desc(razao)) |>
  group_by(ano, uf) |>
  slice(1:10) |>
  pivot_wider(names_from = ano, values_from = c(indicador_informalidade_negros,indicador_informalidade_brancos, razao)) |>
  select(uf, ends_with("_2013"),ends_with("_2014"),
         ends_with("_2015"),ends_with("_2016"),ends_with("_2017"),ends_with("_2018"))

# Por UF e munic

t7_uf_munic <- df_analises |>
  filter(ano %in% 2013:2018) |>
  mutate(razao = indicador_informalidade_negros/indicador_informalidade_brancos) |>
  select(ano, uf, cd_municipio_6digitos, nome_municipio, indicador_informalidade_negros,indicador_informalidade_brancos, razao) |>
  filter(!is.infinite(razao)) |>
  arrange(desc(razao)) |>
  group_by(ano, uf) |>
  slice(1:10) |>
  pivot_wider(names_from = ano, values_from = c(indicador_informalidade_negros,indicador_informalidade_brancos, razao)) |>
  select(uf, cd_municipio_6digitos, nome_municipio, ends_with("_2013"),ends_with("_2014"),
         ends_with("_2015"),ends_with("_2016"),ends_with("_2017"),ends_with("_2018"))

#'--------------------------------------------------------------------
#' # 8 - Homicidios
#'
#' # Brasil como um todo
#'
#' t8_br <- df_analises |>
#'   filter(ano %in% 2013:2021) |>
#'   mutate(razao = indicador_homicidios_negros/indicador_homicidios_brancos) |>
#'   select(ano, cd_municipio_6digitos, nome_municipio, indicador_homicidios_negros,indicador_homicidios_brancos, razao) |>
#'   filter(!is.infinite(razao)) |>
#'   arrange(desc(razao)) |>
#'   group_by(ano) |>
#'   slice(1:10) |>
#'   pivot_wider(names_from = ano, values_from = c(indicador_homicidios_negros,indicador_homicidios_brancos, razao)) |>
#'   select(cd_municipio_6digitos, nome_municipio, ends_with("_2013"),ends_with("_2014"),
#'          ends_with("_2015"),ends_with("_2016"),ends_with("_2017"),ends_with("_2018"))
#'
#' # Por UF e munic
#'
#' t8_uf_munic <- df_analises |>
#'   filter(ano %in% 2013:2021) |>
#'   mutate(razao = indicador_homicidios_negros/indicador_homicidios_brancos) |>
#'   select(ano, uf, cd_municipio_6digitos, nome_municipio, indicador_homicidios_negros,indicador_homicidios_brancos, razao) |>
#'   filter(!is.infinite(razao)) |>
#'   arrange(desc(razao)) |>
#'   group_by(ano, uf) |>
#'   slice(1:10) |>
#'   pivot_wider(names_from = ano, values_from = c(indicador_homicidios_negros,indicador_homicidios_brancos, razao)) |>
#'   select(uf, cd_municipio_6digitos, nome_municipio, ends_with("_2013"),ends_with("_2014"),
#'          ends_with("_2015"),ends_with("_2016"),ends_with("_2017"),ends_with("_2018"))
#'
#' #'--------------------------------------------------------------------
#' # 9 - Intervenção legal
#'
#' # Brasil como um todo
#'
#' t9_br <- df_analises |>
#'   filter(ano %in% 2013:2021) |>
#'   mutate(razao = indicador_intervencao_negros/indicador_intervencao_brancos) |>
#'   select(ano, cd_municipio_6digitos, nome_municipio, indicador_intervencao_negros,indicador_intervencao_brancos, razao) |>
#'   filter(!is.infinite(razao)) |>
#'   arrange(desc(razao)) |>
#'   group_by(ano) |>
#'   slice(1:10) |>
#'   pivot_wider(names_from = ano, values_from = c(indicador_intervencao_negros,indicador_intervencao_brancos, razao)) |>
#'   select(cd_municipio_6digitos, nome_municipio, ends_with("_2013"),ends_with("_2014"),
#'          ends_with("_2015"),ends_with("_2016"),ends_with("_2017"),ends_with("_2018"))
#'
#' # Por UF e munic
#'
#' t9_uf_munic <- df_analises |>
#'   filter(ano %in% 2013:2021) |>
#'   mutate(razao = indicador_intervencao_negros/indicador_intervencao_brancos) |>
#'   select(ano, uf, cd_municipio_6digitos, nome_municipio, indicador_intervencao_negros,indicador_intervencao_brancos, razao) |>
#'   filter(!is.infinite(razao)) |>
#'   arrange(desc(razao)) |>
#'   group_by(ano, uf) |>
#'   slice(1:10) |>
#'   pivot_wider(names_from = ano, values_from = c(indicador_intervencao_negros,indicador_intervencao_brancos, razao)) |>
#'   select(uf, cd_municipio_6digitos, nome_municipio, ends_with("_2013"),ends_with("_2014"),
#'          ends_with("_2015"),ends_with("_2016"),ends_with("_2017"),ends_with("_2018"))


# Exportacao dos dados ----------------------------------------------------

# criando output

wb <- createWorkbook()

addWorksheet(wb, "Ensino_Medio")
addWorksheet(wb, "Ensino_Medio_UF")
addWorksheet(wb, "Ensino_Medio_UF_munic")
addWorksheet(wb, "Acess_Esgoto")
addWorksheet(wb, "Acess_Esgoto_UF")
addWorksheet(wb, "Acess_Esgoto_UF_munic")
addWorksheet(wb, "PBF")
addWorksheet(wb, "PBF_UF")
addWorksheet(wb, "PBF_UF_munic")
addWorksheet(wb, "Renda_outras")
addWorksheet(wb, "Renda_outras_UF")
addWorksheet(wb, "Renda_outras_UF_munic")
addWorksheet(wb, "Maes_adolesc")
addWorksheet(wb, "Maes_adolesc_UF")
addWorksheet(wb, "Maes_adolesc_UF_munic")
addWorksheet(wb, "Desocupados")
addWorksheet(wb, "Desocupados_UF")
addWorksheet(wb, "Desocupados_UF_munic")
addWorksheet(wb, "Informalidade")
addWorksheet(wb, "Informalidade_UF")
addWorksheet(wb, "Informalidade_UF_munic")
# addWorksheet(wb, "Homicidios")
# addWorksheet(wb, "Homicidios_UF_munic")
# addWorksheet(wb, "Intervencoes")
# addWorksheet(wb, "Intervencoes_UF_munic")

# inserir dados nas planilhas
writeData(wb, 1, t1_br)
writeData(wb, 2, t1_uf)
writeData(wb, 2, t1_uf_munic)
writeData(wb, 3, t2_br)
writeData(wb, 4, t2_uf)
writeData(wb, 4, t2_uf_munic)
writeData(wb, 5, t3_br)
writeData(wb, 6, t3_uf)
writeData(wb, 6, t3_uf_munic)
writeData(wb, 7, t4_br)
writeData(wb, 8, t4_uf)
writeData(wb, 8, t4_uf_munic)
writeData(wb, 9, t5_br)
writeData(wb, 10, t5_uf)
writeData(wb, 10, t5_uf_munic)
writeData(wb, 11, t6_br)
writeData(wb, 12, t6_uf)
writeData(wb, 12, t6_uf_munic)
writeData(wb, 13, t7_br)
writeData(wb, 14, t7_uf)
writeData(wb, 14, t7_uf_munic)
# writeData(wb, 15, t8_br)
# writeData(wb, 16, t8_uf_munic)
# writeData(wb, 17, t9_br)
# writeData(wb, 18, t9_uf_munic)

# exportar xlsx

saveWorkbook(wb, file = file.path(DIR, paste0("resultados - ranking.xlsx")),overwrite = TRUE)

