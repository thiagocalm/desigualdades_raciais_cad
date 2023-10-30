#' ------------------------------------------------------
#' @author Thiago Cordeiro Almeida
#' @last-update 2023-10-28
#' @description Análise dos indicadores de desigualdade para o Top 50 de genocidio negro
#' -----------------------------------------------------
options(scipen = 9999999)
rm(list = ls())
gc()

# bibliotecas -------------------------------------------------------------

if(!require("pacman")) install.packages(("pacman"))
pacman::p_load(tidyverse, arrow, openxlsx)

# Importacao dos dados ----------------------------------------------------
DIR_top50 <- "./output/resultados"
DIR <- "./output/base de dados - violencia e desigualdade"
# leitura dos dados

top50_homicidios <- read_parquet(file = file.path(DIR_top50, "resultados - ranking genocidio populacao negra - homicidios.parquet"))

df <- read_parquet(file = file.path(DIR, "base_violencia_desigualdade.parquet"))
df_uf <- read_parquet(file = "./output/base de dados - desigualdade/cad_indicadores_uf.parquet")
df_br <- read_parquet(file = "./output/base de dados - desigualdade/cad_indicadores_br.parquet")

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

df_uf <- df_uf %>%
  mutate(uf = factor(uf, levels = cods_uf$cod, labels = cods_uf$names))


# Fazendo uma media dos indicadores de desigualdade por ano ---------------

df <- df %>%
  filter(ano %in% 2013:2018) %>%
  group_by(regiao, uf, cd_municipio_6digitos, nome_municipio) %>%
  reframe(
    across(c(indicador_em:indicador_informalidade_brancos),
           ~mean(.x))
  )

df_uf <- df_uf %>%
  filter(ano %in% 2013:2018) %>%
  group_by(regiao, uf) %>%
  reframe(
    across(c(indicador_em:indicador_informalidade_brancos),
           ~mean(.x))
  )

df_br <- df_br %>%
  filter(ano %in% 2013:2018) %>%
  reframe(
    across(c(indicador_em:indicador_informalidade_brancos),
           ~mean(.x))
  )

# Construindo base de indicadores de desigualdade para o top 50 -----------

top50_homicidios_desigualdades <- top50_homicidios %>%
  filter(ranking <= 50) %>%
  left_join(
    df %>% select(-c(nome_municipio, uf, regiao)),
    by = c("cd_municipio_6digitos")
  ) %>%
  mutate(nivel_geografico = "municipio") %>%
  bind_rows(
    df_uf %>%
      filter(uf %in% top50_homicidios$uf) %>%
      mutate(nivel_geografico = "uf")
  ) %>%
  bind_rows(
    df_br %>%
      mutate(nivel_geografico = "br")
  ) %>%
  mutate(
    razao_em = indicador_em_negros/indicador_em_brancos,
    razao_acesso_esgoto = indicador_acesso_esgoto_negros/indicador_acesso_esgoto_brancos,
    razao_pbf = indicador_pbf_negros/indicador_pbf_brancos,
    razao_renda_outras_fontes = indicador_renda_outras_fontes_negros/indicador_renda_outras_fontes_brancos,
    razao_maes_adolescentes = indicador_maes_adolescentes_negras/indicador_maes_adolescentes_brancas,
    razao_desocupados = indicador_desocupados_negros/indicador_desocupados_brancos,
    razao_informalidade = indicador_informalidade_negros/indicador_informalidade_brancos
  )

# Organizando a base

top50_homicidios_desigualdades <- top50_homicidios_desigualdades %>%
  mutate(
    uf = case_when(is.na(uf) ~ "Brasil", TRUE ~ uf),
    nome_municipio = case_when(is.na(nome_municipio) ~ uf, TRUE ~ nome_municipio)
  )

municipios <-  as.vector(top50_homicidios$nome_municipio[1:50])

for(i in seq_along(municipios)){
  municipio  <-  municipios[i]
  uf_minc <-  as.vector(top50_homicidios_desigualdades[i,4])[[1]]

  if(i == 1){
    top_50_df <- top50_homicidios_desigualdades %>%
      filter(nome_municipio %in% c(municipio, uf_minc, "Brasil")) %>%
      mutate(
        ranking = i,
        municipio_top50 = municipio,
        nivel_geografico = nome_municipio
      ) %>%
      select(
        municipio_top50, uf, nivel_geografico, ranking, homicidios, indicador_homicidios_negros,
        ends_with(c("negros","negras","brancos","brancas")), starts_with("razao")
      )
  }else{
    top_50_df <- top_50_df %>%
      bind_rows(
        top50_homicidios_desigualdades %>%
          filter(nome_municipio %in% c(municipio, uf_minc, "Brasil")) %>%
          mutate(
            ranking = i,
            municipio_top50 = municipio,
            nivel_geografico = nome_municipio
          ) %>%
          select(
            municipio_top50, uf, nivel_geografico, ranking, homicidios, indicador_homicidios_negros,
            ends_with(c("negros","negras","brancos","brancas")), starts_with("razao")
          )
      )
  }
  print(paste0("Finalizamos o numero ",i," do raking!"))
}

top_50_df_exportar <- top_50_df %>%
  mutate(
    uf = case_when(uf == "Brasil" ~ "-", TRUE ~ uf),
    nivel_geografico = case_when(
      nivel_geografico %in% as.vector(cods_uf[,2])[[1]] ~ "UF",
      nivel_geografico %in% as.vector(top50_homicidios_desigualdades[1:50,2])[[1]] ~ "Municipio",
      TRUE ~ nivel_geografico
    )
  ) %>%
  select(ranking, municipio_top50, uf, nivel_geografico, homicidios, indicador_homicidios_negros,
         starts_with("indicador_em"),ends_with("em"),starts_with("indicador_acesso"),ends_with("esgoto"),
         starts_with("indicador_pbf"),ends_with("pbf"),starts_with("indicador_renda"),ends_with("fontes"),
         starts_with("indicador_maes"),ends_with("adolescentes"),starts_with("indicador_desocupados"),ends_with("desocupados"),
         starts_with("indicador_informalidade"),ends_with("informalidade")) %>%
  pivot_wider(names_from = nivel_geografico, values_from = c(homicidios:razao_informalidade)) %>%
  group_by(municipio_top50) %>%
  mutate_all(~ replace_na(.x, 0)) %>% View()
  mutate(across(c(homicidios_Municipio:razao_informalidade_Brasil), ~ sum(.x))) %>%
  mutate(id = row_number()) %>%
  filter(id == 1) %>%
  select(-id) %>%
  select(all_of(c(
    "ranking","municipio_top50","uf","homicidios_Municipio",
    "indicador_homicidios_negros_Municipio","indicador_em_negros_Municipio",
    "indicador_em_brancos_Municipio","razao_em_Municipio",
    "indicador_em_negros_UF","indicador_em_brancos_UF","razao_em_UF",
    "indicador_em_negros_Brasil","indicador_em_brancos_Brasil","razao_em_Brasil",
    "indicador_acesso_esgoto_negros_Municipio","indicador_acesso_esgoto_brancos_Municipio",
    "razao_acesso_esgoto_Municipio",
    "indicador_acesso_esgoto_negros_UF","indicador_acesso_esgoto_brancos_UF","razao_acesso_esgoto_UF",
    "indicador_acesso_esgoto_negros_Brasil","indicador_acesso_esgoto_brancos_Brasil",
    "razao_acesso_esgoto_Brasil",
    "indicador_pbf_negros_Municipio","indicador_pbf_brancos_Municipio","razao_pbf_Municipio",
    "indicador_pbf_negros_UF","indicador_pbf_brancos_UF","razao_pbf_UF",
    "indicador_pbf_negros_Brasil","indicador_pbf_brancos_Brasil","razao_pbf_Brasil",
    "indicador_renda_outras_fontes_negros_Municipio","indicador_renda_outras_fontes_brancos_Municipio",
    "razao_renda_outras_fontes_Municipio",
    "indicador_renda_outras_fontes_negros_UF","indicador_renda_outras_fontes_brancos_UF",
    "razao_renda_outras_fontes_UF",
    "indicador_renda_outras_fontes_negros_Brasil","indicador_renda_outras_fontes_brancos_Brasil",
    "razao_renda_outras_fontes_Brasil",
    "indicador_maes_adolescentes_negras_Municipio","indicador_maes_adolescentes_brancas_Municipio",
    "razao_maes_adolescentes_Municipio",
    "indicador_maes_adolescentes_negras_UF","indicador_maes_adolescentes_brancas_UF",
    "razao_maes_adolescentes_UF",
    "indicador_maes_adolescentes_negras_Brasil","indicador_maes_adolescentes_brancas_Brasil",
    "razao_maes_adolescentes_Brasil",
    "indicador_desocupados_negros_Municipio","indicador_desocupados_brancos_Municipio",
    "razao_desocupados_Municipio",
    "indicador_desocupados_negros_UF","indicador_desocupados_brancos_UF","razao_desocupados_UF",
    "indicador_desocupados_negros_Brasil","indicador_desocupados_brancos_Brasil",
    "razao_desocupados_Brasil",
    "indicador_informalidade_negros_Municipio","indicador_informalidade_brancos_Municipio",
    "razao_informalidade_Municipio",
    "indicador_informalidade_negros_UF","indicador_informalidade_brancos_UF",
    "razao_informalidade_UF",
    "indicador_informalidade_negros_Brasil","indicador_informalidade_brancos_Brasil",
    "razao_informalidade_Brasil"
  )))

# exportar base

write_csv(top_50_df_exportar, file = file.path(DIR_top50,"resultados - Top 50 municipios violencia e desigualdade.csv"))
rm(top_50_df_exportar)
# Analises ----------------------------------------------------------------

# 1 - Indicadores da populacao negra em relacao à populacao negra na UF e Brasil.

top_50_df %>%
  filter(ranking == 1) %>%
  select(-c(starts_with("razao"),ends_with("brancos"),homicidios, indicador_homicidios_negros)) %>%
  rename_all(~str_remove(.x,"indicador_")) %>%
  rename_all(~str_remove(.x,"_negros")) %>%
  select(-municipio_top50) %>%
  pivot_longer(em:informalidade, names_to = "indicadores", values_to = "valores") %>%
  ggplot() +
  aes(x = indicadores, y = valores, color = nivel_geografico) +
  geom_point() +
  theme_light()

# 2 - Indicadores da razao entre a populacao negra e branca no municipio, na UF e no Brasil

top_50_df %>%
  filter(ranking == 1) %>%
  select(-c(starts_with("indicador_"),homicidios)) %>%
  rename_all(~str_remove(.x,"razao_")) %>%
  select(-municipio_top50) %>%
  pivot_longer(em:informalidade, names_to = "razoes_negros_brancos", values_to = "valores") %>%
  ggplot() +
  aes(x = razoes_negros_brancos, y = valores, color = nivel_geografico) +
  geom_point() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", alpha = .8, linewidth = 1.07) +
  theme_light()

## Agora é salvar, organizar tabela e pensar como replicar os gráficos para todos os municipios E SALVAR NA PASTA
