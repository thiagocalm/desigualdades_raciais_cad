#' ------------------------------------------------------
#' @author Thiago Cordeiro Almeida
#' @last-update 2023-12-03
#' @description Construção da lista do top 50 após a avaliação final do Danilo
#' @update-description Inclusão dos indicadores de desigualdade e contexto
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

top50_homicidios <- read_parquet(
  file = file.path(
    DIR_top50,
    "tabelas complementares",
    "resultados - ranking genocidio populacao negra - homicidios da população jovem negra [lista final].parquet"
  )
)

df <- read_parquet(file = file.path(DIR, "base_violencia_desigualdade.parquet"))
df_uf <- read_parquet(file = "./output/base de dados - desigualdade/cad_indicadores_uf.parquet")
df_br <- read_parquet(file = "./output/base de dados - desigualdade/cad_indicadores_br.parquet")
df_pop_2010 <- read_parquet(file = "./output/base de dados - desigualdade/pop_2010_indicadores.parquet")

# Definicao da base de trabalho -------------------------------------------

cods_uf <- tibble(cod = c("11","12","13","14","15","16","17","21","22","23","24","25","26","27",
                          "28","29","31","32","33","35","41","42","43","50","51","52","53"),
                  names = c("Rondônia","Acre","Amazonas", "Roraima","Pará","Amapá","Tocantins","Maranhão",
                            "Piauí", "Ceará","Rio Grande do Norte","Paraíba","Pernambuco","Alagoas","Sergipe",
                            "Bahia","Minas Gerais","Espírito Santo","Rio de Janeiro","São Paulo","Paraná","Santa Catarina",
                            "Rio Grande do Sul","Mato Grosso do Sul","Mato Grosso","Goiás","Distrito Federal"))

df <- df |>
  mutate(uf = factor(uf, levels = cods_uf$cod, labels = cods_uf$names))

df_uf <- df_uf %>%
  mutate(uf = factor(uf, levels = cods_uf$cod, labels = cods_uf$names))

## Inserindo variaveis de contexto nos dados de desigualdade

# Municipio
# Inserindo variaveis de identificador de codigo do municipio de 6 digitos na base de indicadores de contexto

df <- df %>%
  left_join(
    df_pop_2010 %>%
      filter(nivel_geografico == "Municipio") %>%
      mutate(
        cd_municipio_6digitos = as.numeric(substr(codigo_geografico, 1,6)),
        regiao = substr(codigo_geografico, 1,1),
        uf = substr(codigo_geografico, 1,2),
        uf = factor(uf, levels = cods_uf$cod, labels = cods_uf$names)
      ) %>%
      select(-c(nivel_geografico, codigo_geografico, starts_with("prop"))),
    by = c("regiao", "uf","cd_municipio_6digitos"),
    keep = FALSE
  )

df_uf <- df_uf %>%
  left_join(
    df_pop_2010 %>%
      filter(nivel_geografico == "UF") %>%
      mutate(
        regiao = substr(codigo_geografico, 1,1),
        uf = substr(codigo_geografico, 1,2),
        uf = factor(uf, levels = cods_uf$cod, labels = cods_uf$names)
      ) %>%
      select(-c(nivel_geografico, codigo_geografico, starts_with("prop"))),
    by = c("regiao", "uf"),
    keep = FALSE
  )

df_br <- df_br %>%
  bind_cols(
    df_pop_2010 %>%
      filter(nivel_geografico == "Brasil") %>%
      select(-c(nivel_geografico, codigo_geografico, starts_with("prop")))
  )

# Fazendo uma media dos indicadores de desigualdade por ano ---------------

df <- df %>%
  filter(ano %in% 2013:2018) %>%
  group_by(regiao, uf, cd_municipio_6digitos, nome_municipio) %>%
  reframe(
    across(c(indicador_em:indicador_informalidade_brancos,indicador_pop_negra:indicador_pop_rural),
           ~mean(.x))
  )

df_uf <- df_uf %>%
  filter(ano %in% 2013:2018) %>%
  group_by(regiao, uf) %>%
  reframe(
    across(c(indicador_em:indicador_informalidade_brancos,indicador_pop_negra:indicador_pop_rural),
           ~mean(.x))
  )

df_br <- df_br %>%
  filter(ano %in% 2013:2018) %>%
  reframe(
    across(c(indicador_em:indicador_informalidade_brancos,indicador_pop_negra:indicador_pop_rural),
           ~mean(.x))
  )

# Construindo base de indicadores de desigualdade para o top 50 -----------

top50_homicidios_desigualdades <- top50_homicidios %>%
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
  rename(
    nome_municipio = municipio,
    indicador_homicidios_negros = indicador_homicidios
  ) %>%
  mutate(
    uf = case_when(is.na(uf) ~ "Brasil", TRUE ~ uf),
    nome_municipio = case_when(is.na(nome_municipio) ~ uf, TRUE ~ nome_municipio)
  )

municipios <-  top50_homicidios %>% select(municipio) %>% pull()

for(i in seq_along(municipios)){
  municipio  <-  as.vector(top50_homicidios$municipio[i])
  uf_munic <-  as.vector(top50_homicidios_desigualdades[i,3])[[1]]
  if(i == 1){
    top_50_df <- top50_homicidios_desigualdades %>%
      filter(nome_municipio == municipio & uf == uf_munic |
               nome_municipio == uf_munic |
               nome_municipio == "Brasil") %>%
      mutate(
        ranking = i,
        municipio_top50 = municipio,
        nivel_geografico = nome_municipio,
      ) %>%
      select(
        cd_municipio_6digitos, uf, municipio_top50, nivel_geografico, ranking, homicidios, indicador_homicidios_negros,
        starts_with("pop"),ends_with(c("negros","negras","brancos","brancas","negra","rural")), starts_with("razao")
      )
  }else{
    top_50_df <- top_50_df %>%
      bind_rows(
        top50_homicidios_desigualdades %>%
          filter(nome_municipio == municipio & uf == uf_munic |
                   nome_municipio == uf_munic |
                   nome_municipio == "Brasil") %>%
          mutate(
            ranking = i,
            municipio_top50 = municipio,
            nivel_geografico = nome_municipio
          ) %>%
          select(
            cd_municipio_6digitos, municipio_top50, uf, nivel_geografico, ranking, homicidios, indicador_homicidios_negros,
            starts_with("pop"),ends_with(c("negros","negras","brancos","brancas","negra","rural")), starts_with("razao")
          )
      )
  }
  print(paste0("Finalizamos o numero ",i," do raking!"))
}

top_50_df <- top_50_df %>%
  mutate(
    uf = case_when(uf == "Brasil" ~ "-", TRUE ~ uf),
    nivel_geografico_2 = case_when(
      nivel_geografico %in% as.vector(cods_uf[,2])[[1]] ~ "UF",
      nivel_geografico %in% as.vector(top50_homicidios_desigualdades[1:50,2])[[1]] ~ "Municipio",
      TRUE ~ nivel_geografico
    )
  ) %>%
  select(ranking, municipio_top50, cd_municipio_6digitos, uf, nivel_geografico,nivel_geografico_2,
         homicidios, indicador_homicidios_negros,
         starts_with("pop"),starts_with("indicador_pop"),
         starts_with("indicador_em"),ends_with("em"),starts_with("indicador_acesso"),ends_with("esgoto"),
         starts_with("indicador_pbf"),ends_with("pbf"),starts_with("indicador_renda"),ends_with("fontes"),
         starts_with("indicador_maes"),ends_with("adolescentes"),starts_with("indicador_desocupados"),ends_with("desocupados"),
         starts_with("indicador_informalidade"),ends_with("informalidade"))

top_50_df_exportar <- top_50_df %>%
  select(-nivel_geografico) %>%
  pivot_wider(names_from = nivel_geografico_2, values_from = c(homicidios:razao_informalidade)) %>%
  group_by(ranking) %>%
  mutate_all(~ replace_na(.x, 0)) %>%
  mutate(across(c(homicidios_Municipio:razao_informalidade_Brasil), ~ sum(.x))) %>%
  mutate(id = row_number()) %>%
  filter(id == 1) %>%
  ungroup() %>%
  select(-c(id,cd_municipio_6digitos)) %>%
  select(all_of(c(
    "ranking","municipio_top50","uf","homicidios_Municipio",
    "indicador_homicidios_negros_Municipio",
    "pop_total_Municipio","pop_jovem_negra_Municipio",
    "indicador_pop_negra_Municipio","indicador_pop_negra_UF",
    "indicador_pop_negra_Brasil",
    "indicador_pop_rural_Municipio","indicador_pop_rural_UF",
    "indicador_pop_rural_Brasil",
    "indicador_em_negros_Municipio",
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
    #"indicador_maes_adolescentes_negras_Municipio","indicador_maes_adolescentes_brancas_Municipio",
    #"razao_maes_adolescentes_Municipio",
    #"indicador_maes_adolescentes_negras_UF","indicador_maes_adolescentes_brancas_UF",
    #"razao_maes_adolescentes_UF",
    #"indicador_maes_adolescentes_negras_Brasil","indicador_maes_adolescentes_brancas_Brasil",
    #"razao_maes_adolescentes_Brasil",
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

write_csv(
  top_50_df_exportar,
  file = file.path(
    DIR_top50,
    "resultados - Top 50 municipios violencia e desigualdade - ajustado por pop jovem negra [lista final].csv"
  )
)

rm(top_50_df_exportar)

write_parquet(
  top_50_df,
  sink = file.path(
    DIR_top50,
    "resultados - Top 50 municipios violencia e desigualdade - ajustado por pop jovem negra [lista final].parquet"
  )
)

# Analises ----------------------------------------------------------------

# 1 - Indicadores da populacao negra em relacao à populacao negra na UF e Brasil.

# definindo paleta de cores
cols = c(
  "#a6cee3",
  rep("#1f78b4",27),
  rep("#b2df8a", 50)
)

names(cols) = c("Brasil",cods_uf[,2][[1]],municipios)

for(i in 1:50){
  # criacao de vetor de atribuicao de cores
  color_selection = top_50_df %>% filter(ranking == i)
  colors = cols[names(cols) %in% color_selection[,5][[1]]]
  # criacao de grafico
  plot <- top_50_df %>%
    select(-ends_with(c("_negras","_brancas","total","jovem_negra"))) %>%
    filter(ranking == i) %>%
    select(-c(starts_with("razao"),ends_with("brancos"),homicidios, indicador_homicidios_negros)) %>%
    rename_all(~str_remove(.x,"indicador_")) %>%
    rename_all(~str_remove(.x,"_negros")) %>%
    select(-municipio_top50) %>%
    pivot_longer(pop_negra:informalidade, names_to = "indicadores", values_to = "valores") %>%
    mutate(indicadores = case_when(
      indicadores == "pop_negra"~ "Pop. negra",
      indicadores == "pop_rural"~ "Pop. rural",
      indicadores == "em"~ "Ensino Médio",
      indicadores == "acesso_esgoto"~ "Acesso a Esgoto",
      indicadores == "pbf"~ "Beneficiário do PBF",
      indicadores == "renda_outras_fontes"~ "Rendimento (outras fontes)",
      indicadores == "desocupados"~ "Desocupados ou inativo",
      indicadores == "informalidade"~ "Informalidade"
    )) |>
    ggplot() +
    aes(x = indicadores, y = valores, fill = nivel_geografico) +
    geom_point(size = 4, alpha = .8, shape = 21, color = "#f0f0f0") +
    theme_light() +
    coord_cartesian(ylim = c(0,100)) +
    scale_y_continuous(breaks = seq(0,100,10)) +
    scale_fill_manual(values = colors) +
    labs(
      title = paste0("Município: ",top_50_df[top_50_df$ranking == i,][1,5][[1]], " (",top_50_df[top_50_df$ranking == i,][2,5][[1]],")"),
      x = "Indicadores de desigualdade para a população negra",
      y = "%"
    ) +
    theme(
      legend.title = element_blank(),
      axis.title = element_text(face = "bold", size = 12, color = "#636363", hjust = 1),
      axis.text.y = element_text(size = 10,color = "#636363"),
      axis.text.x = element_text(size = 10,color = "#636363"),
      plot.title = element_text(face = "bold", size = 16, color = "#636363", hjust = .05)
    )

  ggsave(
    filename = paste0("graf_percentual_pop_negra_ranking_",i,".jpeg"),
    plot = plot,
    device = "jpeg",
    path = file.path(DIR_top50, "graficos - ranking violencia e desigualdade"),
    width = 16,
    height = 9,
    units = "in"
  )
  print(paste0("Finalizamos o numero ",i, " do ranking!!!"))
}

# 2.1 - Correlação entre indicadores UF e indicadores Municipais - GERAL

top_50_df %>%
  filter(nivel_geografico_2 != "Brasil") %>%
  select(-c(
    ends_with(c("_negras","_brancas","total","jovem_negra")),
    starts_with("razao"),ends_with("brancos"),homicidios, indicador_homicidios_negros,
    cd_municipio_6digitos, nivel_geografico, ends_with(c("_rural","_negra"))
  )) %>%
  rename_all(~str_remove(.x,"indicador_")) %>%
  rename_all(~str_remove(.x,"_negros")) %>%
  pivot_longer(
    em:informalidade,
    names_to = "indicadores",
    values_to = "valor"
  ) %>%
  mutate(indicadores = case_when(
    # indicadores == "pop_negra"~ "Pop. negra",
    # indicadores == "pop_rural"~ "Pop. rural",
    indicadores == "em"~ "Ensino Médio",
    indicadores == "acesso_esgoto"~ "Acesso a Esgoto",
    indicadores == "pbf"~ "Beneficiário do PBF",
    indicadores == "renda_outras_fontes"~ "Rendimento (outras fontes)",
    indicadores == "desocupados"~ "Desocupados ou inativo",
    indicadores == "informalidade"~ "Informalidade"
  )) %>%
  pivot_wider(names_from = nivel_geografico_2, values_from = valor) %>%
  ggplot() +
  aes(x = UF, y = Municipio, group = indicadores, fill = indicadores) +
  geom_point(size = 4, alpha = .7, shape = 21, color = "#f0f0f0") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  theme_light() +
  coord_cartesian(ylim = c(0,100), xlim = c(0,100)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  scale_x_continuous(breaks = seq(0,100,10)) +
  scale_fill_brewer(palette = "Set2") +
  # labs(
  #   title = paste0("Município: ",top_50_df[top_50_df$ranking == i,][1,5][[1]], " (",top_50_df[top_50_df$ranking == i,][2,5][[1]],")"),
  #   x = "Indicadores de desigualdade para a população negra",
  #   y = "%"
  # ) +
  theme(
    legend.title = element_blank(),
    axis.title = element_text(face = "bold", size = 12, color = "#636363", hjust = 1),
    axis.text.y = element_text(size = 10,color = "#636363"),
    axis.text.x = element_text(size = 10,color = "#636363"),
    plot.title = element_text(face = "bold", size = 16, color = "#636363", hjust = .05)
  )

ggsave(
  filename = paste0("graf_associacao_Municipios_UF_GERAL.jpeg"),
  device = "jpeg",
  path = file.path(DIR_top50, "graficos - ranking violencia e desigualdade"),
  width = 16,
  height = 9,
  units = "in"
)

# 2.2 - Correlação entre indicadores UF e indicadores Municipais - POR UF

top_50_df %>%
  filter(nivel_geografico_2 != "Brasil") %>%
  select(-c(
    ends_with(c("_negras","_brancas","total","jovem_negra")),
    starts_with("razao"),ends_with("brancos"),homicidios, indicador_homicidios_negros,
    cd_municipio_6digitos, nivel_geografico, ends_with(c("_rural","_negra"))
  )) %>%
  rename_all(~str_remove(.x,"indicador_")) %>%
  rename_all(~str_remove(.x,"_negros")) %>%
  pivot_longer(
    em:informalidade,
    names_to = "indicadores",
    values_to = "valor"
  ) %>%
  filter(valor > 0) %>%
  mutate(indicadores = case_when(
    # indicadores == "pop_negra"~ "Pop. negra",
    # indicadores == "pop_rural"~ "Pop. rural",
    indicadores == "em"~ "Ensino Médio",
    indicadores == "acesso_esgoto"~ "Acesso a Esgoto",
    indicadores == "pbf"~ "Beneficiário do PBF",
    indicadores == "renda_outras_fontes"~ "Rendimento (outras fontes)",
    indicadores == "desocupados"~ "Desocupados ou inativo",
    indicadores == "informalidade"~ "Informalidade"
  )) %>%
  pivot_wider(names_from = nivel_geografico_2, values_from = valor) %>%
  ggplot() +
  aes(x = UF, y = Municipio, group = indicadores, fill = indicadores) +
  geom_point(size = 4, alpha = .7, shape = 21, color = "#f0f0f0") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  theme_light() +
  coord_cartesian(ylim = c(0,100), xlim = c(0,100)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  scale_x_continuous(breaks = seq(0,100,10)) +
  lemon::facet_rep_wrap(uf ~ ., repeat.tick.labels = TRUE) +
  scale_fill_brewer(palette = "Set2") +
  # labs(
  #   title = paste0("Município: ",top_50_df[top_50_df$ranking == i,][1,5][[1]], " (",top_50_df[top_50_df$ranking == i,][2,5][[1]],")"),
  #   x = "Indicadores de desigualdade para a população negra",
  #   y = "%"
  # ) +
  theme(
    legend.title = element_blank(),
    axis.title = element_text(face = "bold", size = 12, color = "#636363", hjust = 1),
    axis.text.y = element_text(size = 10,color = "#636363"),
    axis.text.x = element_text(size = 10,color = "#636363"),
    plot.title = element_text(face = "bold", size = 16, color = "#636363", hjust = .05)
  )

ggsave(
  filename = paste0("graf_associacao_Municipios_UF_POR_UF.jpeg"),
  device = "jpeg",
  path = file.path(DIR_top50, "graficos - ranking violencia e desigualdade"),
  width = 16,
  height = 9,
  units = "in"
)

# 3 - indicadores por UF

# definindo paleta de cores
cols = c("#1f78b4","#b2df8a")

names(cols) = c("UF","Municipio")

# criacao de grafico
top_50_df %>%
  filter(uf != "-") %>%
  select(-ends_with(c("_negras","_brancas","total","jovem_negra","_rural","_negra"))) %>%
  select(-c(starts_with("razao"),ends_with("brancos"),homicidios, indicador_homicidios_negros)) %>%
  rename_all(~str_remove(.x,"indicador_")) %>%
  rename_all(~str_remove(.x,"_negros")) %>%
  select(-municipio_top50) %>%
  pivot_longer(em:informalidade, names_to = "indicadores", values_to = "valores") %>%
  filter(valores > 0) %>%
  mutate(indicadores = case_when(
    # indicadores == "pop_negra"~ "Pop. negra",
    # indicadores == "pop_rural"~ "Pop. rural",
    indicadores == "em"~ "Ensino Médio",
    indicadores == "acesso_esgoto"~ "Acesso a Esgoto",
    indicadores == "pbf"~ "Beneficiário do PBF",
    indicadores == "renda_outras_fontes"~ "Rendimento (outras fontes)",
    indicadores == "desocupados"~ "Inativo ou desocupado",
    indicadores == "informalidade"~ "Informalidade"
  )) |>
  ggplot() +
  aes(x = indicadores, y = valores, fill = nivel_geografico_2) +
  geom_point(size = 4, alpha = .8, shape = 21, color = "#f0f0f0") +
  theme_light() +
  coord_cartesian(ylim = c(0,100)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  scale_fill_manual(values = cols) +
  lemon::facet_rep_wrap(uf ~ ., repeat.tick.labels = TRUE) +
  scale_x_discrete(labels = function(x) str_wrap(str_replace_all(x, " ", " "), width = 3)) +
  labs(
    x = "Indicadores de desigualdade para a população negra",
    y = "%"
  ) +
  theme(
    legend.title = element_blank(),
    axis.title = element_text(face = "bold", size = 12, color = "#636363", hjust = 1),
    axis.text.y = element_text(size = 10,color = "#636363"),
    axis.text.x = element_text(size = 8,color = "#636363"),
    plot.title = element_text(face = "bold", size = 16, color = "#636363", hjust = .05)
  )

ggsave(
  filename = "graf_indicadores_desigualdade_POR_UF.jpeg",
  device = "jpeg",
  path = file.path(DIR_top50, "graficos - ranking violencia e desigualdade"),
  width = 18,
  height = 11,
  units = "in"
)
