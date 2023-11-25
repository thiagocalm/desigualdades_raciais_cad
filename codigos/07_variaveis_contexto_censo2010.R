#' ------------------------------------------------------
#' @author Thiago Cordeiro Almeida
#' @last-update 2023-11-25
#' @description Estimativasde proporção da população negra e rural por municipio
#' @update-description Inclusão de proporção da população negra entre 15 e 29 anos
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
  columns = c("V0001","V0002","V0010","V0606","V1006","V6036"),
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
    situacao_dom = V1006,
    idade = V6036
  ) %>%
  mutate(
    cod_munic_7digitos = as.numeric(paste0(uf,V0002)),
    pop_negra = case_when(raca %in% c(2,4) ~ 1, TRUE ~ 0),
    pop_rural = case_when(situacao_dom == 2 ~ 1, TRUE ~ 0),
    pop_negra_jovem = case_when(pop_negra == 1 & idade %in% 15:29 ~ 1,TRUE ~ 0)
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
  left_join(
    df_pop_2010 %>%
      filter(idade %in% 15:29) %>%
      group_by(cod_munic_7digitos) %>%
      reframe(
        pop_negra = 9,
        n = sum(peso)
      ) %>%
      bind_rows(
        df_pop_2010 %>%
          filter(idade %in% 15:29) %>%
          group_by(cod_munic_7digitos, pop_negra) %>%
          reframe(n = sum(peso))
      ) %>%
      pivot_wider(names_from = pop_negra, values_from = n) %>%
      mutate(
        prop_pop_negra_jovem = round((`1`/`9`)*100,2)
      ) %>%
      select(cod_munic_7digitos, prop_pop_negra_jovem),
    by = c("cod_munic_7digitos"),
    keep = FALSE
  ) %>%
  left_join(
    df_pop_2010 %>%
      group_by(cod_munic_7digitos) %>%
      reframe(
        pop_negra_jovem = 9,
        n = sum(peso)
      ) %>%
      bind_rows(
        df_pop_2010 %>%
          group_by(cod_munic_7digitos, pop_negra_jovem) %>%
          reframe(n = sum(peso))
      ) %>%
      pivot_wider(names_from = pop_negra_jovem, values_from = n) %>%
      mutate(
        prop_pop_negra_jovem_total = round((`1`/`9`)*100,2)
      ) %>%
      select(cod_munic_7digitos, prop_pop_negra_jovem_total),
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
  left_join(
    df_pop_2010 %>%
      filter(idade %in% 15:29) %>%
      group_by(uf) %>%
      reframe(
        pop_negra = 9,
        n = sum(peso)
      ) %>%
      bind_rows(
        df_pop_2010 %>%
          filter(idade %in% 15:29) %>%
          group_by(uf, pop_negra) %>%
          reframe(n = sum(peso))
      ) %>%
      pivot_wider(names_from = pop_negra, values_from = n) %>%
      mutate(
        prop_pop_negra_jovem = round((`1`/`9`)*100,2)
      ) %>%
      select(uf, prop_pop_negra_jovem),
    by = c("uf"),
    keep = FALSE
  ) %>%
  left_join(
    df_pop_2010 %>%
      group_by(uf) %>%
      reframe(
        pop_negra_jovem = 9,
        n = sum(peso)
      ) %>%
      bind_rows(
        df_pop_2010 %>%
          group_by(uf, pop_negra_jovem) %>%
          reframe(n = sum(peso))
      ) %>%
      pivot_wider(names_from = pop_negra_jovem, values_from = n) %>%
      mutate(
        prop_pop_negra_jovem_total = round((`1`/`9`)*100,2)
      ) %>%
      select(uf, prop_pop_negra_jovem_total),
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
  bind_cols(
    df_pop_2010 %>%
      filter(idade %in% 15:29) %>%
      reframe(
        pop_negra = 9,
        n = sum(peso)
      ) %>%
      bind_rows(
        df_pop_2010 %>%
          filter(idade %in% 15:29) %>%
          group_by(pop_negra) %>%
          reframe(n = sum(peso))
      ) %>%
      pivot_wider(names_from = pop_negra, values_from = n) %>%
      mutate(
        prop_pop_negra_jovem = round((`1`/`9`)*100,2)
      ) %>%
      select(prop_pop_negra_jovem)
  ) %>%
  bind_cols(
    df_pop_2010 %>%
      reframe(
        pop_negra_jovem = 9,
        n = sum(peso)
      ) %>%
      bind_rows(
        df_pop_2010 %>%
          group_by(pop_negra_jovem) %>%
          reframe(n = sum(peso))
      ) %>%
      pivot_wider(names_from = pop_negra_jovem, values_from = n) %>%
      mutate(
        prop_pop_negra_jovem_total = round((`1`/`9`)*100,2)
      ) %>%
      select(prop_pop_negra_jovem_total)
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
    indicador_pop_negra_jovem = prop_pop_negra_jovem,
    prop_pop_negra_jovem = prop_pop_negra_jovem_total
  )


# Analise exploratória ----------------------------------------------------

# Por UF

df_pop_2010_indicadores %>%
  filter(nivel_geografico == "UF") %>%
  ggplot() +
  aes(x = indicador_pop_negra_jovem, y = indicador_pop_negra) +
  geom_point(shape = 21, size = 2, alpha = .8, fill = "#2b8cbe") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    x = "População negra jovem (%)",
    y = "População negra total (%)",
    title = "Correlação entre População negra total e jovem por UF"
  ) +
  coord_cartesian(xlim = c(0,100), ylim = c(0,100)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  scale_x_continuous(breaks = seq(0,100,10)) +
  theme_light()

# Por município

df_pop_2010_indicadores %>%
  filter(nivel_geografico == "Municipio") %>%
  ggplot() +
  aes(x = indicador_pop_negra_jovem, y = indicador_pop_negra) +
  geom_point(shape = 21, size = 2, alpha = .3, fill = "#2b8cbe") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    x = "População negra jovem (%)",
    y = "População negra total (%)",
    title = "Correlação entre População negra total e jovem por Municipio"
  ) +
  coord_cartesian(xlim = c(0,100), ylim = c(0,100)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  scale_x_continuous(breaks = seq(0,100,10)) +
  theme_light()

# Por município com cores por UF

df_pop_2010_indicadores_plot <- df_pop_2010_indicadores %>%
  filter(nivel_geografico == "Municipio") %>%
  mutate(
    uf = str_sub(codigo_geografico, 1,2),
    regiao = str_sub(codigo_geografico,1,1),
    regiao = factor(
      regiao,
      levels = c(1,2,3,4,5),
      labels = c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste")
    )
  )

cods_uf <- tibble(cod = c("11","12","13","14","15","16","17","21","22","23","24","25","26","27",
                          "28","29","31","32","33","35","41","42","43","50","51","52","53"),
                  names = c("Rondônia","Acre","Amazonas", "Roraima","Pará","Amapá","Tocantins","Maranhão",
                            "Piauí", "Ceará","Rio Grande do Norte","Paraíba","Pernambuco","Alagoas","Sergipe",
                            "Bahia","Minas Gerais","Espírito Santo","Rio de Janeiro","São Paulo","Paraná","Santa Catarina",
                            "Rio Grande do Sul","Mato Grosso do Sul","Mato Grosso","Goiás","Distrito Federal"))

df_pop_2010_indicadores_plot <- df_pop_2010_indicadores_plot |>
  mutate(uf = factor(uf, levels = cods_uf$cod, labels = cods_uf$names))

df_pop_2010_indicadores_plot %>%
  ggplot() +
  aes(x = indicador_pop_negra_jovem, y = indicador_pop_negra, fill = uf) +
  geom_point(size = 2, shape = 21, alpha = .5) +
  geom_abline(intercept = 0, slope = 1, linewidth = 1.1, color = "red", linetype = "dashed") +
  labs(
    x = "População negra jovem (%)",
    y = "População negra total (%)",
    title = "Correlação entre População negra total e jovem por Municipio"
  ) +
  coord_cartesian(xlim = c(0,100), ylim = c(0,100)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  scale_x_continuous(breaks = seq(0,100,10)) +
  lemon::facet_rep_wrap(regiao ~ ., repeat.tick.labels = TRUE) +
  theme_light() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 6),
    legend.position = 'right'
  )

# Exportacao dos dados ----------------------------------------------------

write_parquet(df_pop_2010_indicadores, sink = paste0("./output/base de dados - desigualdade/pop_2010_indicadores.parquet"))
write_csv2(df_pop_2010_indicadores, file = paste0("./output/base de dados - desigualdade/pop_2010_indicadores.csv"))
