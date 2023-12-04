#' ------------------------------------------------------
#' @author Thiago Cordeiro Almeida
#' @last-update 2023-12-03
#' @description Construção da lista do top 50 após a avaliação final do Danilo
#' @update-description Inclusão dos indicadores de mortalidade e pop. total
#' -----------------------------------------------------
options(scipen = 9999999)
rm(list = ls())
gc()

# bibliotecas -------------------------------------------------------------

if(!require("pacman")) install.packages(("pacman"))
pacman::p_load(tidyverse, arrow, openxlsx, readxl)

# Importacao dos dados ----------------------------------------------------

DIR <- "./output/base de dados - violencia e desigualdade"

# leitura dos dados

df_municipios <- readxl::read_xlsx(
  path = "./output/municipios - ranking final.xlsx",
  range = "B1:D51"
)

# leitura das bases de denominador

df <- read_parquet(file = file.path(DIR, "base_violencia_desigualdade.parquet"))
df_pop_2010 <- read_parquet(file = "./output/base de dados - desigualdade/pop_2010_indicadores.parquet")

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

# Juncao dos dados de 2010 com os dados de desigualdade e violencia -------

df <- df %>%
  left_join(
    df_pop_2010 %>%
      mutate(
        cd_municipio_6digitos = as.numeric(str_sub(codigo_geografico, 1,6)),
        regiao = str_sub(codigo_geografico, 1,1),
        uf = str_sub(codigo_geografico, 1,2),
        uf = factor(uf, levels = cods_uf$cod, labels = cods_uf$names),
        prop_pop_negra_jovem = prop_pop_negra_jovem/100
      ) %>%
      select(-c(nivel_geografico, codigo_geografico, starts_with("indicador_"))),
    by = c("regiao", "uf","cd_municipio_6digitos"),
    keep = FALSE
  )

# criando variavel de populacao jovem negra = pop_total * prop_pop_jovem_negra

df <- df %>%
  mutate(pop_jovem_negra = pop_total * prop_pop_negra_jovem)


# Criando lista com base nos municipios -----------------------------------

top50_homicidios_negros_jovens <- df_municipios %>%
  left_join(
    df |>
      group_by(cd_municipio_6digitos) |>
      reframe(
        nome_municipio = nome_municipio,
        ordem = row_number(),
        ano = ano,
        regiao = regiao,
        uf = uf,
        homicidios = sum(homicidios_geral),
        # intervencoes = sum(intervencao_geral),
        pop_total = pop_total,
        pop_jovem_negra = pop_jovem_negra,
        indicador_homicidios = homicidios/pop_jovem_negra*1000,
        # indicador_intervencoes = intervencoes/pop_total*1000
      ) |>
      filter(ordem == 1) %>%
      select(-c(ano, ordem)),
    by = c("municipio" = "nome_municipio", "uf" = "uf")
  )

# Exportacao dos dados de homicidio e intervencao -------------------------

write_csv(top50_homicidios_negros_jovens, file = "./output/resultados/resultados - ranking genocidio populacao negra - homicidios da população jovem negra [lista final].csv")
write_parquet(top50_homicidios_negros_jovens, sink = "./output/resultados/resultados - ranking genocidio populacao negra - homicidios da população jovem negra [lista final].parquet")
