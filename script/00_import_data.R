options(scipen = 999999)


# Libraries ---------------------------------------------------------------

packages_used <- c("tidyverse",
              "DDM", #para aplicar os métodos DDM
              "brpop", # estimativas da pop. observada por idade e sexo por UF
              "microdatasus") # dados dos registros de obito e nascidos vivos

lapply(packages_used, require, character.only = TRUE)

rm(list = ls())

# Dados de óbito -------------------------------------------------------------

# Importação

dados_sim_00 <- fetch_datasus(year_start = 2000,
                           year_end = 2000,
                           information_system = "SIM-DO")

dados_sim_00 <- process_sim(dados_sim_00, municipality_data = FALSE)

dados_sim_10 <- fetch_datasus(year_start = 2010,
                              year_end = 2010,
                              information_system = "SIM-DO")

dados_sim_10 <- process_sim(dados_sim_10, municipality_data = FALSE)

# Processamento

dados_sim_00 <- dados_sim_00 |>
  select(UF = CODMUNRES, SEXO, idade = IDADEanos, DTOBITO) |>
  mutate(ano = lubridate::year(DTOBITO),
         UF = stringr::str_sub(UF, end = -6),
         IDADE = as.numeric(idade)) |>
  select(-c(DTOBITO, idade)) |>
  select(ANO = ano, UF, SEXO, IDADE)


dados_sim_10 <- dados_sim_10 |>
  select(UF = CODMUNRES, SEXO, idade = IDADEanos, DTOBITO) |>
  mutate(ano = lubridate::year(DTOBITO),
         UF = stringr::str_sub(UF, end = -5),
         IDADE = as.numeric(idade)) |>
  select(-c(DTOBITO, idade)) |>
  select(ANO = ano, UF, SEXO, IDADE)

save(dados_sim_00, file = "./Trabalho Final/data/sim_2000_idade_sexo_uf.RData")
save(dados_sim_10, file = "./Trabalho Final/data/sim_2010_idade_sexo_uf.RData")

## Tabelas

# 2000

dados_sim_00_BR <- dados_sim_00 |>
  mutate(GRUPO_ETARIO = cut(IDADE,
                            breaks = seq(0,125,5),
                            right = FALSE)) |>
  mutate(GRUPO_ETARIO = str_extract(str_remove(GRUPO_ETARIO, pattern = "\\["), pattern = "^\\d+")) |>
  mutate(GRUPO_ETARIO = as.integer(GRUPO_ETARIO)) |>
  mutate(GRUPO_ETARIO = case_when(GRUPO_ETARIO >= 85 ~ 85, TRUE ~ GRUPO_ETARIO)) |>
  group_by(SEXO, GRUPO_ETARIO) |>
  count() |>
  summarise(ANO = 2000,
            UF = 'BR',
            OBITOS = sum(n)) |>
  select(ANO, UF, SEXO, GRUPO_ETARIO, OBITOS)

dados_sim_00_UF <- dados_sim_00 |>
  mutate(GRUPO_ETARIO = cut(IDADE,
                            breaks = seq(0,125,5),
                            right = FALSE)) |>
  mutate(GRUPO_ETARIO = str_extract(str_remove(GRUPO_ETARIO, pattern = "\\["), pattern = "^\\d+")) |>
  mutate(GRUPO_ETARIO = as.integer(GRUPO_ETARIO)) |>
  mutate(GRUPO_ETARIO = case_when(GRUPO_ETARIO >= 85 ~ 85, TRUE ~ GRUPO_ETARIO)) |>
  group_by(UF, SEXO, GRUPO_ETARIO) |>
  count() |>
  summarise(ANO = 2000,
            OBITOS = sum(n)) |>
  select(ANO, UF, SEXO, GRUPO_ETARIO, OBITOS)

obitos_00 <- dados_sim_00_BR |> bind_rows(dados_sim_00_UF)

save(obitos_00, file = "./Trabalho Final/data/obitos_2000_idade_sexo_uf.RData")

# 2010

dados_sim_10_BR <- dados_sim_10 |>
  mutate(GRUPO_ETARIO = cut(IDADE,
                            breaks = seq(0,125,5),
                            right = FALSE)) |>
  mutate(GRUPO_ETARIO = str_extract(str_remove(GRUPO_ETARIO, pattern = "\\["), pattern = "^\\d+")) |>
  mutate(GRUPO_ETARIO = as.integer(GRUPO_ETARIO)) |>
  mutate(GRUPO_ETARIO = case_when(GRUPO_ETARIO >= 85 ~ 85, TRUE ~ GRUPO_ETARIO)) |>
  group_by(SEXO, GRUPO_ETARIO) |>
  count() |>
  summarise(ANO = 2010,
            UF = 'BR',
            OBITOS = sum(n)) |>
  select(ANO, UF, SEXO, GRUPO_ETARIO, OBITOS)

dados_sim_10_UF <- dados_sim_10 |>
  mutate(GRUPO_ETARIO = cut(IDADE,
                            breaks = seq(0,125,5),
                            right = FALSE)) |>
  mutate(GRUPO_ETARIO = str_extract(str_remove(GRUPO_ETARIO, pattern = "\\["), pattern = "^\\d+")) |>
  mutate(GRUPO_ETARIO = as.integer(GRUPO_ETARIO)) |>
  mutate(GRUPO_ETARIO = case_when(GRUPO_ETARIO >= 85 ~ 85, TRUE ~ GRUPO_ETARIO)) |>
  group_by(UF, SEXO, GRUPO_ETARIO) |>
  count() |>
  summarise(ANO = 2010,
            OBITOS = sum(n)) |>
  select(ANO, UF, SEXO, GRUPO_ETARIO, OBITOS)

obitos_10 <- dados_sim_10_BR |> bind_rows(dados_sim_10_UF)

save(obitos_10, file = "./Trabalho Final/data/obitos_2010_idade_sexo_uf.RData")


# Dados de nascidos-vivos (SINASC) ----------------------------------------

# Importação dos dados

dados_sinasc_00 <- fetch_datasus(year_start = 2000,
                              year_end = 2000,
                              information_system = "SINASC")

dados_sinasc_00 <- dados_sinasc_00 |> select(-CODOCUPMAE)

dados_sinasc_00 <- process_sinasc(dados_sinasc_00_mod, municipality_data = FALSE)

dados_sinasc_10 <- fetch_datasus(year_start = 2010,
                                 year_end = 2010,
                                 information_system = "SINASC")

dados_sinasc_10 <- process_sinasc(dados_sinasc_10, municipality_data = FALSE)

# Processamento

dados_sinasc_00 <- dados_sinasc_00 |>
  select(UF = CODMUNNASC, SEXO, IDADEMAE) |>
  mutate(ano = 2000,
         UF = stringr::str_sub(UF, end = -6),
         IDADEMAE = as.numeric(IDADEMAE)) |>
  select(ANO = ano, UF, SEXO, IDADEMAE)


dados_sinasc_10 <- dados_sinasc_10 |>
  select(UF = CODMUNNASC, SEXO, IDADEMAE) |>
  mutate(ano = 2010,
         UF = stringr::str_sub(UF, end = -5),
         IDADEMAE = as.numeric(IDADEMAE)) |>
  select(ANO = ano, UF, SEXO, IDADEMAE)

save(dados_sinasc_00, file = "./Trabalho Final/data/sinasc_2000_idade_sexo_uf.RData")
save(dados_sinasc_10, file = "./Trabalho Final/data/sinasc_2010_idade_sexo_uf.RData")

## Tabelas

# 2000

dados_sinasc_00_BR <- dados_sinasc_00 |>
  mutate(GRUPO_ETARIO = cut(IDADEMAE,
                            breaks = seq(0,125,5),
                            right = FALSE)) |>
  mutate(GRUPO_ETARIO = str_extract(str_remove(GRUPO_ETARIO, pattern = "\\["), pattern = "^\\d+")) |>
  mutate(GRUPO_ETARIO = as.integer(GRUPO_ETARIO)) |>
  mutate(GRUPO_ETARIO = case_when(GRUPO_ETARIO >= 85 ~ 85, TRUE ~ GRUPO_ETARIO)) |>
  group_by(SEXO, GRUPO_ETARIO) |>
  count() |>
  summarise(ANO = 2000,
            UF = 'BR',
            NASCIMENTOS = sum(n)) |>
  select(ANO, UF, SEXO, GRUPO_ETARIO, NASCIMENTOS)

dados_sinasc_00_UF <- dados_sinasc_00 |>
  mutate(GRUPO_ETARIO = cut(IDADEMAE,
                            breaks = seq(0,125,5),
                            right = FALSE)) |>
  mutate(GRUPO_ETARIO = str_extract(str_remove(GRUPO_ETARIO, pattern = "\\["), pattern = "^\\d+")) |>
  mutate(GRUPO_ETARIO = as.integer(GRUPO_ETARIO)) |>
  mutate(GRUPO_ETARIO = case_when(GRUPO_ETARIO >= 85 ~ 85, TRUE ~ GRUPO_ETARIO)) |>
  group_by(UF, SEXO, GRUPO_ETARIO) |>
  count() |>
  summarise(ANO = 2000,
            NASCIMENTOS = sum(n)) |>
  select(ANO, UF, SEXO, GRUPO_ETARIO, NASCIMENTOS)

nascimentos_00 <- dados_sinasc_00_BR |> bind_rows(dados_sinasc_00_UF)

save(nascimentos_00, file = "./Trabalho Final/data/nascimentos_2000_idade_sexo_uf.RData")

# 2010

dados_sinasc_10_BR <- dados_sinasc_10 |>
  mutate(GRUPO_ETARIO = cut(IDADEMAE,
                            breaks = seq(0,125,5),
                            right = FALSE)) |>
  mutate(GRUPO_ETARIO = str_extract(str_remove(GRUPO_ETARIO, pattern = "\\["), pattern = "^\\d+")) |>
  mutate(GRUPO_ETARIO = as.integer(GRUPO_ETARIO)) |>
  mutate(GRUPO_ETARIO = case_when(GRUPO_ETARIO >= 85 ~ 85, TRUE ~ GRUPO_ETARIO)) |>
  group_by(SEXO, GRUPO_ETARIO) |>
  count() |>
  summarise(ANO = 2010,
            UF = 'BR',
            NASCIMENTOS = sum(n)) |>
  select(ANO, UF, SEXO, GRUPO_ETARIO, NASCIMENTOS)

dados_sinasc_10_UF <- dados_sinasc_10 |>
  mutate(GRUPO_ETARIO = cut(IDADEMAE,
                            breaks = seq(0,125,5),
                            right = FALSE)) |>
  mutate(GRUPO_ETARIO = str_extract(str_remove(GRUPO_ETARIO, pattern = "\\["), pattern = "^\\d+")) |>
  mutate(GRUPO_ETARIO = as.integer(GRUPO_ETARIO)) |>
  mutate(GRUPO_ETARIO = case_when(GRUPO_ETARIO >= 85 ~ 85, TRUE ~ GRUPO_ETARIO)) |>
  group_by(UF, SEXO, GRUPO_ETARIO) |>
  count() |>
  summarise(ANO = 2010,
            NASCIMENTOS = sum(n)) |>
  select(ANO, UF, SEXO, GRUPO_ETARIO, NASCIMENTOS)

nascimentos_10 <- dados_sinasc_10_BR |> bind_rows(dados_sinasc_10_UF)

save(nascimentos_10, file = "./Trabalho Final/data/nascimentos_2010_idade_sexo_uf.RData")

# Dados população residente -----------------------------------------------

# Importação

dados_pop <- brpop::uf_sex_pop() |>
  filter(year == 2000 | year == 2010) |>
  mutate(sex = case_when(sex == "Male" ~ "Masculino", TRUE ~ "Feminino")) |>
  select(ANO = year, UF = uf, SEXO = sex, GRUPO_ETARIO = age_group, POP = pop) |>
  arrange(ANO,UF,SEXO,GRUPO_ETARIO) |>
  filter(GRUPO_ETARIO != "Total") |>
  mutate(GRUPO_ETARIO = rep(seq(0,80,5),108))


# Tratamento

pop_BR <- dados_pop |>
  group_by(ANO, SEXO, GRUPO_ETARIO) |>
  summarise(UF = 'BR',
            POP = sum(POP))

pop_UF <- dados_pop |>
  mutate(UF = as.character(UF))

pop <- pop_BR |> select(ANO, UF, SEXO, GRUPO_ETARIO, POP) |> bind_rows(pop_UF)

save(pop, file = "./Trabalho Final/data/pop_uf_idade_sexo.RData")
