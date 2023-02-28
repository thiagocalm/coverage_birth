options(scipen = 999999)


# Bibliotecas -------------------------------------------------------------

library(tidyverse)


# Óbitos ------------------------------------------------------------------

# Import data

load("./data/obitos_2010_idade_sexo_uf.RData")
load("./data/obitos_2000_idade_sexo_uf.RData")

### Teste para BR

obitos_00_br <- obitos_00 |> filter(UF == "BR")
obitos_10_br <- obitos_10 |> filter(UF == "BR")

# Calculando as taxas de distribuição de óbitos para idade

M <- obitos_00_br  |>
  bind_rows(obitos_10_br) |>
  mutate(
    M_idade = case_when(is.na(GRUPO_ETARIO) ~ 1, TRUE ~ 0),
    M_sexo = case_when(is.na(SEXO) ~ 1, TRUE ~ 0),
    M_idade_sexo = case_when(M_idade == 1 & M_sexo == 1 ~ 1, TRUE ~ 0))

obitos_br <- M  |>
  #Por idade e sexo
  mutate(
    NM_O = case_when(M_idade_sexo == 0 ~ OBITOS, TRUE ~ 0),
    M_O = case_when(M_idade_sexo == 1 ~ OBITOS, TRUE ~ 0)
  ) |>
  group_by(ANO, UF) |>
  mutate(TOTAL_NM = sum(NM_O),
         TOTAL_M = sum(M_O)) |>
  ungroup() |>
  mutate(K = NM_O/TOTAL_NM,
         OBITOS_IS = NM_O + (TOTAL_M * K)) |>
  # Por sexo
  mutate(
    NM_O = case_when(M_sexo == 0 ~ OBITOS_IS, TRUE ~ 0),
    M_O = case_when(M_sexo == 1 ~ OBITOS_IS, TRUE ~ 0)
    ) |>
  group_by(ANO, UF, GRUPO_ETARIO) |>
  mutate(TOTAL_NM = sum(NM_O),
         TOTAL_M = sum(M_O)) |>
  ungroup() |>
  mutate(K = NM_O/TOTAL_NM,
         OBITOS_S = OBITOS_IS + (TOTAL_M * K)) |>
  # Por idade
  mutate(
    NM_O = case_when(M_idade == 0 ~ OBITOS_S, TRUE ~ 0),
    M_O = case_when(M_idade == 1 ~ OBITOS_S, TRUE ~ 0)
  ) |>
  group_by(ANO, UF, SEXO) |>
  mutate(TOTAL_NM = sum(NM_O),
         TOTAL_M = sum(M_O)) |>
  ungroup() |>
  mutate(K = NM_O/TOTAL_NM,
         OBITOS_I = OBITOS_S + (TOTAL_M * K)) |>
  filter(!is.na(SEXO) & !is.na(GRUPO_ETARIO)) |>
  select(ANO, UF, SEXO, GRUPO_ETARIO, OBITOS = OBITOS_I)

### Replicando para todas as UFs

# Calculando as taxas de distribuição de óbitos para idade

M <- obitos_00  |>
  bind_rows(obitos_10) |>
  mutate(
    M_idade = case_when(is.na(GRUPO_ETARIO) ~ 1, TRUE ~ 0),
    M_sexo = case_when(is.na(SEXO) ~ 1, TRUE ~ 0),
    M_idade_sexo = case_when(M_idade == 1 & M_sexo == 1 ~ 1, TRUE ~ 0))

obitos <- M  |>
  #Por idade e sexo
  mutate(
    NM_O = case_when(M_idade_sexo == 0 ~ OBITOS, TRUE ~ 0),
    M_O = case_when(M_idade_sexo == 1 ~ OBITOS, TRUE ~ 0)
  ) |>
  group_by(ANO, UF) |>
  mutate(TOTAL_NM = sum(NM_O),
         TOTAL_M = sum(M_O)) |>
  ungroup() |>
  mutate(K = NM_O/TOTAL_NM,
         OBITOS_IS = NM_O + (TOTAL_M * K)) |>
  # Por sexo
  mutate(
    NM_O = case_when(M_sexo == 0 ~ OBITOS_IS, TRUE ~ 0),
    M_O = case_when(M_sexo == 1 ~ OBITOS_IS, TRUE ~ 0)
  ) |>
  group_by(ANO, UF, GRUPO_ETARIO) |>
  mutate(TOTAL_NM = sum(NM_O),
         TOTAL_M = sum(M_O)) |>
  ungroup() |>
  mutate(K = NM_O/TOTAL_NM,
         OBITOS_S = OBITOS_IS + (TOTAL_M * K)) |>
  # Por idade
  mutate(
    NM_O = case_when(M_idade == 0 ~ OBITOS_S, TRUE ~ 0),
    M_O = case_when(M_idade == 1 ~ OBITOS_S, TRUE ~ 0)
  ) |>
  group_by(ANO, UF, SEXO) |>
  mutate(TOTAL_NM = sum(NM_O),
         TOTAL_M = sum(M_O)) |>
  ungroup() |>
  mutate(K = NM_O/TOTAL_NM,
         OBITOS_I = OBITOS_S + (TOTAL_M * K)) |>
  filter(!is.na(SEXO) & !is.na(GRUPO_ETARIO)) |>
  select(ANO, UF, SEXO, GRUPO_ETARIO, OBITOS = OBITOS_I)

save(obitos, file = "./data/obitos_idade_sexo_uf_tratados.RData")


# Nascidos-vivos -------------------------------------------------------------

load("./data/nascimentos_2010_idade_sexo_uf.RData")
load("./data/nascimentos_2000_idade_sexo_uf.RData")

### Teste para BR

nascimentos_00_br <- nascimentos_00 |> filter(UF == "BR")
nascimentos_10_br <- nascimentos_10 |> filter(UF == "BR")

# Calculando as taxas de distribuição de óbitos para idade

M <- nascimentos_00_br  |>
  bind_rows(nascimentos_10_br) |>
  mutate(
    M_idade = case_when(is.na(GRUPO_ETARIO) ~ 1, TRUE ~ 0),
    M_sexo = case_when(is.na(SEXO) ~ 1, TRUE ~ 0),
    M_idade_sexo = case_when(M_idade == 1 & M_sexo == 1 ~ 1, TRUE ~ 0))

nascimentos_br <- M  |>
  #Por idade e sexo
  mutate(
    NM_O = case_when(M_idade_sexo == 0 ~ NASCIMENTOS, TRUE ~ 0),
    M_O = case_when(M_idade_sexo == 1 ~ NASCIMENTOS, TRUE ~ 0)
  ) |>
  group_by(ANO, UF) |>
  mutate(TOTAL_NM = sum(NM_O),
         TOTAL_M = sum(M_O)) |>
  ungroup() |>
  mutate(K = NM_O/TOTAL_NM,
         NASCIMENTOS_IS = NM_O + (TOTAL_M * K)) |>
  # Por sexo
  mutate(
    NM_O = case_when(M_sexo == 0 ~ NASCIMENTOS_IS, TRUE ~ 0),
    M_O = case_when(M_sexo == 1 ~ NASCIMENTOS_IS, TRUE ~ 0)
  ) |>
  group_by(ANO, UF, GRUPO_ETARIO) |>
  mutate(TOTAL_NM = sum(NM_O),
         TOTAL_M = sum(M_O)) |>
  ungroup() |>
  mutate(K = NM_O/TOTAL_NM,
         NASCIMENTOS_S = NASCIMENTOS_IS + (TOTAL_M * K)) |>
  # Por idade
  mutate(
    NM_O = case_when(M_idade == 0 ~ NASCIMENTOS_S, TRUE ~ 0),
    M_O = case_when(M_idade == 1 ~ NASCIMENTOS_S, TRUE ~ 0)
  ) |>
  group_by(ANO, UF, SEXO) |>
  mutate(TOTAL_NM = sum(NM_O),
         TOTAL_M = sum(M_O)) |>
  ungroup() |>
  mutate(K = NM_O/TOTAL_NM,
         NASCIMENTOS_I = NASCIMENTOS_S + (TOTAL_M * K)) |>
  filter(!is.na(SEXO) & !is.na(GRUPO_ETARIO)) |>
  select(ANO, UF, SEXO, GRUPO_ETARIO, NASCIMENTOS = NASCIMENTOS_I)

### Replicando para todas as UFs

# Calculando as taxas de distribuição de óbitos para idade

M <- nascimentos_00  |>
  bind_rows(nascimentos_10) |>
  mutate(
    M_idade = case_when(is.na(GRUPO_ETARIO) ~ 1, TRUE ~ 0),
    M_sexo = case_when(is.na(SEXO) ~ 1, TRUE ~ 0),
    M_idade_sexo = case_when(M_idade == 1 & M_sexo == 1 ~ 1, TRUE ~ 0))

nascimentos <- M  |>
  #Por idade e sexo
  mutate(
    NM_O = case_when(M_idade_sexo == 0 ~ NASCIMENTOS, TRUE ~ 0),
    M_O = case_when(M_idade_sexo == 1 ~ NASCIMENTOS, TRUE ~ 0)
  ) |>
  group_by(ANO, UF) |>
  mutate(TOTAL_NM = sum(NM_O),
         TOTAL_M = sum(M_O)) |>
  ungroup() |>
  mutate(K = NM_O/TOTAL_NM,
         NASCIMENTOS_IS = NM_O + (TOTAL_M * K)) |>
  # Por sexo
  mutate(
    NM_O = case_when(M_sexo == 0 ~ NASCIMENTOS_IS, TRUE ~ 0),
    M_O = case_when(M_sexo == 1 ~ NASCIMENTOS_IS, TRUE ~ 0)
  ) |>
  group_by(ANO, UF, GRUPO_ETARIO) |>
  mutate(TOTAL_NM = sum(NM_O),
         TOTAL_M = sum(M_O)) |>
  ungroup() |>
  mutate(K = NM_O/TOTAL_NM,
         NASCIMENTOS_S = NASCIMENTOS_IS + (TOTAL_M * K)) |>
  # Por idade
  mutate(
    NM_O = case_when(M_idade == 0 ~ NASCIMENTOS_S, TRUE ~ 0),
    M_O = case_when(M_idade == 1 ~ NASCIMENTOS_S, TRUE ~ 0)
  ) |>
  group_by(ANO, UF, SEXO) |>
  mutate(TOTAL_NM = sum(NM_O),
         TOTAL_M = sum(M_O)) |>
  ungroup() |>
  mutate(K = NM_O/TOTAL_NM,
         NASCIMENTOS_I = NASCIMENTOS_S + (TOTAL_M * K)) |>
  filter(!is.na(SEXO) & !is.na(GRUPO_ETARIO)) |>
  select(ANO, UF, SEXO, GRUPO_ETARIO, NASCIMENTOS = NASCIMENTOS_I)

save(nascimentos, file = "./data/nascimentos_idade_sexo_uf_tratados.RData")
