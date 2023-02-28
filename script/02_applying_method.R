options(scipen = 999999)


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(DDM)
library(ggalt)
library(brazilmaps)


# Import data -------------------------------------------------------------

load("./data/nascimentos_idade_sexo_uf_tratados.RData")
load("./data/obitos_idade_sexo_uf_tratados.RData")
load("./data/pop_uf_idade_sexo.RData")


# Handling data for the method application --------------------------------

# Restringindo grupo etário aberto a 80+

obitos <- obitos |>
  mutate(GRUPO_ETARIO = case_when(GRUPO_ETARIO >= 80 ~ 80, TRUE ~ GRUPO_ETARIO)) |>
  group_by(ANO, UF, SEXO, GRUPO_ETARIO) |>
  summarise(OBITOS = sum(OBITOS))

# Reordenando UFs
obitos <- obitos |> filter(UF == "BR") |>
  bind_rows(obitos |> filter(UF != "BR"))

# Unindo pop a obitos

obitos_pop <- obitos |>
  bind_cols(pop) |>
  select(ANO = ANO...1, UF = UF...2, SEXO = SEXO...3, GRUPO_ETARIO = GRUPO_ETARIO...4,
         OBITOS, POP)

obitos_mean <- obitos_pop |>
  filter(UF == "BR") |>
  group_by(UF, SEXO, GRUPO_ETARIO) |>
  summarise(OBITOS_mean = mean(OBITOS)) |>
  bind_rows(
    obitos_pop |>
      filter(UF != "BR") |>
      group_by(UF, SEXO, GRUPO_ETARIO) |>
      summarise(OBITOS_mean = mean(OBITOS))
  )

# DDM application ---------------------------------------------------------

cods_uf <- tibble(cod = c("11","12","13","14","15","16","17","21","22","23","24","25","26","27",
                    "28","29","31","32","33","35","41","42","43","50","51","52","53","BR"),
                  names = c("Rondônia","Acre","Amazonas", "Roraima","Pará","Amapá","Tocantins","Maranhão",
                            "Piauí", "Ceará","Rio Grande do Norte","Paraíba","Pernambuco","Alagoas","Sergipe",
                            "Bahia","Minas Gerais","Espírito Santo","Rio de Janeiro","São Paulo","Paraná","Santa Catarina",
                            "Rio Grande do Sul","Mato Grosso do Sul","Mato Grosso","Goiás","Distrito Federal","Brasil"))


####
# Female - 2000
####

# Handling data

obitos_pop_female <- obitos_pop |>
  filter(SEXO == "Feminino") |>
  filter(ANO == 2000) |>
  select(cod = UF, pop1 = POP, year1 = ANO, age = GRUPO_ETARIO, sex = SEXO) |>
  bind_cols(
    obitos_pop |>
      filter(SEXO == "Feminino") |>
      filter(ANO == 2010) |>
      select(pop2 = POP, year2 = ANO)
  ) |>
  bind_cols(
    obitos_mean |>
      filter(SEXO == "Feminino") |>
      select(deaths = OBITOS_mean)
  ) |>
  select(cod, pop1, pop2, deaths, year1, year2, age, sex) |>
  mutate(sex = "f",
         cod = factor(cod, levels = cods_uf$cod, labels = cods_uf$names))

# Calculating growth rate

obitos_pop_female_rx <- obitos_pop_female |>
  group_by(cod) |>
  mutate(cum_pop1 = rev(cumsum(rev(pop1))),
         cum_pop2 = rev(cumsum(rev(pop2))),
         rx = (1/(year2-year1)*log(cum_pop2/cum_pop1))) |>
  select(-c(cum_pop1, cum_pop2))

# Applicating DDM method

ddm_female <- DDM::ddm(obitos_pop_female)

ddm_female <- ddm_female |>
  group_by(cod) |>
  mutate(mean = (ggb+seg+ggbseg)/3,
         mean_2 = (ggb+ggbseg)/2)

ddm_female |>
  select(cod, ggb, seg, ggbseg, mean, mean_2) |>
  pivot_longer("ggb":"mean_2", names_to = "method", values_to = "coverage") |>
  ggplot() +
  aes(x = cod, y = coverage, color = method) +
  geom_hline(yintercept = 1,linetype = "dashed", color = "black", linewidth = 2) +
  geom_segment(aes(x=cod,
                   xend=cod,
                   y=min(coverage),
                   yend=max(coverage)),
               linetype="dashed",
               linewidth=0.1, color = "grey") +
  geom_point(size = 4) +
  coord_flip() +
  labs(
    title = "Grau de cobertura dos óbitos segundo diferentes métodos DDM, por UF",
    subtitle = "Brasil (2000-2010), sexo feminino.",
    x = "UF",
    y = "Grau de cobertura dos óbitos"
  ) +
  scale_y_continuous(breaks = seq(.4,1.6,.1)) +
  theme_classic() +
  scale_color_manual(values = c("#35978f","#4d4d4d", "#993404", "#54278f", "#fdae61")) +
  guides(color = guide_legend(title = "Método"), shape = guide_legend(title = "Método")) +
  theme(
    plot.title = element_text(face = "bold", size = 10, hjust = 0),
    legend.title = element_text(face = "bold", size = 9, hjust = 0, vjust = .5),
    legend.text = element_text(size = 8, hjust = 0, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 9, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
    # panel.grid = element_line(color = "#f0f0f0",linewidth = .01,)
  )

ddm_female <- ddm_female |>
  group_by(cod) |>
  mutate(
    coverage = case_when(
      mean_2 < 1 ~ mean_2,
      mean_2 > 1 & ggbseg < 1 ~ ggbseg,
      mean_2 > 1 & ggbseg > 1 & ggb < 1 ~ ggb,
      TRUE ~ seg))

# Correcting deaths registration by coverage factor

obitos_pop_female_adj <- ddm_female |>
  select(cod, coverage) |>
  right_join(obitos_pop_female_rx, by = c("cod" = "cod"), multiple = "all") |>
  group_by(cod) |>
  mutate(deaths_adj = deaths * (1/coverage))

# Handling data to estimate birth underregistration

female_adj <- obitos_pop_female_adj |>
  group_by(cod) |>
  mutate(pop = if_else(age < 75,
                       sqrt((as.numeric(pop1)*lead(as.numeric(pop2), n = 2))),
                       sqrt((lag(as.numeric(pop1),n = 2)*as.numeric(pop2))))) |>
  select(UF = cod, GRUPO_ETARIO = age, sex = sex, deaths_adj, pop, rx)

rm(ddm_female, obitos_pop_female, obitos_pop_female_adj, obitos_pop_female_rx)

right_equation <- female_adj |>
  group_by(UF) |>
  mutate(deaths_cum = rev(cumsum(rev(deaths_adj))),
         pop_cum = rev(cumsum(rev(pop))),
         cdr_adj = deaths_cum/pop_cum,
         cdr_rx = cdr_adj + rx) |>
  filter(GRUPO_ETARIO == 0)

left_equation <- nascimentos |>
  arrange(UF) |>
  mutate(UF = factor(UF, levels = cods_uf$cod, labels = cods_uf$names)) |>
  filter(ANO == 2000) |>
  group_by(UF, SEXO) |>
  summarise(NASCIMENTOS = sum(NASCIMENTOS)) |>
  filter(SEXO == "Feminino") |>
  mutate(SEXO = "f") |>
  select(UF, sex = SEXO, NASCIMENTOS)

female_equation_00 <- right_equation |>
  select(UF, pop_cum, cdr_rx) |>
  bind_cols(left_equation) |>
  select(-c( UF...4, sex)) |>
  select(UF = UF...1, everything()) |>
  mutate(cbr = NASCIMENTOS/pop_cum,
         coverage_b = cdr_rx/cbr) |>
  select(UF, cbr, cdr_rx, coverage_b)

female_equation_00 |>
  ggplot() +
  aes(x = UF, y = coverage_b) +
  geom_hline(yintercept = 1,linetype = "dashed", color = "black", linewidth = 2) +
  geom_segment(aes(x=UF,
                   xend=UF,
                   y=min(coverage_b),
                   yend=max(coverage_b)),
               linetype="dashed",
               linewidth=0.1, color = "grey") +
  geom_point(size = 2) +
  coord_flip() +
  labs(
    title = "Grau de cobertura dos nascidos-vivos, por UF",
    subtitle = "Brasil (2000-2010), sexo feminino.",
    x = "UF",
    y = "Grau de cobertura dos nascimentos"
  ) +
  # scale_y_continuous(breaks = seq(.4,1.2,.1)) +
  theme_classic() +
  scale_color_manual(values = c("#35978f","#4d4d4d", "#993404", "#54278f", "#fdae61")) +
  guides(color = guide_legend(title = "Método"), shape = guide_legend(title = "Método")) +
  theme(
    plot.title = element_text(face = "bold", size = 10, hjust = 0),
    legend.title = element_text(face = "bold", size = 9, hjust = 0, vjust = .5),
    legend.text = element_text(size = 8, hjust = 0, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 9, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
    # panel.grid = element_line(color = "#f0f0f0",linewidth = .01,)
  )


####
# Female - 2010
####

# Handling data

obitos_pop_female <- obitos_pop |>
  filter(SEXO == "Feminino") |>
  filter(ANO == 2000) |>
  select(cod = UF, pop1 = POP, year1 = ANO, age = GRUPO_ETARIO, sex = SEXO) |>
  bind_cols(
    obitos_pop |>
      filter(SEXO == "Feminino") |>
      filter(ANO == 2010) |>
      select(pop2 = POP, year2 = ANO)
  ) |>
  bind_cols(
    obitos_mean |>
      filter(SEXO == "Feminino") |>
      select(deaths = OBITOS_mean)
  ) |>
  select(cod, pop1, pop2, deaths, year1, year2, age, sex) |>
  mutate(sex = "f",
         cod = factor(cod, levels = cods_uf$cod, labels = cods_uf$names))

# Calculating growth rate

obitos_pop_female_rx <- obitos_pop_female |>
  group_by(cod) |>
  mutate(cum_pop1 = rev(cumsum(rev(pop1))),
         cum_pop2 = rev(cumsum(rev(pop2))),
         rx = (1/(year2-year1)*log(cum_pop2/cum_pop1))) |>
  select(-c(cum_pop1, cum_pop2))

# Applicating DDM method

ddm_female <- DDM::ddm(obitos_pop_female)

ddm_female <- ddm_female |>
  group_by(cod) |>
  mutate(mean = (ggb+seg+ggbseg)/3,
         mean_2 = (ggb+ggbseg)/2)

ddm_female |>
  select(cod, ggb, seg, ggbseg, mean, mean_2) |>
  pivot_longer("ggb":"mean_2", names_to = "method", values_to = "coverage") |>
  ggplot() +
  aes(x = cod, y = coverage, color = method) +
  geom_hline(yintercept = 1,linetype = "dashed", color = "black", linewidth = 2) +
  geom_segment(aes(x=cod,
                   xend=cod,
                   y=min(coverage),
                   yend=max(coverage)),
               linetype="dashed",
               linewidth=0.1, color = "grey") +
  geom_point(size = 4) +
  coord_flip() +
  labs(
    title = "Grau de cobertura dos óbitos segundo diferentes métodos DDM, por UF",
    subtitle = "Brasil (2000-2010), sexo feminino.",
    x = "UF",
    y = "Grau de cobertura dos óbitos"
  ) +
  scale_y_continuous(breaks = seq(.4,1.6,.1)) +
  theme_classic() +
  scale_color_manual(values = c("#35978f","#4d4d4d", "#993404", "#54278f", "#fdae61")) +
  guides(color = guide_legend(title = "Método"), shape = guide_legend(title = "Método")) +
  theme(
    plot.title = element_text(face = "bold", size = 10, hjust = 0),
    legend.title = element_text(face = "bold", size = 9, hjust = 0, vjust = .5),
    legend.text = element_text(size = 8, hjust = 0, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 9, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
    # panel.grid = element_line(color = "#f0f0f0",linewidth = .01,)
  )

ddm_female <- ddm_female |>
  group_by(cod) |>
  mutate(
    coverage = case_when(
      mean_2 < 1 ~ mean_2,
      mean_2 > 1 & ggbseg < 1 ~ ggbseg,
      mean_2 > 1 & ggbseg > 1 & ggb < 1 ~ ggb,
      TRUE ~ seg))

# Correcting deaths registration by coverage factor

obitos_pop_female_adj <- ddm_female |>
  select(cod, coverage) |>
  right_join(obitos_pop_female_rx, by = c("cod" = "cod"), multiple = "all") |>
  group_by(cod) |>
  mutate(deaths_adj = deaths * (1/coverage))

# Handling data to estimate birth underregistration

female_adj <- obitos_pop_female_adj |>
  group_by(cod) |>
  mutate(pop = if_else(age < 75,
                       sqrt((as.numeric(pop1)*lead(as.numeric(pop2), n = 2))),
                       sqrt((lag(as.numeric(pop1),n = 2)*as.numeric(pop2))))) |>
  select(UF = cod, GRUPO_ETARIO = age, sex = sex, deaths_adj, pop, rx)

right_equation <- female_adj |>
  group_by(UF) |>
  mutate(deaths_cum = rev(cumsum(rev(deaths_adj))),
         pop_cum = rev(cumsum(rev(pop))),
         cdr_adj = deaths_cum/pop_cum,
         cdr_rx = cdr_adj + rx) |>
  filter(GRUPO_ETARIO == 0)

left_equation <- nascimentos |>
  arrange(UF) |>
  mutate(UF = factor(UF, levels = cods_uf$cod, labels = cods_uf$names)) |>
  filter(ANO == 2010) |>
  group_by(UF, SEXO) |>
  summarise(NASCIMENTOS = sum(NASCIMENTOS)) |>
  filter(SEXO == "Feminino") |>
  mutate(SEXO = "f") |>
  select(UF, sex = SEXO, NASCIMENTOS)

female_equation_10 <- right_equation |>
  select(UF, pop_cum, cdr_rx) |>
  bind_cols(left_equation) |>
  select(-c( UF...4, sex)) |>
  select(UF = UF...1, everything()) |>
  mutate(cbr = NASCIMENTOS/pop_cum,
         coverage_b = cdr_rx/cbr) |>
  select(UF, cbr, cdr_rx, coverage_b)

female_equation_10 |>
  ggplot() +
  aes(x = UF, y = coverage_b) +
  geom_hline(yintercept = 1,linetype = "dashed", color = "black", linewidth = 2) +
  geom_segment(aes(x=UF,
                   xend=UF,
                   y=min(coverage_b),
                   yend=max(coverage_b)),
               linetype="dashed",
               linewidth=0.1, color = "grey") +
  geom_point(size = 2) +
  coord_flip() +
  labs(
    title = "Grau de cobertura dos nascidos-vivos, por UF",
    subtitle = "Brasil (2000-2010), sexo feminino.",
    x = "UF",
    y = "Grau de cobertura dos nascimentos"
  ) +
  # scale_y_continuous(breaks = seq(.4,1.2,.1)) +
  theme_classic() +
  scale_color_manual(values = c("#35978f","#4d4d4d", "#993404", "#54278f", "#fdae61")) +
  guides(color = guide_legend(title = "Método"), shape = guide_legend(title = "Método")) +
  theme(
    plot.title = element_text(face = "bold", size = 10, hjust = 0),
    legend.title = element_text(face = "bold", size = 9, hjust = 0, vjust = .5),
    legend.text = element_text(size = 8, hjust = 0, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 9, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
    # panel.grid = element_line(color = "#f0f0f0",linewidth = .01,)
  )

# Comparing years

female_equation_00 |> mutate(ano = 2000) |>
  bind_rows(female_equation_10 |> mutate(ano = 2010)) |>
  ggplot() +
  aes(x = UF, y = coverage_b, color = ano) +
  geom_hline(yintercept = 1,linetype = "dashed", color = "black", linewidth = 2) +
  geom_segment(aes(x=UF,
                   xend=UF,
                   y=min(coverage_b),
                   yend=max(coverage_b)),
               linetype="dashed",
               linewidth=0.1, color = "grey") +
  geom_point(size = 2) +
  coord_flip() +
  labs(
    title = "Grau de cobertura dos nascidos-vivos, por UF e ano dos registros.",
    subtitle = "Brasil (2000-2010), sexo feminino.",
    x = "UF",
    y = "Grau de cobertura dos nascimentos"
  ) +
  # scale_y_continuous(breaks = seq(.4,1.2,.1)) +
  theme_classic() +
  # scale_color_manual(values = c("#35978f","#4d4d4d")) +
  # guides(color = guide_legend(title = "Ano"), shape = guide_legend(title = "Ano")) +
  theme(
    plot.title = element_text(face = "bold", size = 10, hjust = 0),
    legend.title = element_text(face = "bold", size = 9, hjust = 0, vjust = .5),
    legend.text = element_text(size = 8, hjust = 0, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 9, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
    # panel.grid = element_line(color = "#f0f0f0",linewidth = .01,)
  )

female_estimates <- female_equation_00 |>
  mutate(ano = 2000) |>
  bind_rows(female_equation_10 |>
              mutate(ano = 2010))

rm(ddm_female, obitos_pop_female, obitos_pop_female_adj, obitos_pop_female_rx)
rm(female_adj, female_equation_00, female_equation_10, left_equation, right_equation)


# Applying for mascs ------------------------------------------------------

####
# Male - 2000
####

# Handling data

obitos_pop_masc <- obitos_pop |>
  filter(SEXO == "Masculino") |>
  filter(ANO == 2000) |>
  select(cod = UF, pop1 = POP, year1 = ANO, age = GRUPO_ETARIO, sex = SEXO) |>
  bind_cols(
    obitos_pop |>
      filter(SEXO == "Masculino") |>
      filter(ANO == 2010) |>
      select(pop2 = POP, year2 = ANO)
  ) |>
  bind_cols(
    obitos_mean |>
      filter(SEXO == "Masculino") |>
      select(deaths = OBITOS_mean)
  ) |>
  select(cod, pop1, pop2, deaths, year1, year2, age, sex) |>
  mutate(sex = "m",
         cod = factor(cod, levels = cods_uf$cod, labels = cods_uf$names))

# Calculating growth rate

obitos_pop_masc_rx <- obitos_pop_masc |>
  group_by(cod) |>
  mutate(cum_pop1 = rev(cumsum(rev(pop1))),
         cum_pop2 = rev(cumsum(rev(pop2))),
         rx = (1/(year2-year1)*log(cum_pop2/cum_pop1))) |>
  select(-c(cum_pop1, cum_pop2))

# Applicating DDM method

ddm_masc <- DDM::ddm(obitos_pop_masc)

ddm_masc <- ddm_masc |>
  group_by(cod) |>
  mutate(mean = (ggb+seg+ggbseg)/3,
         mean_2 = (ggb+ggbseg)/2)

ddm_masc |>
  select(cod, ggb, seg, ggbseg, mean, mean_2) |>
  pivot_longer("ggb":"mean_2", names_to = "method", values_to = "coverage") |>
  ggplot() +
  aes(x = cod, y = coverage, color = method) +
  geom_hline(yintercept = 1,linetype = "dashed", color = "black", linewidth = 2) +
  geom_segment(aes(x=cod,
                   xend=cod,
                   y=min(coverage),
                   yend=max(coverage)),
               linetype="dashed",
               linewidth=0.1, color = "grey") +
  geom_point(size = 4) +
  coord_flip() +
  labs(
    title = "Grau de cobertura dos óbitos segundo diferentes métodos DDM, por UF",
    subtitle = "Brasil (2000-2010), sexo masculino.",
    x = "UF",
    y = "Grau de cobertura dos óbitos"
  ) +
  scale_y_continuous(breaks = seq(.4,1.6,.1)) +
  theme_classic() +
  scale_color_manual(values = c("#35978f","#4d4d4d", "#993404", "#54278f", "#fdae61")) +
  guides(color = guide_legend(title = "Método"), shape = guide_legend(title = "Método")) +
  theme(
    plot.title = element_text(face = "bold", size = 10, hjust = 0),
    legend.title = element_text(face = "bold", size = 9, hjust = 0, vjust = .5),
    legend.text = element_text(size = 8, hjust = 0, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 9, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
    # panel.grid = element_line(color = "#f0f0f0",linewidth = .01,)
  )

ddm_masc <- ddm_masc |>
  group_by(cod) |>
  mutate(
    coverage = case_when(
      mean_2 < 1 ~ mean_2,
      mean_2 > 1 & ggbseg < 1 ~ ggbseg,
      mean_2 > 1 & ggbseg > 1 & ggb < 1 ~ ggb,
      TRUE ~ seg))

# Correcting deaths registration by coverage factor

obitos_pop_masc_adj <- ddm_masc |>
  select(cod, coverage) |>
  right_join(obitos_pop_masc_rx, by = c("cod" = "cod"), multiple = "all") |>
  group_by(cod) |>
  mutate(deaths_adj = deaths * (1/coverage))

# Handling data to estimate birth underregistration

masc_adj <- obitos_pop_masc_adj |>
  group_by(cod) |>
  mutate(pop = if_else(age < 75,
                       sqrt((as.numeric(pop1)*lead(as.numeric(pop2), n = 2))),
                       sqrt((lag(as.numeric(pop1),n = 2)*as.numeric(pop2))))) |>
  select(UF = cod, GRUPO_ETARIO = age, sex = sex, deaths_adj, pop, rx)

right_equation <- masc_adj |>
  group_by(UF) |>
  mutate(deaths_cum = rev(cumsum(rev(deaths_adj))),
         pop_cum = rev(cumsum(rev(pop))),
         cdr_adj = deaths_cum/pop_cum,
         cdr_rx = cdr_adj + rx) |>
  filter(GRUPO_ETARIO == 0)

left_equation <- nascimentos |>
  arrange(UF) |>
  mutate(UF = factor(UF, levels = cods_uf$cod, labels = cods_uf$names)) |>
  filter(ANO == 2000) |>
  group_by(UF, SEXO) |>
  summarise(NASCIMENTOS = sum(NASCIMENTOS)) |>
  filter(SEXO == "Masculino") |>
  mutate(SEXO = "m") |>
  select(UF, sex = SEXO, NASCIMENTOS)

masc_equation_00 <- right_equation |>
  select(UF, pop_cum, cdr_rx) |>
  bind_cols(left_equation) |>
  select(-c( UF...4, sex)) |>
  select(UF = UF...1, everything()) |>
  mutate(cbr = NASCIMENTOS/pop_cum,
         coverage_b = cdr_rx/cbr) |>
  select(UF, cbr, cdr_rx, coverage_b)

masc_equation_00 |>
  ggplot() +
  aes(x = UF, y = coverage_b) +
  geom_hline(yintercept = 1,linetype = "dashed", color = "black", linewidth = 2) +
  geom_segment(aes(x=UF,
                   xend=UF,
                   y=min(coverage_b),
                   yend=max(coverage_b)),
               linetype="dashed",
               linewidth=0.1, color = "grey") +
  geom_point(size = 2) +
  coord_flip() +
  labs(
    title = "Grau de cobertura dos nascidos-vivos, por UF",
    subtitle = "Brasil (2000-2010), sexo masculino.",
    x = "UF",
    y = "Grau de cobertura dos nascimentos"
  ) +
  # scale_y_continuous(breaks = seq(.4,1.2,.1)) +
  theme_classic() +
  scale_color_manual(values = c("#35978f","#4d4d4d", "#993404", "#54278f", "#fdae61")) +
  guides(color = guide_legend(title = "Método"), shape = guide_legend(title = "Método")) +
  theme(
    plot.title = element_text(face = "bold", size = 10, hjust = 0),
    legend.title = element_text(face = "bold", size = 9, hjust = 0, vjust = .5),
    legend.text = element_text(size = 8, hjust = 0, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 9, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
    # panel.grid = element_line(color = "#f0f0f0",linewidth = .01,)
  )


####
# Male - 2010
####

# Handling data

obitos_pop_masc <- obitos_pop |>
  filter(SEXO == "Masculino") |>
  filter(ANO == 2000) |>
  select(cod = UF, pop1 = POP, year1 = ANO, age = GRUPO_ETARIO, sex = SEXO) |>
  bind_cols(
    obitos_pop |>
      filter(SEXO == "Masculino") |>
      filter(ANO == 2010) |>
      select(pop2 = POP, year2 = ANO)
  ) |>
  bind_cols(
    obitos_mean |>
      filter(SEXO == "Masculino") |>
      select(deaths = OBITOS_mean)
  ) |>
  select(cod, pop1, pop2, deaths, year1, year2, age, sex) |>
  mutate(sex = "m",
         cod = factor(cod, levels = cods_uf$cod, labels = cods_uf$names))

# Calculating growth rate

obitos_pop_masc_rx <- obitos_pop_masc |>
  group_by(cod) |>
  mutate(cum_pop1 = rev(cumsum(rev(pop1))),
         cum_pop2 = rev(cumsum(rev(pop2))),
         rx = (1/(year2-year1)*log(cum_pop2/cum_pop1))) |>
  select(-c(cum_pop1, cum_pop2))

# Applicating DDM method

ddm_masc <- DDM::ddm(obitos_pop_masc)

ddm_masc <- ddm_masc |>
  group_by(cod) |>
  mutate(mean = (ggb+seg+ggbseg)/3,
         mean_2 = (ggb+ggbseg)/2)

ddm_masc |>
  select(cod, ggb, ggbseg, mean = mean_2) |>
  pivot_longer("ggb":"mean", names_to = "method", values_to = "coverage") |>
  ggplot() +
  aes(x = cod, y = coverage, color = method) +
  geom_hline(yintercept = 1,linetype = "dashed", color = "black", linewidth = 1.5) +
  geom_segment(aes(x=cod,
                   xend=cod,
                   y=min(coverage),
                   yend=max(coverage)),
               linetype="dashed",
               linewidth=0.1, color = "grey") +
  geom_point(size = 5, shape = 20) +
  coord_flip() +
  labs(
    title = "Grau de cobertura dos óbitos masculinos segundo diferentes métodos DDM, por UF - 2000/2010. ",
    caption = "Fonte: MS/DATASUS, SIM. IBGE/SIDRA, Censo Demográfico 2000 e 2010.",
    x = "UF",
    y = "Grau de cobertura dos óbitos"
  ) +
  scale_y_continuous(breaks = seq(.4,1.6,.1)) +
  theme_classic() +
  scale_color_manual(values = c("#35978f","#4d4d4d", "#993404")) +
  guides(color = guide_legend(title = "Método"), shape = guide_legend(title = "Método")) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    plot.caption = element_text(size = 9),
    legend.title = element_text(face = "bold", size = 9, hjust = 0, vjust = .5),
    legend.text = element_text(size = 8, hjust = 0, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = 1, vjust = .5),
    axis.text = element_text(face = "bold", size = 8, color = "#636363", hjust = .5, vjust = .5),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 9, color = "#636363", hjust = .9, vjust = .5),
    panel.grid = element_line(color = "#f0f0f0",linewidth = .01)
  )

ddm_masc <- ddm_masc |>
  group_by(cod) |>
  mutate(
    coverage = case_when(
      mean_2 < 1 ~ mean_2,
      mean_2 > 1 & ggbseg < 1 ~ ggbseg,
      mean_2 > 1 & ggbseg > 1 & ggb < 1 ~ ggb,
      TRUE ~ seg))

# Correcting deaths registration by coverage factor

obitos_pop_masc_adj <- ddm_masc |>
  select(cod, coverage) |>
  right_join(obitos_pop_masc_rx, by = c("cod" = "cod"), multiple = "all") |>
  group_by(cod) |>
  mutate(deaths_adj = deaths * (1/coverage))

# Handling data to estimate birth underregistration

masc_adj <- obitos_pop_masc_adj |>
  group_by(cod) |>
  mutate(pop = if_else(age < 75,
                       sqrt((as.numeric(pop1)*lead(as.numeric(pop2), n = 2))),
                       sqrt((lag(as.numeric(pop1),n = 2)*as.numeric(pop2))))) |>
  select(UF = cod, GRUPO_ETARIO = age, sex = sex, deaths_adj, pop, rx)

rm(ddm_masc, obitos_pop_masc, obitos_pop_masc_adj, obitos_pop_masc_rx)

right_equation <- masc_adj |>
  group_by(UF) |>
  mutate(deaths_cum = rev(cumsum(rev(deaths_adj))),
         pop_cum = rev(cumsum(rev(pop))),
         cdr_adj = deaths_cum/pop_cum,
         cdr_rx = cdr_adj + rx) |>
  filter(GRUPO_ETARIO == 0)

left_equation <- nascimentos |>
  arrange(UF) |>
  mutate(UF = factor(UF, levels = cods_uf$cod, labels = cods_uf$names)) |>
  filter(ANO == 2010) |>
  group_by(UF, SEXO) |>
  summarise(NASCIMENTOS = sum(NASCIMENTOS)) |>
  filter(SEXO == "Masculino") |>
  mutate(SEXO = "m") |>
  select(UF, sex = SEXO, NASCIMENTOS)

masc_equation_10 <- right_equation |>
  select(UF, pop_cum, cdr_rx) |>
  bind_cols(left_equation) |>
  select(-c( UF...4, sex)) |>
  select(UF = UF...1, everything()) |>
  mutate(cbr = NASCIMENTOS/pop_cum,
         coverage_b = cdr_rx/cbr) |>
  select(UF, cbr, cdr_rx, coverage_b)

masc_equation_10 |>
  ggplot() +
  aes(x = UF, y = coverage_b) +
  geom_hline(yintercept = 1,linetype = "dashed", color = "black", linewidth = 2) +
  geom_segment(aes(x=UF,
                   xend=UF,
                   y=min(coverage_b),
                   yend=max(coverage_b)),
               linetype="dashed",
               linewidth=0.1, color = "grey") +
  geom_point(size = 2) +
  coord_flip() +
  labs(
    title = "Grau de cobertura dos nascidos-vivos, por UF",
    subtitle = "Brasil (2000-2010), sexo masculino.",
    x = "UF",
    y = "Grau de cobertura dos nascimentos"
  ) +
  # scale_y_continuous(breaks = seq(.4,1.2,.1)) +
  theme_classic() +
  scale_color_manual(values = c("#35978f","#4d4d4d", "#993404", "#54278f", "#fdae61")) +
  guides(color = guide_legend(title = "Método"), shape = guide_legend(title = "Método")) +
  theme(
    plot.title = element_text(face = "bold", size = 10, hjust = 0),
    legend.title = element_text(face = "bold", size = 9, hjust = 0, vjust = .5),
    legend.text = element_text(size = 8, hjust = 0, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 9, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
    # panel.grid = element_line(color = "#f0f0f0",linewidth = .01,)
  )

# Comparing years

masc_equation_00 |> mutate(ano = 2000) |>
  bind_rows(masc_equation_10 |> mutate(ano = 2010)) |>
  ggplot() +
  aes(x = UF, y = coverage_b, color = ano) +
  geom_hline(yintercept = 1,linetype = "dashed", color = "black", linewidth = 2) +
  geom_segment(aes(x=UF,
                   xend=UF,
                   y=min(coverage_b),
                   yend=max(coverage_b)),
               linetype="dashed",
               linewidth=0.1, color = "grey") +
  geom_point(size = 2) +
  coord_flip() +
  labs(
    title = "Grau de cobertura dos nascidos-vivos, por UF e ano dos registros.",
    subtitle = "Brasil (2000-2010), sexo masculino.",
    x = "UF",
    y = "Grau de cobertura dos nascimentos"
  ) +
  # scale_y_continuous(breaks = seq(.4,1.2,.1)) +
  theme_classic() +
  # scale_color_manual(values = c("#35978f","#4d4d4d")) +
  # guides(color = guide_legend(title = "Ano"), shape = guide_legend(title = "Ano")) +
  theme(
    plot.title = element_text(face = "bold", size = 10, hjust = 0),
    legend.title = element_text(face = "bold", size = 9, hjust = 0, vjust = .5),
    legend.text = element_text(size = 8, hjust = 0, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 9, hjust = .5, vjust = .5),
    axis.text = element_text(size = 8, hjust = .5, vjust = .5)
    # panel.grid = element_line(color = "#f0f0f0",linewidth = .01,)
  )

masc_estimates <- masc_equation_00 |>
  mutate(ano = 2000) |>
  bind_rows(masc_equation_10 |>
              mutate(ano = 2010))

rm(ddm_masc, obitos_pop_masc, obitos_pop_masc_adj, obitos_pop_masc_rx)
rm(masc_adj, masc_equation_00, masc_equation_10, left_equation, right_equation)

# Comparing estimates for both sexes

estimates_births <- female_estimates |>
  mutate(sex = "f") |>
  bind_rows(
    masc_estimates |>
      mutate(sex = "m")
  )


# Grafico de comparacao das estimativas

estimates_births |>
  filter(ano == 2000) |>
  select(everything(), coverage_2000 = coverage_b) |>
  bind_cols(
    estimates_births |>
      filter(ano == 2010) |>
      select(coverage_2010 = coverage_b)
  ) |>
  select(-ano) |>
  mutate(sex = case_when(sex == "f" ~ "Feminino", TRUE ~ "Masculino")) |>
  ggplot() +
  # geom_dumbbell(aes(x = coverage_2000, xend = coverage_2010),
  #               color ="#a6bddb",
  #               colour_xend ="#1c9099",
  #               size= 2,
  #
  #               dot_guide = FALSE,
  #               show.legend = TRUE) +
  geom_vline(xintercept = 1,linetype = "dashed", color = "black", linewidth = 1.4) +
  geom_segment(aes(x= coverage_2000, xend=coverage_2010,
                   y=UF, yend=UF),
               linetype="dashed",
               linewidth=0.1, color = "#d0d1e6") +
  geom_point(aes(x = coverage_2000, y = UF), size = 4, shape = 20, color = "#a6bddb") +
  geom_point(aes(x = coverage_2010, y = UF), size = 4, shape = 20, color = "#016c59") +
  labs(
    title = "Grau de cobertura dos nascidos-vivos, por UF e ano dos registros - Brasil (2000-2010), por sexo.",
    subtitle = "Cor clara = Nascimentos ocorridos no ano de 2000. \nCor forte = Nascimentos ocorridos no ano de 2010.",
    x = "Grau de cobertura dos nascidos vivos",
    y = "UF"
  ) +
  # scale_y_continuous(breaks = seq(.4,1.2,.1)) +
  theme_bw() +
  lemon::facet_rep_grid(sex ~ ., repeat.tick.labels = TRUE) +
  guides(color = guide_legend(title = "Ano"), shape = guide_legend(title = "Ano")) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    plot.subtitle = element_text(face = "italic", size = 9),
    legend.title = element_text(face = "bold", size = 9, hjust = 0, vjust = .5),
    legend.text = element_text(size = 8, hjust = 0, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(face = "bold", size = 8, color = "#636363", hjust = .5, vjust = .5),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 9, color = "#636363", hjust = .9, vjust = .5),
    panel.grid = element_line(color = "#f0f0f0",linewidth = .01)
  )


# Spatial representation --------------------------------------------------

cod <- cods_uf |> select(cod) |> as.vector()
cod <- rep(cod$cod, 2)

estimates_spatial <- estimates_births |>
  filter(ano == 2000) |>
  select(everything(), coverage_2000 = coverage_b) |>
  bind_cols(
    estimates_births |>
      filter(ano == 2010) |>
      select(coverage_2010 = coverage_b)
  ) |>
  select(-ano) |>
  mutate(sex = case_when(sex == "f" ~ "Feminino", TRUE ~ "Masculino"),
         razao_10_00 = coverage_2010/coverage_2000,
         cod_uf = cod) |>
  select(cod_uf, UF, sex, coverage_2000, coverage_2010, razao_10_00) |>
  mutate(fator = case_when(razao_10_00 < 1 ~ "2010 < 2000",
                           razao_10_00 > 1 ~ "2010 > 2000",
                           TRUE ~ "2010 = 2000"))

# Import spatial data

uf_map <- get_brmap(geo = "State",
                     class = "sf")

# Joining data

estimates_spatial <- uf_map |>
  bind_rows(uf_map) |>
  arrange(State) |>
  bind_cols(
    estimates_spatial |>
      filter(cod_uf != "BR") |>
      mutate(cod_UF = as.numeric(cod_uf)) |>
      arrange(cod_uf)
  ) |>
  select(cod_uf, UF, REGIAO = Region, sexo = sex, fator, coverage_2000, coverage_2010, razao_10_00, geometry)

estimates_spatial |>
  ggplot() +
  geom_sf(aes(fill = razao_10_00),
          colour = "black", size = 0.1) +
  geom_sf(data = get_brmap("State"),
          fill = "transparent",
          colour = "black", size = 0.6) +
  scale_fill_viridis_c(option = 2,direction = -1) +
  lemon::facet_rep_grid(.~sexo, repeat.tick.labels = TRUE) +
  guides(fill = guide_colourbar(title = "Razão Cobertura (2010)/Cobertura (2000)")) +
  labs(
    title = "Razão entre o grau de cobertura dos nascidos-vivos 2010/2000, por UF e sexo.",
    caption = "Fonte: MS/DATASUS, SINASC e SIM. IBGE/SIDRA, Censo Demográfico 2000 e 2010."
  ) +
  # tira sistema cartesiano
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    plot.caption = element_text(size = 8),
    legend.title = element_text(face = "bold", size = 9, hjust = 0, vjust = .5),
    legend.text = element_text(size = 8, hjust = 0, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(face = "bold", size = 8, color = "#636363", hjust = .5, vjust = .5),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 9, color = "#636363", hjust = .9, vjust = .5),
    panel.grid = element_line(color = "#f0f0f0",linewidth = .01),
    # panel.grid = element_line(colour = "grey"),
    panel.background = element_blank())
