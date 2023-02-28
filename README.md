
# coverage_birth (repo in portuguese)

<!-- badges: start -->
<!-- badges: end -->

## Contextualização

Uma análise do nível da fecundidade para determinada população depende da disponibilidade de estatísticas vitais sobre o registro dos nascidos vivos de uma determinada população em um determinado período, além da população exposta ao risco de ter um filho no mesmo período analisado. O uso de informações censitárias, embora seja o recurso, em geral, mais utilizado, gera imprecisões das estimativas no que diz respeito ao seu nível, uma vez que há o erro de período de referência que afeta o numerador (como discutido por Brass e outros).

Abordagens alternativas foram desenvolvidas com o intuito de ajuste do nível das estimativas de modo indireto. Algumas delas são: P/F de Brass clássica e ajustada, Filhos Próprios, Razão Crianças/Mulheres, dentre outras.

Outra alternativa menos comum na literatura seria através do uso dos dados de estatísticas vitais de nascidos vivos. Se por um lado, informações censitárias têm a motivação de se ter uma mesma fonte de dados associada ao numerador e denominador, ela se torna limitada, em alguma medida, aos pressupostos associados aos métodos indiretos.

No caso do uso das estatísticas vitais dos nascidos vivos, por outro lado, há um problema a ser enfrentado que diz respeito à completude das informações. Sabe-se que não há uma boa completude das informações de estatísticas vitais em muitos países. Todavia, há uma série de métodos que vêm sendo desenvolvidos no âmbito dos registros de óbito que podem ser utilizados como referências para se pensar modos de se avaliar e corrigir possível subrregistro que afete estas estimativas.

---

## Objetivo

Avaliar em que medida houve um subrregistro dos nascidos-vivos ocorridos no Brasil e UF entre 2000 e 2010.

---

## Dados

- **Óbitos por idade e sexo:** Sistema de Informações de Mortalidade (SIM) entre 2000 e 2010.

- **Nascidos vivos por sexo:** Sistema Nacional de Nascidos-Vivos (SINASC) entre 2000 e 2010.

- **População residente por idade e sexo:** Sistema de Informações e Recuperação (SIDRA) 2000 e 2010.

---

## Estratégia de tratamento dos dados (etapas seguintes)

- Redistribuição *pro rata* dos registros de óbitos e nascidos vivos sem declaração de idade e/ou sexo (feito)

- Suavização dos registros de óbito por ano utilizando **média móvel trianual** (etapa seguinte)

- Suavização dos registros de nascidos vivos por ano utilizando **média móvel trianual** (etapa seguinte).

---

## Resultados preliminares

- **Grau de cobertura dos óbitos masculinos (como exemplo) por UF e segundo diferentes métodos de distribuição dos óbitos** (*DDM*):

<p align="center" width="80%">
    <img width="80%" src="https://github.com/thiagocalm/coverage_birth/blob/master/output/coverage_deaths_male.jpeg">
</p>

- **Grau de cobertura dos nascidos vivos para Brasil e UFs, por sexo entre 2000 e 2010**:

<p align="center" width="80%">
    <img width="80%" src="https://github.com/thiagocalm/coverage_birth/blob/master/output/coverage_birth.jpeg">
</p>

- **Razão entre grau de cobertura dos nascidos vivos (2010/2000) para UFs, por sexo**:

<p align="center" width="80%">
    <img width="80%" src="https://github.com/thiagocalm/coverage_birth/blob/master/output/coverage_ratio_2010_2000.jpeg">
</p>

## Algumas referências de pacotes utilizados

- [`tidyverse` para tratamento dos dados](https://www.tidyverse.org/)

- [`DDM package` para estimação do grau de cobertura dos óbitos](https://github.com/timriffe/AdultCoverage)

- [`brpop` para importação dos dados de população por idade e sexo](https://rfsaldanha.github.io/brpop/)

- [`microdatasus` para importação e processamento dos dados de óbitos e nascidos-vivos por idade e sexo](https://github.com/rfsaldanha/microdatasus)

- [`brazilmaps` para importação dos shapefiles para espacialização dos dados](https://github.com/rpradosiqueira/brazilmaps)
