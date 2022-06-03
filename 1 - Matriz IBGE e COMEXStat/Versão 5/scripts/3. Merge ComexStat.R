
#################################################################
#  Merge das bases de dados utilizando o arquivo ComexStat.xlsx #
#################################################################

# ======================================================================================================================
# Pacotes utililizados neste Script
# ======================================================================================================================
library(readxl)
library(janitor)
library(tidyr)
library(dplyr)

# ======================================================================================================================
# PARTE 1 - IMPORTANDO OS DADOS DA COMEXSTAT 
# ======================================================================================================================

# Passo 1.1: Agricultura
# ----------------------------------------------------------------------------------------------------------------------
comexstat_agr <- read_excel("data/ComexStat/ComexStat.xlsx", sheet = "Importar_Agricultura") %>% 
  mutate(
    Municipio = case_when(
      Municipio=="Mogi-Mirim - SP"                ~ "Mogi Mirim - SP",
      Municipio=="Belém de São Francisco - PE"    ~ "Belém do São Francisco - PE",
      Municipio=="Santana do Livramento - RS"     ~ "Sant'Ana do Livramento - RS",
      Municipio=="Embu - SP"                      ~ "Embu-Guaçu - SP",
      Municipio=="Amambaí - MS"                   ~ "Amambai - MS",
      Municipio=="Lagoa do Itaenga - PE"          ~ "Lagoa de Itaenga - PE",
      Municipio=="São Valério da Natividade - TO" ~ "São Valério - TO",
      Municipio=="Barueri - SP"                   ~ "Bariri - SP",
      TRUE ~ Municipio
    )
  ) %>% 
  mutate(join = paste(Municipio, "_", Produto)) %>% 
  clean_names() %>% 
  group_by(join) %>% 
  summarise(
    quantidade_toneladas = sum(quantidade_toneladas),
    valor_fob_us = sum(valor_fob_us)
  ) %>% 
  `colnames<-`(c("join", "Quantidade (Toneladas)", "Valor FOB (US$)"))

# Passo 1.2: Pecuária
# ----------------------------------------------------------------------------------------------------------------------
comexstat_pec <- read_excel("data/ComexStat/ComexStat.xlsx", sheet = "Importar_Pecuaria") %>% 
  mutate(
    Municipio = case_when(
      Municipio=="Grão Pará - SC"      ~ "Grão-Pará - SC",
      Municipio=="Lauro Muller - SC"   ~ "Lauro Müller - SC",
      TRUE ~ Municipio
    )
  ) %>% 
  mutate(join = paste(Municipio, "_", Produto)) %>% 
  clean_names() %>% 
  group_by(join) %>% 
  summarise(
    quantidade_toneladas = sum(quantidade_toneladas),
    valor_fob_us = sum(valor_fob_us)
  ) %>% 
  `colnames<-`(c("join", "Quantidade (Toneladas)", "Valor FOB (US$)"))

# ======================================================================================================================
# PARTE 2 - MANIPULANDO OS DADOS - COMEXSTAT
# ======================================================================================================================

# Passo 2.1: Agricultura
# ----------------------------------------------------------------------------------------------------------------------
quantidade_produzida_agr <- quantidade_produzida_agr %>% 
  mutate(join = paste(`Nome do Município`, "-", `Estado (UF)`, "_", Produto)) %>% 
  left_join(comexstat_agr, by = c("join")) %>% 
  select(-join, -`Valor FOB (US$)`)

valor_da_producao_agr <- valor_da_producao_agr %>% 
  mutate(join = paste(`Nome do Município`, "-", `Estado (UF)`, "_", Produto)) %>% 
  left_join(comexstat_agr, by = c("join")) %>% 
  select(-join, -`Quantidade (Toneladas)`)

# Passo 2.2: Pecuária
# ----------------------------------------------------------------------------------------------------------------------
quantidade_produzida_pec <- quantidade_produzida_pec %>% 
  mutate(join = paste(`Nome do Município`, "-", `Estado (UF)`, "_", Produto)) %>% 
  left_join(comexstat_pec, by = c("join")) %>% 
  select(-join, -`Valor FOB (US$)`)

valor_da_producao_pec <- valor_da_producao_pec %>% 
  mutate(join = paste(`Nome do Município`, "-", `Estado (UF)`, "_", Produto)) %>% 
  left_join(comexstat_pec, by = c("join")) %>% 
  select(-join, -`Quantidade (Toneladas)`)

# ======================================================================================================================
# PARTE 3 - CONSOLIDANDO OS DADOS DO COMEXSTAT - AGRICULTURA E PECUÁRIA
# ======================================================================================================================
rm(comexstat_agr, comexstat_pec)

gc()

# -------------------------------------------------------------------------