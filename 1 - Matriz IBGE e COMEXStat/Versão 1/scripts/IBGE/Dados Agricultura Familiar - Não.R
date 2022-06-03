
#########################################
#  Dados trabalhados neste script:      #
#  - 3. Dados Agricultura Familiar Não  #
#########################################


# ======================================================================================================================
# Pacotes utililizados neste Script
# ======================================================================================================================
library(tidyr)
library(magrittr)
library(purrr)
library(readxl)
library(openxlsx)
library(stringr)
library(qdapRegex)
library(naniar)
library(dplyr)

# Clear data
# ----------------------------------------------------------------------------------------------------------------------
rm(list = ls())
options(scipen=999)
rm(list=ls())

# ======================================================================================================================
# PARTE 1 - IMPORTANDO OS DADOS
# ======================================================================================================================

# Passo 1.1: Caminho dos arquivos para leitura
# ----------------------------------------------------------------------------------------------------------------------
path <- list.files("data/IBGE/3. Dados Agricultura Familiar Não/", full.names = T)

# Passo 1.2: Conferir nome das Sheets
# ----------------------------------------------------------------------------------------------------------------------
map(path, ~ .x %>% excel_sheets()) # Os 7 arquivos possuem a mesma ordem: 'Quantidade' sempre em primeiro

# Passo 1.3: Leitura das bases de dados
# ----------------------------------------------------------------------------------------------------------------------
lista_quantidade_produzida <- map(path, ~ .x %>% read_excel(sheet = 1))
lista_valor_da_producao <- map(path, ~ .x %>% read_excel(sheet = 2))

# Passo 1.4: Conferindo se todas as bases possuem assinatura do IBGE ao final
# ----------------------------------------------------------------------------------------------------------------------
lista_quantidade_produzida %>% map(~ .x %>% tail(1))
lista_valor_da_producao %>% map(~ .x %>% tail(1))

# ======================================================================================================================
# PARTE 2 - MANIPULAÇÃO DOS DADOS - QUANTIDADE PRODUZIDA
# ======================================================================================================================

# Passo 2.1: Padronizando para o loop
# ----------------------------------------------------------------------------------------------------------------------
lista_quantidade_produzida_4 <- lista_quantidade_produzida[[4]]
lista_quantidade_produzida[[4]] <- NULL
lista_quantidade_produzida[[5]] %<>% .[-6,]

# Passo 2.2: Loops
# ----------------------------------------------------------------------------------------------------------------------
lista_quantidade_produzida_mod <- 
  lista_quantidade_produzida %>%
  map(
    ~ .x %>% 
      select(-c(2:4)) %>% 
      `colnames<-`(., c("Municipio", .[5,][-1])) %>% 
      .[-c(1:5),] %>%  # Removendo linhas iniciais que nao serao utilizadas
      .[-nrow(.),] %>% # Removendo a assinatura do IBGE na ultima linha
      mutate(Grupo = .x[[2,4]]) %>% 
      pivot_longer(!c("Municipio", "Grupo"), names_to = "Produto", values_to = "Quantidade")
  )

# Passo 2.3: Manipulacao da lista 4: destoa das demais
# ----------------------------------------------------------------------------------------------------------------------
lista_quantidade_produzida_mod[[ {length(lista_quantidade_produzida_mod)+1} ]] <- 
  lista_quantidade_produzida_4 %>% 
  select(-2) %>% 
  `colnames<-`(c("Municipio", str_remove(lista_quantidade_produzida_4[1,1], "Variável - "))) %>% 
  mutate(Grupo = .[[2,2]]) %>% 
  .[-c(1:5),] %>%  # Removendo linhas iniciais que nao serao utilizadas
  .[-nrow(.),] %>% # Removendo a assinatura do IBGE na ultima linha
  pivot_longer(!c("Municipio", "Grupo"), names_to = "Produto", values_to = "Quantidade")

# Passo 2.4: Separando Município de Estado, limpeza na coluna Grupo e separando Produto de Unidade de Medida
# ----------------------------------------------------------------------------------------------------------------------
quantidade_produzida <- 
  lista_quantidade_produzida_mod %>% 
  bind_rows() %>% 
  separate(Municipio, c("Municipio", "Estado"), sep = "[[:space:]][[:punct:]]") %>% 
  mutate(
    Estado = Estado %>% str_remove_all("[[:punct:]]"),
    Grupo = Grupo %>% str_remove_all("Ano x Tipologia x ")
  ) %>% 
  mutate(
    Produto = Produto %>% str_replace_all("[[:punct:]]Tonelada", "__Tonelada"),
    Produto = Produto %>% str_replace_all("[[:punct:]]Mil", "__Mil"),
    Produto = Produto %>% str_replace_all("[[:punct:]]não", "__não")
  ) %>% 
  separate(Produto, c("Produto", "UMedida"), sep = "__") %>% 
  mutate(
    CodigoIBGE = NA,
    Tipologia = NA,
    Produto = Produto %>% str_trim(),
    UMedida = UMedida %>% str_remove_all("[[:punct:]]")
  ) %>% 
  select(CodigoIBGE, Municipio, Estado, Tipologia, Grupo, Produto, UMedida, Quantidade) %>% 
  `colnames<-`(c("Código IBGE", "Nome do Município", "Estado (UF)", "Tipologia", "Grupo", "Produto", "Unidade de Medida", "Quantidade"))


# ======================================================================================================================
# PARTE 3 - MANIPULAÇÃO DOS DADOS - VALOR DA PRODUÇÃO
# ======================================================================================================================

# Passo 3.1: Padronizando para o loop
# ----------------------------------------------------------------------------------------------------------------------
lista_valor_da_producao_4 <- lista_valor_da_producao[[4]]
lista_valor_da_producao[[4]] <- NULL
lista_valor_da_producao[[5]] %<>% .[-6,]

# Passo 3.2: Loops
# ----------------------------------------------------------------------------------------------------------------------
lista_valor_da_producao_mod <- 
  lista_valor_da_producao %>%
  map(
    ~ .x %>% 
      select(-c(2:4)) %>% 
      `colnames<-`(., c("Municipio", .[5,][-1])) %>% 
      .[-c(1:5),] %>%  # Removendo linhas iniciais que nao serao utilizadas
      .[-nrow(.),] %>% # Removendo a assinatura do IBGE na ultima linha
      mutate(
        Grupo = .x[[2,4]],
        UMedida = .x[[1,1]]
      ) %>% 
      pivot_longer(!c("Municipio", "Grupo", "UMedida"), names_to = "Produto", values_to = "Quantidade")
  )

# Passo 3.3: Manipulacao da lista 4: destoa das demais
# ----------------------------------------------------------------------------------------------------------------------
lista_valor_da_producao_mod[[ {length(lista_valor_da_producao_mod)+1} ]] <- 
  lista_valor_da_producao_4 %>% 
  select(-2) %>% 
  `colnames<-`(c("Municipio", ex_between(lista_valor_da_producao_4[1,1], "-", "("))) %>% 
  mutate(
    Grupo = .[[2,2]],
    UMedida = .[[1,1]]
  ) %>% 
  .[-c(1:5),] %>%  # Removendo linhas iniciais que nao serao utilizadas
  .[-nrow(.),] %>% # Removendo a assinatura do IBGE na ultima linha
  pivot_longer(!c("Municipio", "Grupo", "UMedida"), names_to = "Produto", values_to = "Quantidade")

# Passo 3.4: Separando Município de Estado, limpeza na coluna Grupo e separando Produto de Unidade de Medida
# ----------------------------------------------------------------------------------------------------------------------
valor_da_producao <- 
  lista_valor_da_producao_mod %>% 
  bind_rows() %>% 
  separate(Municipio, c("Municipio", "Estado"), sep = "[[:space:]][[:punct:]]") %>% 
  mutate(
    Estado = Estado %>% str_remove_all("[[:punct:]]"),
    Grupo = Grupo %>% str_remove_all("Ano x Tipologia x "),
    UMedida = UMedida %>% sub(".*\\(", "", .) %>% str_remove_all("[[:punct:]]")
  ) %>% 
  mutate(
    CodigoIBGE = NA,
    Tipologia = NA
  ) %>% 
  select(CodigoIBGE, Municipio, Estado, Tipologia, Grupo, Produto, UMedida, Quantidade) %>% 
  `colnames<-`(c("Código IBGE", "Nome do Município", "Estado (UF)", "Tipologia", "Grupo", "Produto", "Unidade de Medida", "Valor"))

# ======================================================================================================================
# PARTE 4 - RETIRANDO TODAS AS BASES AUXILIARES. DEIXANDO SOMENTE O VALOR DA PRODUÇÃO E DA QUANTIDADE PRODUZIDA
# ======================================================================================================================

rm(
  path, lista_quantidade_produzida, lista_quantidade_produzida_4, lista_quantidade_produzida_mod,
  lista_valor_da_producao, lista_valor_da_producao_4, lista_valor_da_producao_mod
)
gc()
