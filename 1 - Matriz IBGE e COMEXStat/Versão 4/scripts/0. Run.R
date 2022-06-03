
# ======================================================================================================================
# Pacotes utililizados neste Script
# ======================================================================================================================
library(tictoc)
library(dplyr)
library(writexl)

# Clear data
# ----------------------------------------------------------------------------------------------------------------------
rm(list = ls())
options(scipen=999)
rm(list=ls())

# ======================================================================================================================
# PARTE 1 - DADOS - FAMILIAR SIM E NÃO
# ======================================================================================================================

# Passo 1.1: Leitura dos Scripts
# ----------------------------------------------------------------------------------------------------------------------
tic(); source("scripts/1. Dados Familiar - Agricultura e Pecuaria - Não.R", encoding = "UTF-8"); toc() # Familiar Não
tic(); source("scripts/1. Dados Familiar - Agricultura e Pecuaria - Sim.R", encoding = "UTF-8"); toc() # Familiar Sim

# Passo 1.2: Dados - Quantidade e Valor: Familiar Não e Sim
# ----------------------------------------------------------------------------------------------------------------------
quantidade_produzida <- quantidade_produzida_fam_nao %>% bind_rows(quantidade_produzida_fam_sim)
valor_da_producao <- valor_da_producao_fam_nao %>% bind_rows(valor_da_producao_fam_sim)

# -------------------------------------------------------------------------

rm(
  quantidade_produzida_fam_nao, quantidade_produzida_fam_sim,
  valor_da_producao_fam_nao, valor_da_producao_fam_sim
)

# ======================================================================================================================
# PARTE 2 - DADOS - MANIPULAÇÃO DOS PRODUTOS 
# ======================================================================================================================

# Passo 2.1: Manipulando os produtos
# ----------------------------------------------------------------------------------------------------------------------
tic(); source("scripts/2. Manipulação Produtos.R", encoding = "UTF-8"); toc() # Manipulação da coluna Produtos - Juntando os menos exportados


# ======================================================================================================================
# PARTE 3 - DADOS - ELABORANDO O JOIN
# ======================================================================================================================

# Passo 3.1: Merge ComexStat
# ----------------------------------------------------------------------------------------------------------------------
tic(); source("scripts/3. Merge ComexStat.R", encoding = "UTF-8"); toc() # Adicionando as colunas Quantidade (Toneladas) e Valor FOB (US$)


# ======================================================================================================================
# PARTE 4 - OUTPUT - SALVANDO AS BASES DE DADOS 
# ======================================================================================================================
quantidade_produzida %>% write_xlsx("output/quantidade_produzida.xlsx")
valor_da_producao %>% write_xlsx("output/valor_da_producao.xlsx")

# -------------------------------------------------------------------------

gc()