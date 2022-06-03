# ======================================================================================================================
# Este Script vai realizar a união das bases de dados da Agricultura + Pecuária (sim e não)
# ======================================================================================================================

# ======================================================================================================================
# Pacotes utililizados neste Script
# ======================================================================================================================
library(dplyr)
library(tictoc)

# ======================================================================================================================
# PARTE 1 - IMPORTANDO OS DADOS
# ======================================================================================================================

# Passo 1.1: Leitura dos Scripts
# ----------------------------------------------------------------------------------------------------------------------
tic(); source("scripts/IBGE/Dados Familiar - Agricultura e Pecuaria - Não.R", encoding = "UTF-8"); toc() # Familiar Não
tic(); source("scripts/IBGE/Dados Familiar - Agricultura e Pecuaria - Sim.R", encoding = "UTF-8"); toc() # Familiar Sim

# Passo 1.2: Data - Quantidade e Valor: Familiar Não e Sim
# ----------------------------------------------------------------------------------------------------------------------
quantidade_produzida <- quantidade_produzida_fam_nao %>% bind_rows(quantidade_produzida_fam_sim)
valor_da_producao <- valor_da_producao_fam_nao %>% bind_rows(valor_da_producao_fam_sim)

# ======================================================================================================================
# PARTE 2 - CONSOLIDAÇÃO E UNIÃO DOS DADOS
# ======================================================================================================================
rm(
  quantidade_produzida_fam_nao, quantidade_produzida_fam_sim,
  valor_da_producao_fam_nao, valor_da_producao_fam_sim
)
