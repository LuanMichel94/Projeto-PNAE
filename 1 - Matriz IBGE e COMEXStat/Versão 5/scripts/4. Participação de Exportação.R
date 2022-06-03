
################################################################################
# Manipulação dos dados para Construção da participação da exportação, segundo #
# o arquivo "Sugestões para construção da participação de exportação.docx"     #
################################################################################

# ======================================================================================================================
# Pacotes utililizados neste Script
# ======================================================================================================================
library(magrittr)
library(scales)
library(tidyr)
library(dplyr)

# ======================================================================================================================
# PARTE 1 - IMPORTANDO OS DADOS  
# ======================================================================================================================

# Passo 1.1: Codigo IBGE
# ----------------------------------------------------------------------------------------------------------------------
cod_ibge <- read_excel("data/IBGE/Geon_Cod.xlsx") %>% 
  select(uf, regiao) %>% 
  `colnames<-`(c("UF", "Região")) %>% 
  distinct_all()

# ======================================================================================================================
# PARTE 2 - MANIPULANDO OS DADOS DA QUANTIDADE PRODUZIDA
# ======================================================================================================================

# Passo 2.1: Soma dos Produtos por Estado
# ----------------------------------------------------------------------------------------------------------------------
quantidade_produzida_est <- quantidade_produzida %>% 
  group_by(`Estado (UF)`, Tipologia, Produto) %>% 
  summarise(
    Quantidade = sum(Quantidade, na.rm = TRUE),
    `Quantidade (Toneladas)` = sum(`Quantidade (Toneladas)`, na.rm = TRUE)
  ) %>% 
  ungroup()

# Passo 2.2: Soma dos Produtos Brasil
# ----------------------------------------------------------------------------------------------------------------------
quantidade_produzida_br <- quantidade_produzida %>% 
  group_by(Tipologia, Produto) %>% 
  summarise(
    Quantidade = sum(Quantidade, na.rm = TRUE),
    `Quantidade (Toneladas)` = sum(`Quantidade (Toneladas)`, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate("Estado (UF)" = "Brasil") %>% 
  select(`Estado (UF)`, everything())

# Passo 2.3: Soma dos PrOdutos - Juntando
# ----------------------------------------------------------------------------------------------------------------------
quantidade_produzida_jun <- quantidade_produzida_est %>% bind_rows(quantidade_produzida_br)

# Passo 2.4: Calculando o percentual entre Familiar Sim e Não
# ----------------------------------------------------------------------------------------------------------------------
quantidade_produzida_perc <- quantidade_produzida_jun %>% 
  group_by(`Estado (UF)`, Produto) %>% 
  summarise(perc = paste(Tipologia, "_", Quantidade, "__", Quantidade/sum(Quantidade))) %>% 
  ungroup() %>% 
  separate(perc, c("Tipologia", "col_aux"), sep = " _ ") %>% 
  separate(col_aux, c("Quantidade", "perc_quantidade"), sep = " __ ") %>% 
  mutate(perc_quantidade = as.numeric(perc_quantidade))

# Passo 2.5: Produtos a serem removidos, pois tem quantidade 0 em ambos (Fam Sim e Não)
# ----------------------------------------------------------------------------------------------------------------------
quantidade_produzida_perc %>% filter(is.nan(perc_quantidade))

# Passo 2.6: Removendo
# ----------------------------------------------------------------------------------------------------------------------
quantidade_produzida_perc %<>% filter(!is.nan(perc_quantidade))

# Passo 2.7: Separação em Agricultura e Pecuária Familiar Sim e Não
# ----------------------------------------------------------------------------------------------------------------------
quantidade_produzida_perc_agr_sim <- quantidade_produzida_perc %>% filter(Tipologia=="Agricultura Familiar Sim")
quantidade_produzida_perc_agr_nao <- quantidade_produzida_perc %>% filter(Tipologia=="Agricultura Familiar Não")

quantidade_produzida_perc_pec_sim <- quantidade_produzida_perc %>% filter(Tipologia=="Pecuária Familiar Sim")
quantidade_produzida_perc_pec_nao <- quantidade_produzida_perc %>% filter(Tipologia=="Pecuária Familiar Não")

# ======================================================================================================================
# PARTE 3 - CONSTRUINDO A MATRIZ
# ======================================================================================================================
quantidade_produzida_agr_sim_matriz <- quantidade_produzida_perc_agr_sim %>% 
  select(`Estado (UF)`, Produto, perc_quantidade) %>% 
  `colnames<-`(c("UF", "Produto", "perc_quantidade")) %>% 
  mutate(
    perc_quantidade = perc_quantidade %>% percent(accuracy = 0.001, big.mark = ".", decimal.mark = ",")
  ) %>% 
  pivot_wider(names_from = Produto, values_from = perc_quantidade) %>% 
  mutate_all(~ ifelse(is.na(.x), "-", .x)) %>% 
  left_join(cod_ibge, by = c("UF")) %>% 
  select(`Região`, UF, everything()) %>% 
  mutate(`Região` = ifelse(UF=="Brasil", "Brasil", `Região`)) %>% 
  arrange(`Região`)

quantidade_produzida_pec_sim_matriz <- quantidade_produzida_perc_pec_sim %>% 
  select(`Estado (UF)`, Produto, perc_quantidade) %>% 
  `colnames<-`(c("UF", "Produto", "perc_quantidade")) %>% 
  mutate(
    perc_quantidade = perc_quantidade %>% percent(accuracy = 0.001, big.mark = ".", decimal.mark = ",")
  ) %>% 
  pivot_wider(names_from = Produto, values_from = perc_quantidade) %>% 
  mutate_all(~ ifelse(is.na(.x), "-", .x)) %>% 
  left_join(cod_ibge, by = c("UF")) %>% 
  select(`Região`, UF, everything()) %>% 
  mutate(`Região` = ifelse(UF=="Brasil", "Brasil", `Região`)) %>% 
  arrange(`Região`)

# ======================================================================================================================
# PARTE 4 - BASES FINAIS
# ======================================================================================================================

rm(
  quantidade_produzida_est, quantidade_produzida_br,
  quantidade_produzida_jun, quantidade_produzida_perc, cod_ibge,
  quantidade_produzida_perc_agr_sim, quantidade_produzida_perc_agr_nao,
  quantidade_produzida_perc_pec_sim, quantidade_produzida_perc_pec_nao
)

# -------------------------------------------------------------------------
