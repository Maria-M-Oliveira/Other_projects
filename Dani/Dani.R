# LIBRARIES
library(tidyverse)
library(ggplot2)
library(readxl)
library(openxlsx)
library(janitor)
# Files
BD <- read_xlsx(".\\Dani\\UEvora_Sines_2021.xlsx")
survey <- read.csv("Inqueritos_limpar.csv") %>% 
  clean_names()

# Cleaning data
survey <- survey %>%
  mutate(
    Question_1 = case_when(
      !is.na(x1_local)      ~ "local",
      !is.na(x1_costeira)   ~ "costeira",
      !is.na(x1_largo)      ~ "largo",
      !is.na(x1_ns_nr)      ~ "NS/NR",
      TRUE                  ~ NA_character_
    ),
    Question6_2 = case_when(
      !is.na(x6_2_sim)      ~ "sim",
      !is.na(x6_2_nao)      ~ "nao",
      !is.na(x6_2_ns_nr)    ~ "NS/NR",
      TRUE                  ~ NA_character_
    ),
    Question6_2_1 = case_when(
      !is.na(x6_2_1_fontes_oficiais_governamentais)   ~ "oficiais",
      !is.na(x6_2_1_noticias_na_comunicacao_social)   ~ "noticias",
      !is.na(x6_2_1_redes_sociais)                   ~ "redes sociais",
      !is.na(x6_2_1_pessoas_conhecidas)              ~ "pessoas",
      !is.na(x6_2_1_outra_fonte_qual)                ~ "outra",
      TRUE                                          ~ NA_character_
    ),
    Question6_3 = case_when(
      !is.na(x6_3_sim)      ~ "sim",
      !is.na(x6_3_nao)      ~ "não",
      !is.na(x6_3_ns_nr)    ~ "NS/NR",
      TRUE                  ~ NA_character_
    ),
    Question6_4 = case_when(
      !is.na(x6_4_concordo_totalmente)   ~ "concordo totalmente",
      !is.na(x6_4_concordo)              ~ "concordo",
      !is.na(x6_4_nem_concordo_nem_discordo)     ~ "nem nem",
      !is.na(x6_4_discordo)              ~ "discordo",
      !is.na(x6_4_discordo_totalmente)   ~ "discordo totalmente",
      !is.na(x6_4_ns_nr)                 ~ "NS/NR",
      TRUE                              ~ NA_character_
    ),
    Question6_5 = case_when(
      !is.na(x6_5_sim)      ~ "sim",
      !is.na(x6_5_nao)      ~ "não",
      !is.na(x6_5_ns_nr)    ~ "NS/NR",
      TRUE                  ~ NA_character_
    ),
    Question6_6 = case_when(
      !is.na(x6_6_concordo_totalmente)   ~ "concordo totalmente",
      !is.na(x6_6_concordo)              ~ "concordo",
      !is.na(x6_6_nem_concordo_nem_discordo)     ~ "nem nem",
      !is.na(x6_6_discordo)              ~ "discordo",
      !is.na(x6_6_discordo_totalmente)   ~ "discordo totalmente",
      !is.na(x6_6_ns_nr)                 ~ "NS/NR",
      TRUE                              ~ NA_character_
    ),
    Question6_7 = case_when(
      !is.na(x6_7_aumentara)            ~ "aumentara",
      !is.na(x6_7_aumentara_muito)      ~ "aumentara muito",
      !is.na(x6_7_manter_se_a)          ~ "manter",
      !is.na(x6_7_diminuira)            ~ "diminuira",
      !is.na(x6_7_diminuira_muito)      ~ "dimiuira muito",
      !is.na(x6_7_ns_nr)                ~ "NS/NR",
      TRUE                             ~ NA_character_
    ),
    Question6_8 = case_when(
      !is.na(x6_8_aumentara)            ~ "aumentara",
      !is.na(x6_8_aumentara_muito)      ~ "aumentara muito",
      !is.na(x6_8_manter_se_a)          ~ "manter",
      !is.na(x6_8_diminuira)            ~ "diminuira",
      !is.na(x6_8_diminuira_muito)      ~ "dimiuira muito",
      !is.na(x6_8_ns_nr)                ~ "NS/NR",
      TRUE                             ~ NA_character_
    ),
    Question9 = case_when(
      !is.na(x9_feminino)               ~ "feminino",
      !is.na(x9_masculino)              ~ "masculino",
      TRUE                             ~ NA_character_
    )
  ) %>% 
  select(-c(x6_2_sim:x6_8_ns_nr)) %>% 
  select(-c(x9_masculino:x9_feminino)) %>% 
  select(-c(x1_local:x1_ns_nr))

survey <- survey %>% 
  mutate(x3_poliarte = as.numeric(rowSums(select(., x3_cerco, x3_covos_alcatruzes, x3_toneiras, x3_redes_de_emalhar_tresmalho, x3_arrasto, x3_palangre) == 1, na.rm=TRUE) > 1)) %>% 
  select(-c(x3_cerco:x3_outras_quais))

BD$MES <- as.character(BD$MES)

# Peso por especie por mes
BD_sem_rep <- BD %>% select(MES,NOME_ESPECIE,NOME_CIENTIFICO,QUANT_KGS) %>% 
  group_by(NOME_ESPECIE, MES) %>% 
  summarise(total_weight = sum(QUANT_KGS))

# Peso por especie no ano inteiro
BD_2 <- BD %>% select(MES,NOME_ESPECIE,NOME_CIENTIFICO,QUANT_KGS) %>% 
  group_by(NOME_ESPECIE) %>% 
  summarise(total_weight = sum(QUANT_KGS))

# Ocorrencia de esp?cies por m?s (1=sim, 0= nao)
species_month <- data.frame(xtabs(~BD_sem_rep$NOME_ESPECIE + BD_sem_rep$MES)) %>% 
  pivot_wider(names_from = BD_sem_rep.MES , values_from = Freq)
species_month



# freq especies (se aparece todos os meses ou n, se temos epocas)

# calcular pre?o medio, peso,  de cada especie mensal, trimestral, semestral e anual
# dividir por portos (geral e por especie, por especie por total porto)
# tipo de arte de pesca (geral, quantidade de peixe por arte, por porto)
# tipologia (freq, quantidade de peixe, por porto)
#

##### DATA VALIDATION #####





# export
write.xlsx(BD_sem_rep, "teste.xlsx", sheetName = "Sheet1", rowNames = FALSE)

write.xlsx(BD_2, "teste1.xlsx", sheetName = "Sheet1", rowNames = FALSE)
write.xlsx(survey, "quest_limpo.xlsx", rowNames = FALSE)
