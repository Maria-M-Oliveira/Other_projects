# LIBRARIES
library(tidyverse)
library(ggplot2)
library(readxl)
library(openxlsx)
# Files
BD <- read_xlsx(".\\Dani\\UEvora_Sines_2021.xlsx")


# Cleaning data
BD$MES <- as.character(BD$MES)

# Peso por especie por mes
BD_sem_rep <- BD %>% select(MES,NOME_ESPECIE,NOME_CIENTIFICO,QUANT_KGS) %>% 
  group_by(NOME_ESPECIE, MES) %>% 
  summarise(total_weight = sum(QUANT_KGS))

# Peso por especie no ano inteiro
BD_2 <- BD %>% select(MES,NOME_ESPECIE,NOME_CIENTIFICO,QUANT_KGS) %>% 
  group_by(NOME_ESPECIE) %>% 
  summarise(total_weight = sum(QUANT_KGS))

# Ocorrencia de espécies por mês (1=sim, 0= nao)
species_month <- data.frame(xtabs(~BD_sem_rep$NOME_ESPECIE + BD_sem_rep$MES)) %>% 
  pivot_wider(names_from = BD_sem_rep.MES , values_from = Freq)
species_month



# freq especies (se aparece todos os meses ou n, se temos epocas)

# calcular preço medio, peso,  de cada especie mensal, trimestral, semestral e anual
# dividir por portos (geral e por especie, por especie por total porto)
# tipo de arte de pesca (geral, quantidade de peixe por arte, por porto)
# tipologia (freq, quantidade de peixe, por porto)
#

##### DATA VALIDATION #####





# export
write.xlsx(BD_sem_rep, "teste.xlsx", sheetName = "Sheet1", rowNames = FALSE)

write.xlsx(BD_2, "teste1.xlsx", sheetName = "Sheet1", rowNames = FALSE)