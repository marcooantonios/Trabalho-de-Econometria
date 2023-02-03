#Trabalho de Econometria 
#Tema: Fatores que impactam a inovação: uma comparação entre Minas Gerais e Ceará

{
library(wooldridge)
library(basedosdados)
library(bigrquery)
library(readxl)
library(data.table)
library(tidyverse)
library(dbplyr)
library('stargazer')
}

#####################################################
#Para importar da maneira como foi demonstrado é importante saber onde está os arquivos da base de dados
{
#Importando a base de dados do ISDEL para Minas Gerais
ISDEL_MG_XL <- read_excel("C:/Users/marco/OneDrive/Área de Trabalho/Trabalho de Econometria/Base de Dados/ISDEL_MG_XL.xlsx")
View(ISDEL_MG_XL)

#Exluindo algumas colunas que não serão usadas e restando as que poderão ser utilizadas.
ISDEL_MG_XL <- subset(ISDEL_MG_XL, select = c("Ano","UF","Mun_Cod","Município","Capital Empreendedor","Tecido Empresarial",
"Governança para o Desenvolvimento","Organização Produtiva","Inserção Competitiva","Educação","Educação Empreendedora","Redes de Empresas","Estrutura Produtiva","Inovação",
"Comércio Internacional","Complexidade","Depósitos de patentes","Depósitos de Desenho Industrial por Município","Complexidade econômica"))

#Importando dados sobre Escolarida com dados da RAIS de Minas Gerais
Escolaridade_MG_XL <- read_excel("C:/Users/marco/OneDrive/Área de Trabalho/Trabalho de Econometria/Base de Dados/Escolaridade_MG_XL.xlsx")
View(Escolaridade_MG_XL)

#Iremos selecionar apenas Superior Completo, Mestrado e Doutorado
Escolaridade_MG_XL <- subset(Escolaridade_MG_XL, select = c("Populacao Estimada","Estado","Município","Superior Completo","Mestrado","Doutorado"))

######################################################

#Importando a base de dados do ISDEL para o Ceara
ISDEL_CE_XL <- read_excel("C:/Users/marco/OneDrive/Área de Trabalho/Trabalho de Econometria/Base de Dados/ISDEL_CE_XL.xlsx")
View(ISDEL_CE_XL)

#Exluindo algumas colunas que não serão usadas e restando as que poderão ser utilizadas.
ISDEL_CE_XL <- subset(ISDEL_CE_XL, select = c("Ano","UF","Mun_Cod","Município","Capital Empreendedor","Tecido Empresarial",
"Governança para o Desenvolvimento","Organização Produtiva","Inserção Competitiva","Educação","Educação Empreendedora","Redes de Empresas","Estrutura Produtiva","Inovação",
"Comércio Internacional","Complexidade","Depósitos de patentes","Depósitos de Desenho Industrial por Município","Complexidade econômica"))

#Importando dados sobre Escolarida com dados da RAIS do Ceara
Escolaridade_CE_XL <- read_excel("C:/Users/marco/OneDrive/Área de Trabalho/Trabalho de Econometria/Base de Dados/Escolaridade_CE_XL.xlsx")
View(Escolaridade_CE_XL)

#Iremos selecionar apenas Superior Completo, Mestrado e Doutorado
Escolaridade_CE_XL <- subset(Escolaridade_CE_XL, select = c("Populacao Estimada","Estado","Município","Superior Completo","Mestrado","Doutorado"))

}

###############################################################
{
#Construindo as variávies para Minas Gerais
MESTRADO_MG <- (Escolaridade_MG_XL$Mestrado/Escolaridade_MG_XL$`Populacao Estimada`)
DOUTORADO_MG <- (Escolaridade_MG_XL$Doutorado/Escolaridade_MG_XL$`Populacao Estimada`)
LOG_ESTRUTURA_PRODUTIVA_MG <- log(ISDEL_MG_XL$`Estrutura Produtiva`)
LOG_INSERCAO_COMPETITIVA_MG <- log(ISDEL_MG_XL$`Inserção Competitiva`)
COMPLEXIDADE_ECONOMICA_MG <- ISDEL_MG_XL$`Complexidade econômica`
N_PATENTES_MG <- ISDEL_MG_XL$`Depósitos de patentes`


#Construindo as varáveis para o Ceara
MESTRADO_CE <- (Escolaridade_CE_XL$Mestrado/Escolaridade_CE_XL$`Populacao Estimada`)
DOUTORADO_CE <- (Escolaridade_CE_XL$Doutorado/Escolaridade_CE_XL$`Populacao Estimada`)
LOG_ESTRUTURA_PRODUTIVA_CE <- log(ISDEL_CE_XL$`Estrutura Produtiva`)
LOG_INSERCAO_COMPETITIVA_CE <- log(ISDEL_CE_XL$`Inserção Competitiva`)
COMPLEXIDADE_ECONOMICA_CE <- ISDEL_CE_XL$`Complexidade econômica`
N_PATENTES_CE <- ISDEL_CE_XL$`Depósitos de patentes`
}
###############################################################
{
#Realizando a regressão para Minas Gerais 
LM_MG_1 <- lm(N_PATENTES_MG~LOG_ESTRUTURA_PRODUTIVA_MG)

LM_MG_2 <- lm(N_PATENTES_MG~LOG_ESTRUTURA_PRODUTIVA_MG+LOG_INSERCAO_COMPETITIVA_MG)

LM_MG_3 <- lm(N_PATENTES_MG~LOG_ESTRUTURA_PRODUTIVA_MG+LOG_INSERCAO_COMPETITIVA_MG+COMPLEXIDADE_ECONOMICA_MG)

LM_MG_4 <- lm(N_PATENTES_MG~LOG_ESTRUTURA_PRODUTIVA_MG+LOG_INSERCAO_COMPETITIVA_MG+COMPLEXIDADE_ECONOMICA_MG+MESTRADO_MG)

LM_MG <- lm(N_PATENTES_MG~LOG_ESTRUTURA_PRODUTIVA_MG+LOG_INSERCAO_COMPETITIVA_MG+COMPLEXIDADE_ECONOMICA_MG+MESTRADO_MG+DOUTORADO_MG)
summary(LM_MG)

stargazer(LM_MG_1,LM_MG_2,LM_MG_3,LM_MG_4,LM_MG,type = "text",title = "Resultados",align = T)

#Realizando a regressão para o Ceara
LM_CE_1 <- lm(N_PATENTES_CE~LOG_ESTRUTURA_PRODUTIVA_CE)

LM_CE_2 <- lm(N_PATENTES_CE~LOG_ESTRUTURA_PRODUTIVA_CE+LOG_INSERCAO_COMPETITIVA_CE)

LM_CE_3 <- lm(N_PATENTES_CE~LOG_ESTRUTURA_PRODUTIVA_CE+LOG_INSERCAO_COMPETITIVA_CE+COMPLEXIDADE_ECONOMICA_CE)

LM_CE_4 <- lm(N_PATENTES_CE~LOG_ESTRUTURA_PRODUTIVA_CE+LOG_INSERCAO_COMPETITIVA_CE+COMPLEXIDADE_ECONOMICA_CE+MESTRADO_CE)

LM_CE <- lm(N_PATENTES_CE~LOG_ESTRUTURA_PRODUTIVA_CE+LOG_INSERCAO_COMPETITIVA_CE+COMPLEXIDADE_ECONOMICA_CE+MESTRADO_CE+DOUTORADO_CE)
summary(LM_CE)

stargazer(LM_CE_1,LM_CE_2,LM_CE_3,LM_CE_4,LM_CE,type = "text",title = "Resultados",align = T)
}

#####################################################
#Ver se os dados se encaixam com a reta traçada
plot(LM_MG_1)
LM_MG_IC <- lm(N_PATENTES_MG~INSERCAO_COMPETITIVA_MG)
plot(LM_MG_IC)
LM_MG_CE <- lm(N_PATENTES_MG~COMPLEXIDADE_ECONOMICA_MG)
plot(LM_MG_CE)
LM_MG_ME <- lm(N_PATENTES_MG~MESTRADO_MG)
plot(LM_MG_ME)
LM_MG_DO <- lm(N_PATENTES_MG~DOUTORADO_MG)
plot(LM_MG_DO)
#####################################################
#Teste F com as variáveis de Inserção Competitiva e Complexidade Econômica
 LM_Teste_F_CE_1 <- lm(COMPLEXIDADE_ECONOMICA_MG~INSERCAO_COMPETITIVA_MG)
summary(LM_Teste_F_CE_1)

LM_Teste_F_IC_1 <- lm(LOG_INSERCAO_COMPETITIVA_MG~COMPLEXIDADE_ECONOMICA_MG)
summary(LM_Teste_F_IC_1)


cov(COMPLEXIDADE_ECONOMICA_MG,LOG_INSERCAO_COMPETITIVA_MG)
>0.3909907
cor(COMPLEXIDADE_ECONOMICA_MG,LOG_INSERCAO_COMPETITIVA_MG)
>0.8906328
