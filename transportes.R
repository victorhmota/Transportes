install.packages('car')
install.packages('lmtest')
install.packages('MASS')

library(tidyverse)
library(devtools)
library(car)
library(lmtest)
library(MASS)

transportes <- read.csv('gastos_transporte.csv')
transportes$UF <- sapply(transportes$UF, as.character)

modelo <- lm(lnY ~ lnX2 + lnX3, data = transportes)
summary(modelo)

#Calculando o FIV

fiv <- vif(modelo)

#Testes de heterocedasticidade

#Breausch-Pegan:

bp <- bptest(modelo)

#Estimando uma Regressão Linear Robusta

modelo_robusta <- rlm(lnY ~ lnX2 + lnX3, data = transportes)
summary(modelo_robusta)


#Separando por UF:

norte <- transportes %>% filter(substr(transportes$UF, 1, 1) == '1')
nordeste <- transportes %>% filter(substr(transportes$UF, 1, 1) == '2')
sudeste <- transportes %>% filter(substr(transportes$UF, 1, 1) == '3')
sul <- transportes %>% filter(substr(transportes$UF, 1, 1) == '4')
centro <- transportes %>% filter(substr(transportes$UF, 1, 1) == '5')

#Regredindo para as regiões.

modelo_norte <- rlm(lnY ~ lnX2 + lnX3, data = norte)
modelo_nordeste <- rlm(lnY ~ lnX2 + lnX3, data = nordeste)
modelo_sudeste  <- rlm(lnY ~ lnX2 + lnX3, data = sudeste)
modelo_sul <- rlm(lnY ~ lnX2 + lnX3, data = sul)
modelo_centro <- rlm(lnY ~ lnX2 + lnX3, data = centro)

summary(modelo_norte)
summary(modelo_nordeste)
summary(modelo_sudeste)
summary(modelo_sul)
summary(modelo_centro)


#Para faixas de renda diferente. 2 salários minímos = 2200, 10 salário mínimos = 11000

dois_sal <- transportes %>% filter(transportes$RENDA_TOTAL <= 2200.0)
dez_sal <- transportes %>% filter(transportes$RENDA_TOTAL >= 11000.0)

#BP teste

bp_dois <- bptest(teste)

#Regredindo para as faixas de Renda

modelo_dois_sal <- rlm(lnY ~ lnX2 + lnX3, data = dois_sal, psi = psi.hampel)
modelo_dez_sal <- rlm(lnY ~ lnX2 + lnX3, data = dez_sal, psi = psi.hampel)


summary(modelo_dois_sal)$r.squared
summary(modelo_dez_sal)




