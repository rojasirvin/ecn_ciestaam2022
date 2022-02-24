#Inferencia estadística

rm(list = ls()) 
options(scipen=999) 

library(tidyverse)
library(modelsummary)
library(sandwich) # calcular las varianzas robustas
library(foreign) # usar datos de varios formaatos
library(lmtest) # test lineales
library(car) # car = companion to applied regression

setwd("C:/Users/rojas/Dropbox/presentations_sites/ecn_ciestaam2022")

#Datos de ingresos
data.ingresos <- read.dta("./data/edudat2.dta")

rmco <- lm(log(wage) ~ educ + gender + pareduc,
   data=data.ingresos)

#Los errores que se reportan por default son los homocedásticos
summary(rmco)




#Errores de White (usamos coeftest para los test y sandwich para el cálculo de matrices)
coeftest(rmco,
         vcov = vcovHC(rmco,
                       type = "HC0"))


#Prueba conjunta
linearHypothesis(rmco, c("educ=0",
                         "pareduc=0"))




#La misma interpretación en modelos no lineales
data.grogger<-read_csv("./data//grogger.csv",
                       locale = locale(encoding = "latin1"))

prob_probit <- glm(arr86 ~ pcnv+avgsen+tottime+ptime86+inc86+black+hispan+born60,
                   family = binomial(link = "probit"), 
                   data = data.grogger)

summary(prob_probit)

