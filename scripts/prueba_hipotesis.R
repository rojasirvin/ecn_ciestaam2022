#Inferencia estadística

rm(list = ls()) 
options(scipen=999) 

#install.packages('sandwich')
#install.packages('foreign')
#install.packages('lmtest')
#install.packages('car')

library(tidyverse)
library(modelsummary)
library(sandwich) # calcular las varianzas robustas
library(foreign) # usar datos de varios formaatos
library(lmtest) # test lineales
library(car) # car = companion to applied regression (incluye pruebas no lineales)



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

#Pruebas de hipótesis no lineales
#Una prueba lineal es un caso especial de una prueba no lineal
deltaMethod(rmco, "pareduc", rhs = 0)

#Una prueba no lineal: razón de coeficientes
deltaMethod(rmco,
            "educ/pareduc",
            rhs = 3)





#La misma interpretación en modelos no lineales
data.grogger<-read_csv("./data/grogger.csv",
                       locale = locale(encoding = "latin1"))

prob_probit <- glm(arr86 ~ pcnv+avgsen+tottime+ptime86+inc86+black+hispan+born60,
                   family = binomial(link = "probit"), 
                   data = data.grogger)

summary(prob_probit)




#Importancia de los errores robustos----
set.seed(110)

#Generamos proceso heterocedástico
X <- rnorm(n=100,
           mean=0,
           sd = 5)
e <- rnorm(n=100,
           mean=0,
           sd = 2)
u <- X*e
b<- 1

Y <- 1 + b*X + u

#Modelo con errores homocedásticos
summary(reg.sim <- lm(Y ~ X))


#Veamos cómo luce esta simulación
data.sim <- data.frame(X,Y)

data.sim %>% ggplot(aes(x=X, y=Y)) +
  geom_point() +
  geom_smooth(method = 'lm')

coeftest(reg.sim,
         vcov = vcovHC(reg.sim,
                       type = "HC0"))


#Comparemos con los errores homocedásticos
coeftest(reg.sim,
         vcov = vcovHC(reg.sim,
                       type = "const"))


