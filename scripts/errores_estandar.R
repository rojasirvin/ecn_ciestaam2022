rm(list = ls()) 
options(scipen=999) 

library(tidyverse)
library(sandwich)
library(clubSandwich) #incluye vcovCR para varianzas con errores agrupados
library(foreign)
library(stargazer)
library(lmtest)
library(AER)

data.des <- haven::read_dta("data/elemapi2.dta")


#Tipos de errores estándar----

#Asumiendo homocedasticidad

summary(m.mco <- lm(api00 ~ acs_k3 + acs_46 + full + enroll,
                    data=data.des))$coef

#Errores robustos
coeftest(m.mco, vcov = vcovHC(m.mco, type = "HC0"))

 
stargazer(m.mco, m.mco,
          se = list(NULL,
                    sqrt(diag(vcovHC(m.mco, type = "HC0")))),
          type="text",
          keep=c("acs_k3", "acs_46"),
          keep.stat = "n")



#Estimamos diversas formas de la matriz de varianzas

stargazer(m.mco, m.mco, m.mco, m.mco, m.mco,
          column.labels = c("Homoc.", "HC0", "HC1", "HC2", "HC3"),
          se = list(NULL,
                    sqrt(diag(vcovHC(m.mco, type = "HC0"))),
                    sqrt(diag(vcovHC(m.mco, type = "HC1"))),
                    sqrt(diag(vcovHC(m.mco, type = "HC2"))),
                    sqrt(diag(vcovHC(m.mco, type = "HC3")))),
          type="text",
          keep=c("acs_k3", "acs_46"),
          keep.stat = "n",
          digits = 4)


#Errores agrupados

coeftest(m.mco,
         vcovCR(m.mco, cluster=data.des$dnum, type="CR1S"))

stargazer(m.mco, m.mco, m.mco,
          column.labels = c("Homoc.", "HC0", "Agrupados"),
          se = list(NULL,
                    sqrt(diag(vcovHC(m.mco, type = "HC0"))),
                    sqrt(diag(vcovCR(m.mco, type = "CR1S", cluster=data.des$dnum)))),
          type="text",
          keep=c("acs_k3", "acs_46"),
          keep.stat = "n",
          digits = 4)


#Bootstrap----

set.seed(927)

#Los datos que usamos en el problema de VI
data.ingresos <- read.csv("data/ingresos_iv.csv")

obs <- nrow(data.ingresos)
obs

#En la muestra original
mean(data.ingresos$lwage)

#Una muestra bootstrap
data.b <-data.ingresos[sample(nrow(data.ingresos),obs, replace = TRUE),]

mean(data.b$lwage)
  
#Otra muestra bootstrap
data.b <-data.ingresos[sample(nrow(data.ingresos),obs, replace = TRUE),]

mean(data.b$lwage)

#Obtenemos el error estándar del coeficiente de regresión
B=500

#Inicializamos el vector donde guardaremos los beta estimados
beta <- data.frame(beta=matrix(ncol = 1, nrow = B))

for (i in 1:B)
{
  data.b <-data.ingresos[sample(nrow(data.ingresos),obs, replace = TRUE),]
  
  #Corremos regresión
  
  m <- lm(lwage ~  educ + exper + black + south + married + smsa,
           data = data.b)
  
  #Guardamos en cada entrada el ratio estimado
  beta[i,1] <- as.numeric(m$coefficients[2])
}

#El error estimado es simplemente la desviación estándar de los B estadísticos estimados
sd(beta$beta)


#Comparamos con el error estimado con ivreg
summary(lm(lwage ~  educ + exper + black + south + married + smsa
           , data = data.ingresos))


#Ejercicio: obtenga el error estándar del coeficiente de MC2E estimado a mano.
#Es decir, para cada muestra bootstrap, estime la primera etapa y obtenga
#los valores ajustados de la educación. Luego estime una segunda
#etapa con estos errores ajustados como regresor en la ecuación estructural
#y coleccione el coeficiente estimado de la educación (ajustada) en la
#segunda etapa. Repita esto 500 veces y obtenga la desviación estándar de
#los coeficientes coleccionados. ¿Cómo se compara con lo que obtiene con
#ivreg?
