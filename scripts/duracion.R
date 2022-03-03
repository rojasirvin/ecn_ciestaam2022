#Modelos de conteo

rm(list = ls()) 
options(scipen=999) 

#install.packages('survival')
#install.packages('survminer')

library(tidyverse)
library(foreign)
library(survival) # construye objetos de sobrevivencias
library(survminer) # incluye ggsurvplot para graficar curvas de sobrevivencia

data.rossi <- read_csv("data/data_rossi.csv",
                       locale = locale(encoding = "latin1")) 

#Objeto de sobrevivencia
km <- survfit(Surv(week, arrest) ~ 1,
              type = "kaplan-meier",
              data=data.rossi)

summary(km)


#KM
ggsurvplot(fit = km,
           data =data.rossi,
           conf.int = TRUE,
           title = "Curva de Supervivencia",
           xlab = "Semanas",
           ylab = "Probabilidad de no ser arrestado",
           legend.title = "Estimación",
           legend.labs = "Kaplan-Meier",
           ylim = c(0.65,1))


#NA
ggsurvplot(km,
           fun = "cumhaz",
           xlab = "Semanas",
           censor = T,
           ylab = "Riesgo Acumulado",
           title = "Riesgo Acumulado",
           legend.title = "Semanas sin ser arrestado")




#Modelo Weibull
sreg <- survreg(Surv(week, arrest) ~ fin + age + race + wexp + mar + paro + prio,
                data=data.rossi,
                dist = "weibull")

SurvRegCensCov::ConvertWeibull(sreg, conf.level = 0.95)



