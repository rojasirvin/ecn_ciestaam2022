#Modelos de conteo
rm(list = ls()) 
options(scipen=999) 

library(tidyverse)
library(foreign)
library(survival) # construye objetos de sobrevivencias
library(survminer) # incluye ggsurvplot para graficar curvas de sobrevivencia
library(ggsurvplot)
library(flexsurv)


data.huelgas <- read.dta("data/strkdur.dta")

#Debemos crear un objeto de duración

h.surv <- Surv(data.huelgas$dur) 

#Estimación de la curva de K-M

km <- survfit(Surv(data.huelgas$dur)~1,
              data = data.huelgas,
              type = "kaplan-meier")

summary(km)

#Gráfico de K-M
ggsurvplot(fit =km,
           data =data.huelgas,
           conf.int = TRUE,
           title = "Curva de sobrevivencia",
           xlab = "Tiempo (días)",
           ylab = "Probabilidad de sobrevivencia", 
           legend.labs = "Kaplan-Meier")



#La curva N-A

ggsurvplot(km,
           fun = "cumhaz",
           censor = F,
           xlab = "Tiempo (días)",
           ylab = "Riesgo acumulado",
           title = "Curva de riesgo acumulado",
           legend.title = "Duración de huelgas contractuales")



#Estimación paramétrica

data.desemp <- read.dta("data/ema1996.dta")

#censor1 indica si encontró un trabajo de tiempo completo
Surv(data.desemp$spell,data.desemp$censor1) 

km <- survfit(Surv(data.desemp$spell
                   data.desemp$censor1) ~1,
        data = data.desemp,
        type = "kaplan-meier")

summary(km)

#Gráfico de K-M
ggsurvplot(fit = km,
           data = data.desemp,
           conf.int = TRUE,
           title = "Curva de sobrevivencia",
           xlab = "Quincenas",
           ylab = "Probabilidad de sobrevivencia", 
           legend.title = "Kaplan-Meier")


#Gráfico A-N


ggsurvplot(km,
           fun = "cumhaz",
           censor = F,
           xlab = "Quincenas",
           ylab = "Riesgo acumulado",
           title = "Curva de riesgo acumulado",
           legend.title = "Nelson-Aalen")



#Estimación paramétrica

lognormal.model<-psm(Surv(T,status)~1,strikes,dist="lognormal")

data.desemp <- data.desemp %>% 
  mutate(disrateui=disrate*ui,
         reprateui=reprate*ui)

survfit(Surv(data.desemp$spell,
                 data.desemp$censor1) ~
              reprate + disrate + ui + disrateui +
              reprateui + logwage,
        data = data.desemp,
        dist = "exponential")



