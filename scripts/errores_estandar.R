rm(list = ls()) 
options(scipen=999) 

library(tidyverse)
library(sandwich)
library(clubSandwich)
library(foreign)


data.des <- haven::read_dta("data/elemapi2.dta")

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
         vcovCL(m.mco, cluster=data.des$dnum))

stargazer(m.mco, m.mco, m.mco,
          column.labels = c("Homoc.", "HC0", "Agrupados"),
          se = list(NULL,
                    sqrt(diag(vcovHC(m.mco, type = "HC0"))),
                    sqrt(diag(vcovCL(m.mco, cluster=data.des$dnum)))),
          type="text",
          keep=c("acs_k3", "acs_46"),
          keep.stat = "n",
          digits = 4)
