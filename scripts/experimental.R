#Creado por Irvin Rojas
#Para la Incubadora de Evaluaciones 2022
#Datos de ejemplo provenientes de:
#Ruben Irvin Rojas Valdes, Bruce Wydick, Travis J Lybbert, Can hope elevate microfinance? Evidence from Oaxaca, Mexico,
#Oxford Economic Papers, Volume 74, Issue 1, January 2022, Pages 236–264.
#https://academic.oup.com/oep/article-abstract/74/1/236/6154385
#Más detalles en:
#http://oaxacahope-project.weebly.com/

rm(list = ls()) 
options(scipen=999) 

library(tidyverse)
library(sandwich)
library(modelsummary) # para hacer tablas
library(stargazer)
library(sandwich)
library(clubSandwich)

#0. Cargamos datos----
data <- haven::read_dta("data/analysis_hope.dta") 



#1. El tratamiento se aleatorizó de manera correcta

#Datos de línea base
bl <- data %>% 
  filter(t==0)

summary(bl$educ)

#Por grupos
bl %>% 
  group_by(hopegroup) %>%
  summarize(mean=mean(educ,na.rm=T)) %>% 
  ungroup()

#Prueba de diferencia de medias

t.test(educ ~ hopegroup,
       data = bl)

summary(m1 <- lm(educ ~ hopegroup,
                 data = bl))$coef


#Errores robustos y agrupados
#Nota: CR0 es el estimador de sandwich agrupado de Liang & Zeger (1986) que no hace
#correción de muestras pequeñas
#Algunas modificaciones multiplican CR0 por un factor
#CR1 = CR01 * [G / (G - 1)]
#CR1S = stata =  (G (N-1)) / [(G - 1)(N - k)]


stargazer(m1, m1,
          column.labels = c("Err. Clásicos", "Err. Agrupados"),
          se = list(NULL,
                    sqrt(diag(vcovCR(m1, type = "CR1S", cluster=bl$communitybank)))),
          type="text",
          keep.stat = "n",
          digits = 4)





#Para el resto de los covariables
m2 <- lm(age ~ hopegroup,
         data = bl)

m3 <- lm(evangelical ~ hopegroup,
                data = bl)

m4 <- lm(children ~ hopegroup,
                data = bl)

m5 <- lm(bankleader ~ hopegroup,
                data = bl)

stargazer(m1, m2, m3, m4, m5,
          dep.var.labels.include = F,
          dep.var.caption	= "",
          covariate.labels = c("Tratado","Constante (media en control)"),
          column.labels = c("Educación", "Edad", "Evangélica", "Hijos", "Líder" ),
          se = list(sqrt(diag(vcovCR(m1, type = "CR1S", cluster=bl$communitybank))),
                    sqrt(diag(vcovCR(m2, type = "CR1S", cluster=bl$communitybank))),
                    sqrt(diag(vcovCR(m3, type = "CR1S", cluster=bl$communitybank))),
                    sqrt(diag(vcovCR(m4, type = "CR1S", cluster=bl$communitybank))),
                    sqrt(diag(vcovCR(m5, type = "CR1S", cluster=bl$communitybank)))),
          type="text",
          keep.stat = "n",
          digits = 4)


#En general, los covariables están bien balanceados








#2. Estimación de efectos de tratamiento----
#Usamos datos de un año después de la intervención
el <- data %>% 
  filter(t==2)

#Regresión corta
summary(e1corta <- lm(StdAnderAgencyIndex ~ hopegroup,
                         data = el))$coef

#Regresión larga
summary(e1 <- lm(StdAnderAgencyIndex ~ hopegroup + age + educ + evangelical + children + children_under_18 + bankleader + Dwelling_Index + factor(pair_number),
                        data = el))$coef


stargazer(e1corta, e1,
          dep.var.labels.include = F,
          dep.var.caption	= "",
          column.labels = c("Corta", "Larga" ),
          se = list(sqrt(diag(vcovCR(e1corta, type = "CR1S", cluster=el$communitybank))),
                    sqrt(diag(vcovCR(e1, type = "CR1S", cluster=el$communitybank)))),
          type="text",
          keep.stat = "n",
          keep = "hopegroup",
          digits = 4)


#Efecto en un índice agregado de los tres componentes de la esperanza

#Hope 3
summary(e6 <- lm(StdAnderHope3Index ~ hopegroup + age + educ + evangelical + children + children_under_18 + bankleader + Dwelling_Index + factor(pair_number),
                        data = el))$coef[,c(1:2,4)]

stargazer(e1, e6,
          dep.var.labels.include = F,
          dep.var.caption	= "",
          column.labels = c("Agencia", "Hope 3" ),
          se = list(sqrt(diag(vcovCR(e1, type = "CR1S", cluster=el$communitybank))),
                    sqrt(diag(vcovCR(e6, type = "CR1S", cluster=el$communitybank)))),
          type="text",
          keep.stat = "n",
          keep = "hopegroup",
          digits = 4)


#Tarea: ¿cuál es el impacto en los otros dos componentes de la esperanza
#(aspiraciones, StdAnderAspIndex, y avenidas, StdAnderAveIndex) y en un 
#índice de desempeño de los negocios (StdAnderBPIndex)?



