#Datos en panel

rm(list = ls()) 
options(scipen=999) 

library(tidyverse)
library(foreign)
library(sandwich)
library(stargazer)
library(AER)
library(plm)

#Datos en formato wide
data.comp <- read_csv("data/comportamiento_wide.csv",
                      locale = locale(encoding = "latin1"))

#Pasamos los datos a formato long
data.comp <- data.comp %>% 
  pivot_longer(cols = c(anti90:anti94, self90:self94, pov90:pov94),
               names_to = c(".value", "year"),
               names_pattern =  "([A-Za-z]+)(\\d+)")



#MCO ignorando estructura de panel
summary(m.mco.a <- lm(anti ~ self + pov,
                       data = data.comp))


summary(m.mco.b <- plm(anti ~ self + pov,
                     data = data.comp,
                     model = "pooling",
                     index = c("id","year")))

stargazer(m.mco.a, m.mco.b,
          type = 'text')



#Aunque estemos dispuestos a asumir que el modelo es el pooled
#debemos tomar en cuenta correlación serial

#La matriz CR1S multiplica la matriz agrupada por (m (N-1)) / [(m - 1)(N - p)]
stargazer(m.mco.a, m.mco.b, m.mco.b,
          type = 'text',
          se = list(NULL,
                    NULL,
                    sqrt(diag(vcovCR(m.mco.b, type = "CR1S", cluster = data.comp$id)))))



#Efectos fijos

m.fe <- plm(anti ~ self + pov,
            data = data.comp,
            model = "within",
            index = c("id", "year"))

stargazer(m.mco.b, m.fe,
          type = 'text',
          se = list(sqrt(diag(vcovCR(m.mco.b, type = "CR1S", cluster = data.comp$id))),
                    sqrt(diag(vcovCR(m.fe, type = "CR1S", cluster = data.comp$id)))))



#Efectos aleatorios

m.re <- plm(anti ~ self + pov,
            data = data.comp,
            model = "rando",
            index = c("id", "year"))

stargazer(m.mco.b, m.fe, m.re,
          type = 'text',
          se = list(sqrt(diag(vcovCR(m.mco.b, type = "CR1S", cluster=data.comp$id))),
                    sqrt(diag(vcovCR(m.fe, type = "CR1S", cluster=data.comp$id))),
                    sqrt(diag(vcovCR(m.re, type = "CR1S", cluster=data.comp$id)))))




#Qué pasa si incluimos la raza en efectos fijos
summary(plm(anti ~ self + pov + black,
    data = data.comp,
    model = "within",
    index = c("id", "year")))





#Prueba de Hausman

phtest(m.fe, m.re)

phtest(anti ~ self + pov,
       data = data.comp,
       index = c("id", "year"))

phtest(anti ~ self + pov,
       data = data.comp,
       method = "aux",
       index = c("id", "year"),
       vcov = function(x) vcovCR(x, type="CR1S", cluster=data.comp$id))




#Equivalencia de estimadores

data.comp.sub <- data.comp %>% 
  filter(year!=90) %>% 
  select(id, year, anti, self, pov)


#Efectos fijos es MCO con dummies

m.fe <- lm(anti ~ self + pov + factor(id),
                data.comp.sub)


m.dummies <- lm(anti ~ self + pov + factor(id),
                data.comp.sub)

stargazer(m.fe, m.dummies,
          type = "text",
          keep=c("self", "pov"))


#Efectos fijos es equivalente (los coeficientes) a MCO a las primeras diferencias

data.comp.sub <- data.comp.sub %>% 
  group_by(id) %>% 
  mutate(danti=anti-dplyr::lag(anti, order_by = year),
         dself=self-dplyr::lag(self, order_by = year),
         dpov=pov-dplyr::lag(pov, order_by= year)) %>% 
  ungroup()


m.dif <- lm(danti ~ -1 + dself + dpov,
                data.comp.sub)

stargazer(m.fe, m.dummies, m.dif,
          type = "text",
          keep=c("self", "pov", "dself", "dpov"))


