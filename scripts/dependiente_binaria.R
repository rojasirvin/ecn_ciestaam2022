#Variable dependiente binaria

#Iniciamos una nueva sesión quitando todos los elementos previos
rm(list = ls()) 
#Para evitar la notación científica
options(scipen=999) 

library(tidyverse)
library(modelsummary)

setwd("C:/Users/rojas/Dropbox/presentations_sites/ecn_ciestaam2022")


#Datos
data_binary <- read_csv("./data/fishing_data_clean.csv",
                       locale = locale(encoding = "latin1"))


mprobit <- glm(charter ~ lnrelp,
               family = binomial(link = "probit"), 
               data = data_binary)

#Pedimos los coeficientes
summary(mprobit)$coef




mlogit <- glm(charter ~ lnrelp,
              family = binomial(link = "logit"),
              data = data_binary)

mlineal <- lm(charter ~ lnrelp,
              data=data_binary)


#Resumen de los tres modelos
msummary(models=list(mlogit, mprobit, mlineal))


#Podemos ponerlo más bonito
msummary(models=list('Logit' = mlogit,
                     'Probit' = mprobit,
                     'Lineal' = mlineal),
         coef_map = c('(Intercept)' = 'Constante',
                      'lnrelp' = 'ln(precio rel.)'))





#Predicciones con cada modelo
data_binary <- data_binary %>% 
  mutate(plogit=predict(mlogit, type="response")) %>% 
  mutate(pprobit=predict(mprobit, type="response")) %>% 
  mutate(plineal=predict(mlineal, type="response")) 
  


#Colecciono las variables que usaré
data_binary <- data_binary %>% 
  select(lnrelp, plogit, pprobit, plineal)



#Arreglo en formato long (lo usaremos más en panel)
data_binary <- pivot_longer(data_binary,
                            cols= c("plogit","pprobit","plineal"),
                            names_to="Modelo",
                            values_to = "prob")

#Construyo la gráfica
data_binary %>% 
  ggplot()+
  geom_line(aes(x=lnrelp,y=prob, color=Modelo))

