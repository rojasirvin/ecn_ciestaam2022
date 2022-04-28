# Espacio de trabajo ----
rm(list = ls())
options(scipen=999) # Prevenir notación científica

library(tidyverse)
library(rdrobust)



#Diseños discontinuos----

data.hs <- read_csv("data/datos_pobreza.csv")

#Usaremos un paquete llamado rdrobust
#Recuerden que hay que instalarlo y luego llamarlo


#Descripción gráfica
#Sabemos que los municipios que reciben el programa son los que están por
#arriba de 59.1968 puntos de pobreza

x0 <- 59.1968

(rdplot(y = data.hs$mort_age59_related_postHS,
        x = data.hs$povrate60,
        c = x0,
        nbins = 40,
        p = 1))


(rdplot(y = data.hs$mort_age59_related_postHS,
        x = data.hs$povrate60,
        c = x0,
        nbins = 40,
        p = 2))



#Creamos una variable que indica a los municipios pobres
data.hs <- data.hs %>% 
  mutate(ispoor=ifelse(povrate60>=x0,1,0))

#Noten que debemos seleccionar la ventana
b <- 10

summary(lm(mort_age59_related_postHS ~ povrate60 + ispoor,
           data=filter(data.hs, povrate60>=x0-b & povrate60<=x0+b)))


#Y ahora un modelo con un polinomio de orden 2

summary(lm(mort_age59_related_postHS ~ povrate60  + I(povrate60^2)+ ispoor,
           data=filter(data.hs, povrate60>=x0-b & povrate60<=x0+b)))







#Mostremos cómo las características observables no varían en el corte

#Población: census1960_pop
(rdplot(y = data.hs$census1960_pop,
        x = data.hs$povrate60,
        c = x0,
        nbins = 40,
        p = 3))

#Población urbana: census1960_pcturban
(rdplot(y = data.hs$census1960_pcturban,
        x = data.hs$povrate60,
        c = x0,
        nbins = 40,
        p = 3))

#Población de raza negra: census1960_pcturban
(rdplot(y = data.hs$census1960_pctblack,
        x = data.hs$povrate60,
        c = x0,
        nbins = 40,
        p = 3))





#Ejercicio 1

#Estimemos ahora el modelo con discontinuidades con una ventana de
#15 puntos
#5 puntos

#15 puntos
b <-15
summary(lm(mort_age59_related_postHS ~ povrate60  + I(povrate60^2)+ ispoor,
           data=filter(data.hs, povrate60>=x0-b & povrate60<=x0+b)))


#Ahora 5
b <-5
summary(lm(mort_age59_related_postHS ~ povrate60  + I(povrate60^2)+ ispoor,
           data=filter(data.hs, povrate60>=x0-b & povrate60<=x0+b)))

#Ahora agreguemos un polinomio de orden 3 y una ventana de 10 puntos
b <- 10
summary(lm(mort_age59_related_postHS ~ povrate60  + I(povrate60^2) + I(povrate60^3)+ ispoor,
           data=filter(data.hs, povrate60>=x0-b & povrate60<=x0+b)))


#Verificar paramétricamente que la población no cambia en el corte
summary(lm(census1960_pop ~ povrate60  + I(povrate60^2) + I(povrate60^3)+ ispoor,
           data=filter(data.hs, povrate60>=x0-b & povrate60<=x0+b)))


