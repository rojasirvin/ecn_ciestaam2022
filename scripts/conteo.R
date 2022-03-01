#Modelos de conteo
rm(list = ls()) 
options(scipen=999) 

library(tidyverse)

#Paquetes a usar
#install.packages('pastecs') # otro auxiliar para estadística descriptiva
#install.packages('MASS') # Incluye la estimación del modelo NB2
#install.packages('sjPlot) # Resumen de modelos estimados

data.phd <- read_csv("data/phd_articulos.csv")


#Descriptiva
#Vean como podemos usar los paquetes sin cargar

pastecs::stat.desc(data.phd$art)

summary(data.phd$art)



#Modelo Poisson----

mpoisson <- glm(art ~ factor(female) + factor(married) + kid5 + phd + mentor,
                family="poisson",
                data=data.phd)

summary(mpoisson)


#Hacer tablas bonitas

mpoisson.table <- sjPlot::tab_model(mpoisson,
                                    collapse.se = TRUE,
                                    show.ci = F,
                                    show.r2  =F,
                                    wrap.labels = 35,
                                    p.style = "stars",
                                    p.threshold = c(0.1, 0.05, 0.01))



#Cambiamos los factores para ver mejor el efecto de la variable female
table(data.phd$female)

data.phd <- data.phd %>% 
  mutate(female=factor(female,
                       levels=c('Male','Female')))

mpoisson <- glm(art ~ factor(female) + factor(married) + kid5 + phd + mentor,
                family="poisson",
                data=data.phd)

summary(mpoisson)



#Razón de tasas de incidencia (IRR)


exp(summary(mpoisson)$coef)




#NB2----

mnb2 <- MASS::glm.nb(art ~
                       factor(female) + factor(married) + kid5 + phd + mentor,
                     data = data.phd)
summary(mnb2)

#A diferencia de otros paquetes (Stata por ejemplo), glm.nb reporta \theta=1/\alpha
(alpha <- 1/summary(mnb2)$theta)  


#Comparemos resultados
exp(summary(mnb2)$coef)

exp(summary(mpoisson)$coef)