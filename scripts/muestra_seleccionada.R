rm(list = ls()) 
options(scipen=999) 

library(tidyverse)
library(magick)
library(reticulate)
library(sandwich)
library(stargazer)
library(gtsummary)
library(foreign)
library(AER)
library(sampleSelection) #incluye la función heckit


data.part <- read.dta("./data/mroz.dta") 

data.part %>% 
  filter(hours<=3000) %>% 
  ggplot(aes(x=hours)) +
  geom_histogram()


#Tobit----


#Hacemos MCO ignorando la solución de esquina
mmco <- lm(hours ~ nwifeinc + educ + exper +
           expersq + age + kidslt6 + kidsge6,
         data = data.part)

#Si truncamos la muestra
mmcot <- lm(hours ~ nwifeinc + educ + exper +
             expersq + age + kidslt6 + kidsge6,
           data = filter(data.part,hours>0))

#Usando tobit
mtobit <- AER::tobit(hours ~ nwifeinc + educ + exper +
        expersq + age + kidslt6 + kidsge6,
        left = 0,
        data = data.part)


sjPlot::tab_model(mmco, mmcot, mtobit,
                  dv.labels = c("OLS (todas)", "OLS (h>0)", "Tobit"),
                  collapse.se = TRUE,
                  show.ci = F,
                  show.r2  =F,
                  wrap.labels = 35,
                  p.style = "stars",
                  p.threshold = c(0.1, 0.05, 0.01))


#Tobit usando censReg
mtobit.a <- censReg::censReg(hours ~ nwifeinc + educ + exper +
                               expersq + age + kidslt6 + kidsge6,
                             left = 0,
                             data = data.part)

#Efectos marginales en la media del modelo Tobit
margEff(mtobit.a)

#Para ciertos valores específicos
margEff(mtobit.a, c(1,9,5,2,4,25,0,0))



#¿Cómo hacemos esto a mano? Recordemos la fórmula
#El efecto en los datos censurados es Phi(x'b/sigma)*beta_k

#Calculemos x'b/sigma
#scale en el modelo tobit estimado es sigma

data.part <- data.part %>% 
  mutate(index = predict(mtobit),
         Phi = pnorm(index/mtobit$scale),
         efecto_educ = Phi*mtobit$coef[3])

#Obtenemos el promedio de los efectos marginales
summary(data.part$efecto_educ)




#El efecto marginal en la media

data.part <- data.part %>% 
  mutate(constant=1) %>% 
  mutate(prom_index=mean(index),
         Phi = pnorm(prom_index/mtobit$scale),
         efecto_educ_prom = Phi*mtobit$coef[3])
summary(data.part$efecto_educ_prom)


#Efecto evaluado en las medias de X

medias <- colMeans(select(data.part,nwifeinc,educ,exper,
                          expersq, age, kidslt6, kidsge6)[,])

#Ponerlo en dataframe de una observación
medias <- data.frame(rbind(medias))

medias <- medias %>% 
  mutate(index=predict(mtobit, newdata=medias),
         Phi = pnorm(index/mtobit$scale),
         efecto_educ = Phi*mtobit$coef[3])
         



#Heckit----

#Cargo los datos porque ya le añadimos cosas antes
data.part <- read.dta("./data/mroz.dta") 

mheckit <- heckit(inlf ~ age + I( age^2 ) + kidslt6 + huswage + educ,
                  log(wage) ~ educ + exper + I( exper^2 ) + city,
                  data=data.part)

summary(mheckit)





stargazer::stargazer(mheckit, mlineal,
                     type = "text")


sjPlot::tab_model(mheckit, mlineal,
                  collapse.se = TRUE,
                  transform = F,
                  show.ci = F,
                  show.r2  =F,
                  wrap.labels = 35,
                  p.style = "stars",
                  p.threshold = c(0.1, 0.05, 0.01))




#Heckit----


data.gastos <- read.dta("./data/limdep_ambexp.dta")

data.gastos <- data.gastos %>% 
  mutate(part=ifelse(ambexp>0,1,0))



mheckit <- heckit(selection = part ~ educ + age + female + blhisp + fairpoor + totchr,
                  outcome = lambexp ~ educ + age + female + I(age^2) + I(educ^2) + age*female,
                  method = "ml",
                  data=data.gastos) 
summary(mheckit)



#Hagámoslo a mano

primera <- glm(part ~ educ + age + female + blhisp + fairpoor + totchr,
               family = binomial(link = "probit"),
               data = data.gastos)

#Construyo el IMR
data.gastos <- data.gastos %>% 
  mutate(index = predict(primera, .)) %>%
  mutate(imr = dnorm(index)/pnorm(index))

#La segunda etapa solo usa a las mujeres que trabajan

segunda <- lm(lambexp ~ educ + age + female + imr + I(age^2) + I(educ^2) + age*female,
              data=filter(data.gastos, part==1))


stargazer::stargazer(mheckit, primera, segunda,
                     type = "text")
