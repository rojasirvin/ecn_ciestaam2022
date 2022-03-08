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





#Consecuencias de ignorar censura y truncamiento

#Construimos un vector de -2 a 2 y calculamos la densidad
#la cdf, el IMR y lo ponemos en un data frame
c <- seq(from=-2, to=2, by=0.1)
densidad <- dnorm(c)
cdf <- pnorm(c)
imr <- densidad /(1-cdf)
data <- data.frame(cbind(c,densidad,cdf,imr))

#Ponemos los puntos en una sola columna
data <- data %>% 
  pivot_longer(cols = c(cdf,densidad,imr),
               names_to = "type",
               values_to = "value") 

data %>% 
  ggplot(aes(x=c, y=value, color=type)) +
  geom_line()+
  labs(x = "Corte c", y="CDF, densidad e IRM")

#lambda(z) es casi lineal en c para valores de c>0






#Experimento ----

set.seed(109)
e <- rnorm(200, mean = 0, sd = 1000)
lnw <- rnorm(200, mean = 2.75, sd = 0.6)

data <- data.frame(cbind(e,lnw))

data <- data %>% 
  mutate(ystar = -2500 + 1000*lnw + e,
         ytrunc = ystar,
         ytrunc = ifelse(ystar<0,NA,ystar),
         ycens = ystar,
         ycens = ifelse(ystar<0,0,ystar),
         dy = ycens,
         dy = ifelse(ycens>0,1,dy))

data %>% 
  ggplot(aes(x=lnw, y=ystar, color=as.factor(dy)))+
  geom_point()


#Calculemos ahora las medias truncadas y censuradas, que son claramente no lineales
data <- data %>%
  mutate(xb = -2500 + 1000*lnw,
         sigma = 1000,
          Phi = pnorm(xb/sigma),
          phi = dnorm(xb/sigma),
          lambda = phi / Phi,
          media_trunc = xb+sigma*lambda,
          media_cens = Phi*xb+sigma*phi)

data <- data %>% 
  select(ystar,media_trunc, media_cens,xb, lnw, dy) %>%
  pivot_longer(cols=c(ystar,media_trunc, media_cens,xb),
               names_to="estadistica",
               values_to = "valor")

data %>% 
  filter(estadistica=="ystar") %>% 
  ggplot(aes(x=lnw,y=valor))+
    geom_point()+
    geom_line(data=filter(data,estadistica!="ystar"),
              aes(x=lnw,y=valor, color=estadistica))
    