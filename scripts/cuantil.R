# Espacio de trabajo ----
rm(list = ls())
options(scipen=999) # Prevenir notación científica

library(tidyverse)
library(sandwich)
#install.packages('quantreg')
library(quantreg)

#Ejemplo con datos de Vietnam---

data.wb<-read_csv("data/vietnam_hogares.csv",
                        locale = locale(encoding = "latin1")) %>% 
  filter(!is.na(lhhex12m)) %>% 
  rename(lnmed=lhhex12m, lntotal=lhhexp1)


#Hacemos MCO
summary(r.mco <- lm(lnmed  ~ lntotal, data=data.wb))

#Generamos valores ajustados
data.wb <- data.wb %>% 
  mutate(pols=predict(r.mco,))


#Regresión cuantil
summary(r.q90 <- rq(lnmed  ~ lntotal, data=data.wb, tau=0.9))


#Grafico de ambos coeficientes estimados
data.wb %>% 
  ggplot(aes(x=lntotal,y=lnmed)) + 
  geom_point(alpha=0.3, size=.1) + 
  geom_abline(intercept=coef(r.q90)[1], slope=coef(r.q90)[2], color="black", linetype="dashed")+
  geom_abline(intercept=coef(r.mco)[1], slope=coef(r.mco)[2], color="red")


#Para nueve cuantiles
r.q1_9 <- rq(lnmed  ~ lntotal, data=data.wb, tau= 1:9/10)

#Ver los coeficientes
coef(r.q1_9)

#Plot
plot(r.q1_9)

#Podemos recuperar los coeficientes que nos interesan
#Por el contexto de los datos, estos son "Coeficientes de Engel"
plot(summary(r.q1_9), parm="lntotal")


plot(r.q1_9, parm = 2, mar = c(5.1, 4.1, 2.1, 2.1), main = "", xlab = "tau",
     ylab = "income coefficient", cex = 1, pch = 19)


#Curvas de Engel

data.wb %>%
  ggplot(aes(x=lntotal, y=lnmed)) +
  geom_point(alpha=0.3, size=.1) + 
  geom_smooth(method = 'lm',
              size = .5,
              color='red',
              se = F) +
  geom_quantile(quantiles = c(0.25, 0.5, 0.75),
                size= .5,
                color='blue')



#Con 9 cuantiles
q9 <- 1:9/10

data.wb %>%
  ggplot(aes(x=lntotal, y=lnmed)) +
  geom_point(alpha=0.3, size=.1) + 
  geom_smooth(method = 'lm',
              size = .5,
              color='red',
              se = F) +
  geom_quantile(quantiles = q9,
                size= .5,
                color='blue')+
  geom_quantile(quantiles = .5,
                size= .5,
                color='green')


#Simulación----
set.seed(111)                             

x <- seq(0,100,length.out = 100)        
sigma <- 0.1 + 0.05*x                     
b_0 <- 6                                
b_1 <- 0.1                              
e <- rnorm(100,mean = 0, sd = sigma)      
y <- b_0 + b_1*x + e         

data <- data.frame(x,y)

#Notemos que tenemos un proceso heterocedástico
data %>%  ggplot(aes(x,y)) +
  geom_point()


#Tracemos una regresión lineal
data %>%  ggplot(aes(x,y)) +
  geom_point()+
  geom_smooth(method="lm")

#El modelo lineal no es muy informativo para, por ejemplo,
#decir algo sobre y cuando x=80

#Funciona bien para valores pequeños de x

#Estimemos entonces una regresión cuantil con tau=.9

data %>%  ggplot(aes(x,y)) +
  geom_point() +
  geom_smooth(method="lm") +
  geom_quantile(quantiles = 0.9)


#Si quisieran hacer esto a mano
r_q9 <- rq(y ~ x,
         data=data,
         tau = 0.9)

summary(r_q9)

#Y el gráfico

data %>%  ggplot(aes(x,y)) +
  geom_point() +
  geom_smooth(method="lm") +
  geom_abline(intercept=coef(r_q9)[1], slope=coef(r_q9)[2])

#Con varios cuantiles

q1_9 <- rq(y ~ x,
            data=data,
            tau = q9)

coef(q1_9)

data %>%  ggplot(aes(x,y)) +
  geom_point() +
  geom_smooth(method="lm") +
  geom_quantile(quantiles = q9)


#Graficando tau
plot(summary(q1_9),
     parm="x")


