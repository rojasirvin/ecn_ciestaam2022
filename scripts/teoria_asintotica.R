#Recordatorio de MCO

#Iniciamos una nueva sesión quitando todos los elementos previos
rm(list = ls()) 
#Para evitar la notación científica
options(scipen=999) 

library(tidyverse)
library(ggpubr) #amplía las funciones de ggplot


#Primera simulación de 100 números aleatorios
# Semilla para poder generar la misma secuencia 
set.seed(820)

# Obtenemos una muestra de tamaño 100 con media 10 y desviación estándar 2
sample <- rnorm(100,10,2)

# Veamos 10 de las observaciones
head(sample,10)

#¿Cuál es la media muestral?
mean(sample)

#Genero otra muestra
sample <- rnorm(100,10,2)
mean(sample)



#Hagamos la simulación 1,000 veces
set.seed(820)

# Un vector para guardar las medias calculadas. Haremos 1,000 cálculos
reps <- 1000
ymedias <- numeric(reps)

# En cada una de las 1000 repeticiones, obtendremos una muestra de tamaño 100

for (i in 1:reps){
  sample<- rnorm(100,10,2) 
  ymedias[i]<-mean(sample)
}

#Veamos la media y varianza del vector que tiene 1,000 medias
mean(ymedias)
var(ymedias)

#Gráfico
ymedias <- data.frame(ymedias)

ymedias %>% 
  ggplot(aes(x=ymedias))+
  geom_histogram(aes(y=..density..))+
  stat_function(fun = dnorm, args = list(mean = 10, sd = sqrt(0.04)))

  



#TLC en acción----
#Simulemos cuatro tamaños de muestra diferentes
set.seed(820)
reps <- 1000

ymedias10 <- numeric(reps)
for (i in 1:reps){
  sample<- rnorm(10,10,2) 
  ymedias10[i]<-mean(sample)
}

ymedias50 <- numeric(reps)
for (i in 1:reps){
  sample<- rnorm(50,10,2) 
  ymedias50[i]<-mean(sample)
}

ymedias100 <- numeric(reps)
for (i in 1:reps){
  sample<- rnorm(100,10,2) 
  ymedias100[i]<-mean(sample)
}

ymedias1000 <- numeric(reps)
for (i in 1:reps){
  sample<- rnorm(1000,10,2) 
  ymedias1000[i]<-mean(sample)
}

ymedias_n <- data.frame(ymedias10, ymedias50, ymedias100, ymedias1000)


#Graficamos cada histograma con su curva teórica
g10 <- ymedias_n %>% 
  ggplot(aes(x=ymedias10)) + 
  geom_histogram(aes(y=..density..))+
  stat_function(fun = dnorm, args = list(mean = 10, sd = sqrt(4/10))) +
  xlim(8,12)
  
  

g50 <- ymedias_n %>% 
  ggplot(aes(x=ymedias50)) + 
  geom_histogram(aes(y=..density..))+
  stat_function(fun = dnorm, args = list(mean = 10, sd = sqrt(4/50)))+
  xlim(8,12)


g100 <- ymedias_n %>% 
  ggplot(aes(x=ymedias100)) + 
  geom_histogram(aes(y=..density..))+
  stat_function(fun = dnorm, args = list(mean = 10, sd = sqrt(4/100)))+
  xlim(8,12)


g1000 <- ymedias_n %>% 
  ggplot(aes(x=ymedias1000)) + 
  geom_histogram(aes(y=..density..))+
  stat_function(fun = dnorm, args = list(mean = 10, sd = sqrt(4/1000)))+
  xlim(8,12)

#Los cuatro objetos
ggarrange(g10, g50, g100, g1000,
          labels = c("N=10", "N=50", "N=100", "N=1000"),
          ncol = 2, nrow = 2)



#Ahora usando una distribución chi-cuadrada con 1 gdl----
set.seed(820)
sample <- rchisq(100,1)

#Así luce una distribución teórica
curve(dchisq(x,1), 0,3)

set.seed(820)
reps <- 1000

ymedias5 <- numeric(reps)
for (i in 1:reps){
  sample<-rchisq(5,1)
  ymedias5[i]<-mean(sample)
}

ymedias10 <- numeric(reps)
for (i in 1:reps){
  sample<- rchisq(10,1)
  ymedias10[i]<-mean(sample)
}

ymedias100 <- numeric(reps)
for (i in 1:reps){
  sample<- rchisq(100,1)
  ymedias100[i]<-mean(sample)
}

ymedias1000 <- numeric(reps)
for (i in 1:reps){
  sample<- rchisq(1000,1)
  ymedias1000[i]<-mean(sample)
}

ymedias_c <- data.frame(ymedias5, ymedias10, ymedias100, ymedias1000)

h5 <- ymedias_c %>% 
  ggplot(aes(x=ymedias5)) + 
  geom_histogram(aes(y=..density..))+
  stat_function(fun = dnorm, args = list(mean = 1, sd = sqrt(2/5))) +
  xlim(0,2)

h10 <- ymedias_c %>% 
  ggplot(aes(x=ymedias10)) + 
  geom_histogram(aes(y=..density..))+
  stat_function(fun = dnorm, args = list(mean = 1, sd = sqrt(2/10)))+
  xlim(0,2)

h100 <- ymedias_c %>% 
  ggplot(aes(x=ymedias100)) + 
  geom_histogram(aes(y=..density..))+
  stat_function(fun = dnorm, args = list(mean = 1, sd = sqrt(2/100)))+
  xlim(0,2)


h1000 <- ymedias_c %>% 
  ggplot(aes(x=ymedias1000)) + 
  geom_histogram(aes(y=..density..))+
  stat_function(fun = dnorm, args = list(mean = 1, sd = sqrt(2/1000)))+
  xlim(0,2)

#Los cuatro objetos
ggarrange(h5, h10, h100, h1000,
          labels = c("N=5", "N=10", "N=100", "N=1000"),
          ncol = 2, nrow = 2)





#Propiedades asintóticas de MCO----
set.seed(1234)

educacion <-  rnorm(100000,10,3)
e <-  rnorm(100000,0,8)
b0 <- 150
b1 <- 2

salario <- b0 + b1 * educacion + e 
poblacion <- data.frame(salario, educacion)


#Tomamos muestras de tamaño N y estimamos por MCO
#Hacemos este rep veces para cada tamaño

reps <- 1000

#Inicializamos una matriz para guardar resultados
estimadores_mco <- matrix(ncol = 2, nrow = reps)

#Hacemos reps veces el procedimiento
for (i in 1:reps){
  muestra <- poblacion[sample(1:100000, size=100), ]
  estimadores_mco[i, ] <- lm(salario ~ educacion, data = muestra)$coefficients
}

estimadores_mco <- data.frame(estimadores_mco) 

estimadores_mco <- estimadores_mco%>% 
  rename(alpha=X1, beta=X2)


estimadores_mco %>% 
  ggplot(aes(x=alpha))+
  geom_density()+
  xlim(140,160)


estimadores_mco %>% 
  ggplot(aes(x=beta))+
  geom_density()+
  xlim(1,3)




#Modificar el programa para mostrar que alpha y beta tienen una masa que se concentra en su media cuando crece el tamaño de la muestra

