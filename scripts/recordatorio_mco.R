#Recordatorio de MCO

#Iniciamos una nueva sesión quitando todos los elementos previos
rm(list = ls()) 
#Para evitar la notación científica
options(scipen=999) 

library(tidyverse)
library(broom)


#Simulación de datos de educación e ingreso

#Fijo una semilla
set.seed(1234)

#Genero el vector de educación y una perturbación normal
educacion <-  rnorm(100,10,3)
e <-  rnorm(100,0,8)

#Genero el salario con parámetros conocidos (b0=100, b1=2)
b0 <- 150
b1 <- 2

salario <- b0 + b1 * educacion + e 
datos.salarios <- as.data.frame(cbind(salario, educacion))
  
(lm(salario ~ educacion))




#Gráfico de la relación educación e ingreso

datos.salarios %>% 
  ggplot(aes(x=educacion, y=salario )) +
  geom_point()+
  labs(x="Educación", "Salario por hora")




#Estimación por MCO

reg1 <- lm(salario ~ educacion,
           data=datos.salarios)

summary(reg1)



#Gráfico con línea de regresión
datos.salarios %>% 
  ggplot(aes(x=educacion, y=salario )) +
  geom_point()+
  labs(x="Educación", "Salario por hora")+
  geom_smooth(method = 'lm', se = F)




#Visualización de residuales

datos.salarios <- augment(reg1)

datos.salarios %>% 
  ggplot(aes(x=educacion, y=salario )) +
  geom_point()+
  labs(x="Educación", "Salario por hora")+
  geom_smooth(method = 'lm', se = F)+
  geom_segment(aes(xend = educacion, yend = .fitted), color = "red", size = 0.3)





#Simulación de las propiedades del estimador de MCO


#Cómo lo haríamos una vez

#Genero el vector de educación y una perturbación normal
educacion <-  rnorm(100,10,3)
e <-  rnorm(100,0,8)

#Genero el salario con parámetros conocidos (b0=100, b1=2)
b0 <- 150
b1 <- 2

salario <- b0 + b1 * educacion + e 
datos.salarios <- as.data.frame(cbind(salario, educacion))

#Veo un coeficiente
(lm(salario ~ educacion))$coef[2]







reps <- 10000
betas <- numeric(reps)

#Repetiremos reps veces la misma cosa

for (j in 1:reps){
#Obtenemos una relización de e
  e <-  rnorm(100,0,8)
  
  salario <- b0 + b1 * educacion + e 
  datos.salarios <- as.data.frame(cbind(salario, educacion))
  
  #Estimo la regresón
  
  betas[j] <- (lm(salario ~ educacion))$coef[2]
  
}


#Veamos los primeros 10 resultados
betas[1:10]

#Media
mean(betas)

#varianza
var(betas)


#Noten que la varianza que nos dice la distribución del estimador es
8^2 /  sum((educacion-mean(educacion))^2)


#Pogamos todo en un data frame

betas <- data.frame(betas)

betas %>% 
  ggplot(aes(x=betas)) + 
  geom_histogram() +
  geom_vline(xintercept=2, linetype='dashed')



