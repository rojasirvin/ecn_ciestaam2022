#Sesión 1
#Introducción a R para estudiantes de economía
#Irvin Rojas

#Iniciamos una nueva sesión quitando todos los elementos previos
rm(list = ls()) 
#Para evitar la notación científica
options(scipen=999) 

#Si no tienen instalados los paquetes, descomenten la siguiente línea
#install.packages("tidyverse")

#Con library cargamos un paquete, que debe estar ya instalado
#tidiverse es una colección de paquetes
#library(tidyverse)

#Objetos básicos----

#Constantes y operaciones básicas
a <- 1
b <- 2


#Sumas
c <- a+b
d <- a-b

e <- a+g

#Imprimir un elemento
c
d




#Ejercicio 1----
#Un rectánculo tiene una base igual a 10 unidades una altura igual
#a 4 unidades. Genere una constante llamada area.rect que indique
#el área del rectánculo

base <- 10
altura <- 4

area.rect <- base*altura


#¿Qué pasa si el rectángulo ahora tiene una altura de 5? Genere la
#nueva área area.rect.2

altura <- 5

area.rect.2 <- base*altura

#Un círculo tiene radio de 2.6. Genere una constante a.circ que 
#indique el área del círculo y otra p.circ que indique el perímetro

radio <- 2.6

a.circ <- radio^2*pi

p.circ <- radio*2*pi



#Vectores y matrices----

#Definimos un vector
x <- c(3,4,5)

#Un vector por un escalar
(xx <- x*2)

#Un vector con entradas alfanuméricas
y <- c("Pedro", "Andrea", "Ana")
y





#Definimos una matriz
(M <- matrix(data=c(4,-2,1,6,0,5),
             nrow=2,
             ncol=3))

#La misma matriz sin tener que hacer explícitos los nombres de 
#los argumentos
(N <- matrix(c(4,-2,1,6,0,5), 2, 3))

#Una matriz por un escalar
(N2 <- matrix(c(4,-2,1,6,0,5), 2, 3)*2)

