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
library(tidyverse)

#Objetos básicos----

#Constantes y operaciones básicas
a <- 1
b <- 2

#Sumas
c <- a+b
d <- a-b

#Imprimir un elemento
c
d




#Ejercicio 1----
#Un rectánculo tiene una base igual a 10 unidades una altura igual
#a 4 unidades. Genere una constante llamada area.rect que indique
#el área del rectánculo



#¿Qué pasa si el rectángulo ahora tiene una altura de 5? Genere la
#nueva área area.rect.2



#Un círculo tiene radio de 2.6. Genere una constante a.circ que 
#indique el área del círculo y otra p.circ que indique el perímetro






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




#Funciones básicas----

#Calculemos la media de las entradas en x
mean(x)

#Pedimos ayuda sobre la función
?mean

#Creamos un vector también llamado x y luego definimos y como un 
#nuevo vector igual al cuadrado de x
x <- c(-4,-3,-2,-1,0,1,2,3,4)
potencia <- 2
y <- x^potencia

#Usamos la función qplot (que es parte del paquete ggplot2) para 
#hacer una gráfica
qplot(x, y)

#Tal vez queremos cambiar la potencia
potencia <- 3

# Redefinimos y
y <- x^potencia

# Volvemos a graficar los puntos
qplot(x, y)

# Queremos ver detalles sobre qplot
?qplot





#Ejercicio 2----
#Genere un nuevo vector, w, que vaya del 1 al 10
w <- 1:10

#La función log() devuelve el logaritmo natural del argumento
#Genere un nuevo vector lw que sea igual al logaritmo natural de w

#Grafique lw en función de w

#Genere sw igual a la raíz cuadrada de w y grafique sw en función
#de w






#Leer archivos de datos----

#Fijamos un directorio
setwd("C:/Users/rojas/Dropbox/presentations_sites/ecn_ciestaam2022/")

#El punto quiere decir "en donde apunta el directorio de trabajo"
mundata <- read_csv(file = "./data/mundata.csv")

#Una forma de leer los caracteres en español
mundata <- read_csv(file = "./data/mundata.csv",
                    locale = locale(encoding = "latin1"))

#Esta base de datos o data framecontiene la población de los
#municipios de  #México y el número de camas y unidades de cuidado
#intensivo ICU de acuerdo a los últimos registros oficiales
#Ver Rojas (2021)




#Ejercicio 3----
colnames(mundata)

head(mundata)

head(mundata, n=10)

tail(mundata)

dim(mundata)

filas <- nrow(mundata)

ncol(mundata)

str(mundata)











#dplyr para manipulación de datos----
#install.packages('tidyverse')
library(tidyverse)


#filter para seleccionar filas
filter(mundata, clave_ent==2)

#Creamos nuevos objetos usando <- (equivalente a =)
mun.bc <- filter(mundata, clave_ent==2)

mun.peninsula.baja <- filter(mundata, clave_ent==2 | clave_ent==3)

#Seleccionar los estados de la frontera norte

#Creamos un vector con los nombres
estados.frontera <- c("Baja California",
                      "Sonora",
                      "Chihuahua",
                      "Coahuila",
                      "Nuevo León",
                      "Tamaulipas")

#El operador %in% indica pertenencia a un conjunto
mun.frontera <- filter(mundata, nom_ent %in% estados.frontera)







#Ejercicio 4----
#Crea un nuevo objeto que tenga a los municipios del Estado de México
#(llamado "México" en la base de datos) y que además tenga más de
#10,000 habitantes
mun.edo.mex <- filter(#########)

#Crea otro objeto que tenga a los municipios en los estados de la frontera
#y que tengan cada uno más de 100 camas de hospital (beds)
mun.frontera.camas <- filter(#########)

#Crea un nuevo objeto que tenga a todos los municipios de México que tengan
#entre 10 y 20 UCI
mun.uci <- filter(#########)





#Más de dplyr----

#arrange para ordenar los datos de acuerdo a una variable
arrange(mun.bc, beds)

mun.bc <- arrange(mun.bc, beds)

#La opción desc da el orden descendente
mun.bc <- arrange(mun.bc, desc(beds))





#select permite seleccionar columnas
pob.mun <- select(mundata, clave, pop)




#rename nos permite renombrar columnas
mundata <- rename(mundata, nom_mun=mun)



#mutate sirve para crear nuevas columnas
mundata <- mutate(mundata, recursos=beds+icu)




#if_else nos permite evaluar condicionales
#(lo veremos de nuevo en funciones)
mundata <- mutate(mundata,
                  frontera=ifelse(nom_ent %in% estados.frontera, 1, 0))





#Ejercicio 5----

#Genere un objeto que contenga los municipios del centro del país
#(Ciudad de México, Estado de México, Puebla, Tlaxcala y Morelos)

estados.centro <- c("Ciudad de México",
                    "México",
                    "Puebla",
                    "Tlaxcala",
                    "Morelos")

mun.centro <- filter(######)

#En el nuevo objeto, genere una nueva variable que indique el logaritmo
#natural del número de camas de hospital por cada 100,000 habitantes en
#dichos municipios

mun.centro <- mutate(mun.centro,
                     #########)





#Más de dplyr----

#summarise para obtener estadísticas básicas
summarise(mundata, media.pop=mean(pop))

summarise(mundata, sd.pop=sd(pop))

#group_by para implementar funciones por subgrupos
por_estado <- group_by(mundata,clave_ent)
summarise(por_estado, media.pop=mean(pop))

#Creamos un nuevo objeto con la media por estado
medias.estado <- summarise(por_estado, media.pop=mean(pop))





##Ejercicio 6----

#Hemos hecho varias cosas, así que limpiemos y empecemos de nuevo

mundata <- read_csv(file = "./data/mundata.csv",
                    locale = locale(encoding = "latin1"))

mundata <- select(#########)

#Generemos una nueva variable llamada región que tome los valores de
#acuerdo a la definición de regiones siguiente:
#1 para la región
#2 para la región centro
#3 para la región centro-occidente
#4 para la región sureste

estados.norte <- c("Baja California",
                   "Baja California Sur", 
                   "Chihuahua",
                   "Coahuila",
                   "Durango",
                   "Nuevo León",
                   "Sinaloa",
                   "Sonora",
                   "Tamaulipas")

estados.centro <- c("Ciudad de México",
                    "Guerrero",
                    "Hidalgo",
                    "México",
                    "Morelos",
                    "Puebla",
                    "Tlaxcala",
                    "Oaxaca")

estados.centroocc <- c("Aguascalientes",
                       "Colima",
                       "Guanajuato",
                       "Jalisco",
                       "Michoacán",
                       "Nayarit",
                       "Querétaro",
                       "San Luis Potosí",
                       "Zacatecas")

estados.sureste <- c("Campeche",
                     "Chiapas",
                     "Quintana Roo",
                     "Tabasco",
                     "Veracruz",
                     "Yucatán")

#Creamos una variable categórica para las cuatro regiones
#Creamos una variable del 1 al 4 para las regiones
#Usamos la función case_when

mundata <- mutate(#########)


#Construya un objeto que reporte el promedio del número de 
#camas por cada 100,000 habitantes de los municipios, por región

mundata <- mutate(#########)
por_region <- group_by(#########)

camas.region <- summarise(#########)








#Convertimos region a factor
mundata <- mutate(mundata,
                  region=factor(region,
                                levels = c(1,2,3,4),
                                labels = c("Región Norte",
                                           "Región Centro",
                                           "Región Centro-Occidente",
                                           "Region Sureste")))

levels(mundata$region)





# La pipa ----

#Haremos en un paso lo siguiente: nos quedaremos con un objeto de los
#municipios de la Región Centro, creamos una variable que sume las
#camas y las UCI, generamos una variable que indique que el municipio
#tiene recursos bajos si tiene menos de 100 recursos de salud y,
#finalmente, que nos de la proporción de municipios por estado que
#se consideran de recursos bajos

mundata %>% 
  filter(region=="Región Centro") %>%
  mutate(recursos_totales=beds+icu) %>% 
  mutate(recursos_bajos=ifelse(recursos_totales<100,1,0)) %>%
  group_by(nom_ent) %>% 
  summarize(prop.bajos=mean(recursos_bajos)) %>% 
  ungroup()


#Trabajar con NA
mundata %>% 
  filter(region=="Región Centro") %>%
  mutate(recursos_totales=beds+icu) %>% 
  mutate(recursos_bajos=ifelse(recursos_totales<100,1,0)) %>%
  group_by(nom_ent) %>% 
  summarize(prop.bajos=mean(recursos_bajos, na.rm=TRUE)) %>% 
  ungroup()










#Si queremos una objeto que indique el promedio de camas por
#estado
aggregate(mundata$beds,
          by=list(Estado=mundata$nom_ent),
          mean)

mean.camas.mun <- aggregate(mundata$beds,
                            by=list(Estado=mundata$nom_ent),
                            mean)

#Arreglamos con na.rm
aggregate(mundata$beds,
          by=list(Estado=mundata$nom_ent),
          mean,
          na.rm=T)

mean.camas.mun <- aggregate(mundata$beds,
                            by=list(Estado=mundata$nom_ent),
                            mean,
                            na.rm=T)

#Tenemos que arreglar los nombres
mean.camas.mun <- set_names(mean.camas.mun, c("Estado", "media.camas"))




#Hacemos un subgrupo del data frame, tomando estados de la península de Baja California
mundata.baja <- subset(mundata,
                       nom_ent %in% c("Baja California","Baja California Sur"),
                       select=c(nom_ent,mun, pop, beds, icu))

#Y luego podemos calcular la media entre municipios de cada uno
#de los dos estados
mean.camas.baja <- aggregate(mundata.baja$beds,
                             by=list(Estado=mundata.baja$nom_ent),
                             mean,
                             na.rm=T)

#Y tendríamos que arreglar los nombres
#Todo esto es más fácil con las pipas





#Lo mismo pero con pipas y group_by----

mundata <- read_csv(file = "./data/mundata.csv",
                    locale = locale(encoding = "latin1"))

subset(mundata,
       nom_ent %in% c("Baja California","Baja California Sur"),
       select=c(nom_ent,mun, pop, beds, icu)) %>% 
  group_by(nom_ent) %>% 
  summarise(media.camas=mean(beds)) %>% 
  ungroup()



#Podemos calcular varias estadísticas

tabla.desc <- subset(mundata,
                     nom_ent %in% c("Baja California","Baja California Sur"),
                     select=c(nom_ent,mun, pop, beds, icu)) %>% 
  group_by(nom_ent) %>% 
  summarise(media.camas=mean(beds),
            de.camas=sd(beds),
            obs.camas=n(),
            min.camas=min(beds),
            max.camas=max(beds)) %>% 
  ungroup()




#Estadística a distintos niveles---
##A nivel SUN
sundata <- mundata %>% 
  filter(!is.na(cve_sun)) %>% 
  group_by(cve_sun, nom_sun) %>% 
  summarise(pop=sum(pop,na.rm=TRUE),
            beds=sum(beds,na.rm=TRUE),
            icu=sum(icu,na.rm=TRUE)) %>% 
  ungroup()


##A nivel estado
statedata <- mundata %>% 
  group_by(clave_ent, nom_ent) %>% 
  summarise(pop=sum(pop,na.rm=TRUE),
            beds=sum(beds,na.rm=TRUE),
            icu=sum(icu,na.rm=TRUE)) %>% 
  arrange(clave_ent) %>%
  ungroup() 


#Total nacional
statedata %>% 
  summarise(pop=sum(pop,na.rm=TRUE),
            beds=sum(beds,na.rm=TRUE),
            icu=sum(icu,na.rm=TRUE)) 





#Ejercicio 7----
#Genere un nuevo data frame que incluya el total de población,
#camas y UCI de cada estado y su participación porcentual respecto
#a los totales nacionales



#Fin


