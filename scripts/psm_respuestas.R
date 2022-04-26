# Espacio de trabajo ----
rm(list = ls())
options(scipen=999) # Prevenir notación científica

library(tidyverse)
library(estimatr)
#install.packages('Zelig') #permite hacer la simulación del impacto
library(Zelig)
#install.packages('MatchIt')
library(MatchIt)
#install.packages('cobalt') #nos deja hacer los love plots
library(cobalt)
#install.packages('summarytools')
library(summarytools) # hacer estadística por grupos

#Datos

data.jcf <- read.csv("data/datos_jcf_analisis.csv") 



#Estadística descriptiva por grupos
datasummary_balance(~jcf2,
                    fmt = "%.2f",
                    data = select(data.jcf,jcf2,ingtot_tri,tot_integ, mujer, indigena, rural, escoacum, 
                                  casadounion, jefehog, haymenores, proggob),
                    dinm_statistic = "p.value",
                    title = "Diferencias entre grupos",
                    notes = "Fuente: Datos de la ENIGH2020 del INEGI")


#Preparamos datos (esto se necesita porque matchit no acepta NAs)
sub.data <- data.jcf %>% 
  dplyr::select(ingtot_tri, jcf2, mujer, indigena, cve_ent, rural, escoacum, 
                casadounion, jefehog, haymenores, proggob, tot_integ, factor.x)

sub.data <- sub.data[complete.cases(sub.data), ] 



#Usamos matchit para hacer los emparejamientos

m.out.a <- matchit(formula=jcf2 ~ mujer + indigena + factor(cve_ent) + rural  + escoacum + 
                     casadounion + jefehog + haymenores + proggob + tot_integ,
                   method = "nearest",
                   distance= "glm",
                   replace = FALSE,
                   data = sub.data)



#Resultados del emparejamiento
m.out.a[["X"]][["factor(cve_ent)"]] <- NULL

summary(m.out.a, standardize=T)




#El love plot usando la librería cobalt
love.plot(bal.tab(m.out.a),
          threshold = .1)





#Usamos zelig para la simulación de efectos de tratamiento

set.seed(1711)

z.out.a <- zelig(formula = ingtot_tri ~ jcf2,
                 data=match.data(m.out.a),
                 model="ls")

#Simulo las dos situaciones
x.out.a <- setx(z.out.a, jcf2=0)
x1.out.a <- setx1(z.out.a, jcf2=1)

#Corremos la simulación
sim.out.a <- sim(z.out.a, x=x.out.a, x1=x1.out.a)

#Vemos los resultados
summary(sim.out.a)



#Tarea

#Estime ahora el TOT del programa en la probabilidad de encontrar empleo, **encontro**, 

#Para esto, se comparan a los jóvenes que recibieron el programa con todos los demás jóvenes (esta variable es **jcf**)

#Los beneficiarios tienen *jcf==1*, mientras que el resto de los jóvenes tienen *jcf==0*, pero debera usar solo a las personas que tienen *transicion == 1*, es decir, que dejaron su trabajo

#Realice la estimación del TOT de manera análoga a lo realizado en clase, pero ahora use
# - Dos vecinos, incluyendo *ratio = 1* en el proceso *matchit*
# - Un caliper de 0.05, incluyendo *caliper = 0.05* en el proceso de *matchit*

sub.data <- data.jcf %>%
  filter(transicion == 1) %>%
  dplyr::select(encontro, jcf, mujer, indigena, cve_ent, rural, escoacum, casadounion, jefehog,
                haymenores, proggob, tot_integ)

sub.data <- sub.data[complete.cases(sub.data), ]

m.out.b <- matchit(formula = jcf ~ mujer + indigena + factor(cve_ent) + rural + escoacum + casadounion +
                     jefehog + haymenores + proggob + tot_integ,
                   method = "nearest",
                   ratio = 1,
                   distance = "glm",
                   replace = FALSE,
                   data = sub.data)

z.out.b <- zelig(formula = encontro ~ jcf,
                 data = match.data(m.out.b),
                 model = "ls")

# Simularemos el valor esperado de las diferencias cuando t==0
x.out.b <- setx(z.out.b, jcf = 0)

# Con respecto a cuando t==1
x1.out.b <- setx1(z.out.b, jcf = 1)

# Corremos la simulación
sim.out.b <- sim(z.out.b, x = x.out.b, x1 = x1.out.b)

# Vemos los resultados
summary(sim.out.b)
