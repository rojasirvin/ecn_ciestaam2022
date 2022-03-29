rm(list = ls()) 
options(scipen=999) 

library(tidyverse)
library(foreign)
#install.packages('np')
#install.packages('FNN')
library(np)
library(FNN)


data.salarios<-read_csv("data/salarios.csv",
                        locale = locale(encoding = "latin1"))

#Histograma----
data.salarios %>% 
  ggplot(aes(x=lnhwage)) +
  geom_histogram(aes(y=..density..),
                 bins=20,
                 fill="#69b3a2",
                 color="#e9ecef",
                 alpha=0.9) +
  labs(title = "Histograma del log del salario por hora") +
  theme(plot.title = element_text(size=15))


#h óptimo de Silverman para kernel Epanechnikov----
delta <- 1.7188 # Ver CT
w.sd <- sd(data.salarios$lnhwage)
w.iqr.adj <- IQR(data.salarios$lnhwage)/1.349
w.N <- nrow(data.salarios)
constante <- 1.3643
ajuste <- min(w.sd,w.iqr.adj)
h <- constante*delta*ajuste*w.N^(-0.2) # ancho de banda
h


data.salarios %>% 
  ggplot(aes(x=lnhwage)) +
  geom_histogram(aes(y=..density..),
                 bins=20, fill="#69b3a2",
                 color="#e9ecef", alpha=0.9) +
  geom_density(kernel="epanechnikov",
               bw=h/2,
               linetype="solid")+
  theme(plot.title = element_text(size=15))





#Kernel con varios anchos de banda----
data.salarios %>% 
  ggplot(aes(x=lnhwage)) +
  geom_histogram(aes(y=..density..), bins=20, fill="#69b3a2", color="#e9ecef", alpha=0.9)+
  geom_density(aes(x=lnhwage, color='Óptimo'), kernel="epanechnikov", bw=h/2, adjust=1)+
  geom_density(aes(x=lnhwage, color='1/2 óptimo'), kernel="epanechnikov", bw=h/2, adjust=0.5)+
  geom_density(aes(x=lnhwage, color='2 óptimo'), kernel="epanechnikov", bw=h/2, adjust=2)+
  scale_color_manual("h",values = c('Óptimo' = 'black', '1/2 óptimo' = 'red', '2 óptimo'='blue'))+
  theme(legend.position = 'right')



#Regresión no paramétrica----
set.seed(911)
N <- 100
u <- rnorm(n=N, mean=0, sd=25)
x <- seq(1:N)
y <- 150 + 6.5*x -0.15*x^2+0.001*x^3+u

data.sim <- as.data.frame(cbind(x,y))


knn5 <- knn.reg(data.sim$x, y=data.sim$y, k=5)
knn25 <- knn.reg(data.sim$x, y=data.sim$y, k=25)

data.sim <- data.sim %>% 
  mutate(y5=knn5$pred) %>% 
  mutate(y25=knn25$pred)


data.sim %>% 
  ggplot(aes(x=x,y=y))+
  geom_point()+
  geom_smooth(aes(x=x,y=y, color='MCO'), method=lm, se=FALSE)+
  geom_line(aes(x=x,y=y5, color='kNN, k=5'))+
  geom_line(aes(x=x,y=y25, color='kNN, k=25'))+
  theme(legend.position = 'right')+
  scale_color_manual("Método", values = c('kNN, k=5' = 'blue', 'kNN, k=25'='green', 'MCO'='black'))
  


#Lowess----

fit.lowess <- lowess(data.sim$x, y=data.sim$y, f=25/100)

data.sim <- data.sim %>% 
  mutate(y.lowess=fit.lowess$y)

np2 <- data.sim %>% 
  ggplot(aes(x=x,y=y))+
  geom_point()+
  geom_smooth(aes(x=x,y=y, color='MCO cúbico'), method=lm, formula= y ~ x+I(x^2)+I(x^3), se=FALSE)+
  geom_line(aes(x=x,y=y.lowess, color='Lowess k=25'))+    theme(legend.position = 'right')+
  scale_color_manual("Método", values = c('MCO cúbico' = 'blue', 'Lowess k=25' = 'red'))




#Estimador de Robinson----
data(wage1)
colnames(wage1)

bw <- npplregbw(formula=lwage~educ+ tenure | exper, data=wage1, regtype="ll")

model.pl <- npplreg(bw)

par(mfcol=c(5,3),mai=c(0.5,0.5,0.5,0))
g.robinson <- npplot(bw,
                     plot.errors.method = "bootstrap",
                     plot.behavior="plot-data")


#Obtenemos los resultados de g.robinson

g <- fitted(g.robinson$plr3)
se <- g.robinson[["plr3"]][["merr"]]
lci <- g - se[,1]
uci <- g + se[,2]

#Este objeto nos dicen dónde fueron evaluados
exp.eval <- g.robinson[["plr3"]][["evalz"]][["V1"]]

fitted <- data.frame(exp.eval, g,lci,uci)


ggplot() + 
  geom_point(data=wage1, aes(exper,lwage), color='black', alpha=0.5) + 
  geom_line(data=fitted, aes(exp.eval, g), linetype='solid')+
  geom_line(data=fitted, aes(exp.eval, uci), linetype='dashed')+
  geom_line(data=fitted, aes(exp.eval, lci), linetype='dashed')