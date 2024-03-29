# Espacio de trabajo ----
rm(list = ls())
options(scipen=999) # Prevenir notación científica

library(tidyverse)
library(Synth)


#Tenemos un panel en formato long
panel.ca <- read_csv("data/california_panel.csv") 

panel.ca <- data.frame(panel.ca)

#Tenemos que preparar los datos
dataprep.out <-
  dataprep(panel.ca,
           predictors= c("lnincome", "beer", "age15to24","retprice"),
           predictors.op = c("mean"),
           dependent = c("cigsale"),
           unit.variable = c("state_id"),
           time.variable = c("year"),
           special.predictors = list(
             list("cigsale",1975,c("mean")),
             list("cigsale",1980,c("mean")),
             list("cigsale",1988,c("mean"))),
           treatment.identifier = 3,
           controls.identifier = c(1:2,4:39),
           time.predictors.prior = c(1980:1988),
           time.optimize.ssr = c(1970:1988),
           unit.names.variable = c("state"),
           time.plot = c(1970:2000))


#La función synth estima el control sintético
synth.out <- synth(data.prep.obj = dataprep.out)

#Con synth.tab construimos tablas
synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out
) 

#Veamos los resultados
print(synth.tables)




#Gráficas de resultados principales

#Verdadero vs sintético
path.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          tr.intake = 1989,
          Ylab = c("per-capita cigarette sales (in packs)"),
          Xlab = c("year"), 
          Ylim = c(0,140), 
          Legend = c("California","synthetic California")) 

#Brechas
gaps.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          tr.intake = 1989,
          Ylab = c("per-capita cigarette sales (in packs)"),
          Xlab = c("year"), 
          Ylim = c(-30,30))



##Podemos hacer la gráfica "manual"

#Unidad sintética
Ys <- dataprep.out$Y0plot %*% synth.out$solution.w
# %*% es la multiplicación de matrices                 


#Unidad verdadera
Y1 <- dataprep.out$Y1plot

#Creamos una basesita
data.plot <- as.data.frame(cbind(Y1,Ys))
colnames(data.plot) <- c("Y1","Ys")
data.plot <- data.plot %>% 
  mutate(year=seq(from=1970, to=2000))


#Además de que nos gusta aprender a hacer las cosas "a mano"...
data.plot %>% 
  ggplot()+
  geom_line(aes(y=Y1,x=year)) +
  geom_line(aes(y=Ys,x=year), linetype = "dashed")+
  ylab("per-capita cigarette sales (in packs)")+
  xlab("year")+
  geom_vline(xintercept=1988, color = "black", size=1, linetype="dashed")+
  scale_y_continuous(breaks = seq(0,140,20))+
  scale_x_continuous(breaks=seq(1970, 2000, 5))


#La gráfica de la brecha resulta de hacer la diferencia
data.plot <- data.plot %>% 
  mutate(gap=Y1-Ys)


#En este caso nos sirve aprender a hacer una gráfica para hacer los placebos
#Además de que nos gusta aprender a hacer las cosas "a mano"...
data.plot %>% 
  ggplot()+
  geom_line(aes(y=gap,x=year)) +
  ylab("per-capita cigarette sales (in packs)")+
  xlab("year")+
  geom_vline(xintercept=1988, color = "black", size=1, linetype="dashed")+
  geom_hline(yintercept=0, color = "black", size=1, linetype="dashed")+
  scale_y_continuous(breaks = seq(-30,30,10))+
  scale_x_continuous(breaks=seq(1970, 2000, 5))


#Estudios placebo
#Queremos repetir lo que hicimos antes con California para cada estado e ir guardando la brecha
#Después, queremos graficar todas las brechas y ver en qué posición está la de California

#Yo decidí resolverlo así, pero quizás tengo un cerebro aún medio matlabero

#Tenemos 38 estados y 31 periodos a guardar
placebos <- data.frame(matrix(ncol = 39, nrow = 31))

#Hacemos un ciclo para operacionalizar el trabajo
for (j in 1:39) {
  
  tunit <- j
  tunit_m1 <- j-1
  tunit_p1 <- j+1
  
  if (j==1)
    dpool <- c(2:39)
  else
    if (j==39)
      dpool <- c(1:38)
  else
    dpool <- c(1:tunit_m1,tunit_p1:39)
  
  
  dataprep.out <-
    dataprep(panel.ca,
             predictors= c("lnincome", "beer", "age15to24","retprice"),
             predictors.op = c("mean"),
             dependent = c("cigsale"),
             unit.variable = c("state_id"),
             time.variable = c("year"),
             special.predictors = list(
               list("cigsale",1975,c("mean")),
               list("cigsale",1980,c("mean")),
               list("cigsale",1988,c("mean"))),
             treatment.identifier = tunit,
             controls.identifier = dpool,
             time.predictors.prior = c(1980:1988),
             time.optimize.ssr = c(1970:1988),
             unit.names.variable = c("state"),
             time.plot = c(1970:2000))
  
  #Repetimos tareas
  synth.out <- synth(data.prep.obj = dataprep.out)
  Ys <- dataprep.out$Y0plot %*% synth.out$solution.w
  Y1 <- dataprep.out$Y1plot
  
  data.plot <- as.data.frame(cbind(Y1,Ys))
  colnames(data.plot) <- c("Y1","Ys")
  data.plot <- data.plot %>% 
    mutate(gap=Y1-Ys)
  
  #Guardamos donde corresponde
  placebos[,j] <- data.plot$gap
  
  #Para ir viendo en dónde vamos
  print(j)
  
}


##Ahora tenemos un panel de estados placebo
placebos <- placebos %>% 
  mutate(year=seq(from=1970, to=2000))

#Pasamos todo a forrmato long
placebos <- placebos %>%
  pivot_longer(cols=starts_with("X"),
               names_to = "state",
               values_to = "cons_synth") %>% 
  mutate(state=substr(state,2,3)) %>% 
  mutate(treated=ifelse(state=="3",1,0)) %>% 
  mutate(treated=factor(treated, levels=c("0","1"), labels=c("control states", "California")))


#Replicamos la figura 4
placebos %>%
  filter(cons_synth>-30 & cons_synth<30) %>% 
  ggplot(aes(x=year, y=cons_synth, group=state, linetype=treated, color=treated))+
  geom_line()+
  scale_linetype_manual(values=c("solid", "solid"))+
  scale_color_manual(values=c("grey","black")) +
  ylab("per-capita cigarette sales (in packs)")+
  xlab("year")+
  geom_vline(xintercept=1988, color = "black", size=.5, linetype="dashed")+
  geom_hline(yintercept=0, color = "black", size=.5, linetype="dashed")+
  scale_y_continuous(breaks = seq(-30,30,10))+
  scale_x_continuous(breaks=seq(1970, 2000, 5))+
  theme(legend.title = element_blank(),
        legend.position = c(.14, .90),
        legend.box.background = element_rect(color="black", size=.3),
        legend.box.margin = margin(1, 1, 1, 1))

