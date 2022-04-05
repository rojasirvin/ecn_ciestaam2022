# Espacio de trabajo ----
rm(list = ls())
options(scipen=999) # Prevenir notación científica

library(tidyverse)
library(sf)
library(leaflet) #Mapas temáticos (la usa el NYT)
library(geojsonio)

library(janitor)
library(sandwich)
library(pastecs)
library(nnet)
library(MASS)
library(AER)
library(survival)
#library(sampleSelection)
#library(COUNT)
#library(quantreg)
library(rgdal) # Leer objetos espaciales

library(spatialreg)
library(sf) #leer shapefiles
library(sp)
library(spdep) #análisis de regresión
library(tmap)

#Tenemos dos basesitas con el número de robos y el número de totems por colonia en la CDMX----

num_robo<-read_csv("data/num_robo.csv")

num_totems<-read_csv("data/num_totems.csv")

#Bordes por colonia
#st_read del paquete sf lee los datos
colonias <- st_read("data/coloniascdmx.shp")

colonias %>% ggplot() +
  geom_sf()

#Al shapefile de colonias le pegamos el conteo de robos
colonias <- colonias %>% 
  left_join(num_robo, by="cve_col") %>% 
  mutate(robos=ifelse(is.na(robos),0,robos)) #Para las colonias sin reporte alguno, ponemos 0

#Podemos ver las características
plot(colonias)
plot(colonias[8])

#Pero mejor usamos ggplot
colonias %>%
  ggplot() +
  geom_sf(aes(fill = robos))


#Paletas
colonias %>%
  ggplot() +
  geom_sf(aes(fill = robos), lwd = 0.1) +
  scale_fill_viridis_c(option = "plasma",
                       trans = "sqrt") 







#Mapas temáticos----
#El paquete leaflet nos deja hacer mejores mapas

#Un planisferio para empezar
leaflet() %>%
  addTiles()  


#Zoom a mi colonia
#Pueden ver long y lat en Google Maps por ejemplo
leaflet() %>%
  addTiles() %>%
  setView(lng=-99.1615, lat=19.430586, 
          zoom = 15)














#Pongámosle un fondo con Leaflet
leaflet(colonias) %>%
  addTiles() %>%
  addPolygons() %>% 
  setView(lng=-99.1615, lat=19.430586, 
          zoom = 15) 

#Ahora podemos colorear por el valor de robos
paleta <- colorBin("YlOrRd",
                     domain = colonias$robos,
                     bins=8)

leaflet(colonias) %>%
  addTiles() %>%
  addPolygons(fillColor = ~paleta(robos),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7) 


#Colorear el borde a la colonia seleccionada

leaflet(colonias) %>%
  addTiles() %>%
  addPolygons(fillColor = ~paleta(robos),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              highlightOptions = highlightOptions(
                weight = 3,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE))



#Agregamos etiqueta al cursor
etiqueta <- sprintf("<strong>%s</strong><br/>%g robos"
                      ,colonias$nombre, colonias$robos) %>%
  lapply(htmltools::HTML)

leaflet(colonias) %>%
  addTiles() %>%
  addPolygons(fillColor = ~paleta(robos),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              label = etiqueta,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))


#Y añadimos leyenda

leaflet(colonias) %>%
  addTiles() %>%
  addPolygons(fillColor = ~paleta(robos),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              label = etiqueta,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLegend(pal = paleta,
          values = ~robos,
          oppacity = 0.7,
          title = NULL,
          position = "bottomright")

  
  
  addLegend(pal = paleta,
            values = ~robos,
            opacity = 0.7,
            title = NULL,
            position = "bottomright") %>% 
  setView(lng=-99.1615, lat=19.430586, 
          zoom = 15)











leaflet(colonias) %>%
  addPolygons(weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~robos,
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))



colonias <- geojsonio::geojson_read("data/coloniascdmx.shp",
                                  what = "sp")
class(colonias)

%>%
  addProviderTiles("MapBox",options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))






#Para mapas temáticos usamos el paquete tmap----
tm_shape(colonias) +
  tm_fill("robos",
          title="Robos a transeúnte en 2019")+
  tm_borders(alpha = 0.1)+
  tmap_style("col_blind") +
  tmap_options(check.and.fix = TRUE) 




tmap_mode("view") # Con esto lo hacemos interactivo

#Regresamos al modo de plots
tmap_mode("plot")

#La opción style nos permite dividir los datos
c1 <-tm_shape(colonias) +
  tm_fill("robos",title="Robos a transeúnte en 2019",style="Equal")+
  tm_borders(alpha = 0.1)+
  tmap_style("col_blind")

c2 <-tm_shape(colonias) +
  tm_fill("robos",title="Robos a transeúnte en 2019",style="jenks")+
  tm_borders(alpha = 0.1)+
  tmap_style("col_blind")

c3 <-tm_shape(colonias) +
  tm_fill("robos",title="Robos a transeúnte en 2019",style="quantile")+
  tm_borders(alpha = 0.1)+
  tmap_style("col_blind")

c4 <-tm_shape(colonias) +
  tm_fill("robos",title="Robos a transeúnte en 2019",style="cont")+
  tm_borders(alpha = 0.1)+
  tmap_style("col_blind")

#Podemos ver cuál nos convence más
tmap_arrange(c2, c2, c3, c4)





#Mapas con símbolos----
tm_shape(colonias) + 
  tm_bubbles("robos")


tm_shape(colonias) +                        
  tm_bubbles("robos", border.lwd=NA) +      
  tm_borders(alpha=0.1) +                   
  tm_layout(legend.position = c("right", "bottom"),
            legend.title.size = 0.8,
            legend.text.size = 0.5)


#Quizás nos importa una Alcaldia
colonias_cuauhtemoc <- colonias %>% 
  filter(alcaldia=="CUAUHTEMOC")

tm_shape(colonias_cuauhtemoc) +                         
  tm_bubbles("robos", border.lwd=NA, col="blue") +              
  tm_borders(alpha=0.1) +                           
  tm_layout(legend.position = c("right", "bottom"), 
            legend.title.size = 0.8,
            legend.text.size = 0.5)


#Pongámosle un fondo con Leaflet
m <- leaflet(colonias_cuauhtemoc) %>%
  addTiles() %>%
  addPolygons() %>% 
  setView(lng=-99.1615, lat=19.430586, 
          zoom = 15) 
m


#Ahora podemos colorear por el valor de robos
pal <- colorBin("YlOrRd", domain = colonias_cuauhtemoc$robos)


m <- leaflet(colonias_cuauhtemoc) %>%
  addTiles() %>%
  addPolygons(fillColor = ~pal(robos),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7) %>%
  addLegend(pal = pal, values = ~robos, opacity = 0.7, title = NULL,
            position = "bottomright") %>% 
  setView(lng=-99.1615, lat=19.430586, 
          zoom = 15) 
m

#Acá una intro a leaflet https://rstudio.github.io/leaflet/


#Regresión espacial----
#Usemos la base de colonias y le pegamos nuestras bases de crimen y totems
colonias_shp@data <- colonias_shp@data %>% 
  left_join(num_robo, by="cve_col")
colonias_shp@data <- colonias_shp@data %>% 
  left_join(num_totems, by="cve_col")

#Para propósitos de exposición, si robos o totems son NA, ponemos 0
colonias_shp@data <- colonias_shp@data %>% 
  mutate(robos=ifelse(is.na(robos),0,robos)) %>%
  mutate(totems=ifelse(is.na(totems),0,totems))



#MCO
summary(m.mco<-lm(robos ~ totems, data=colonias_shp@data))


#Matrices de pesos
#La matriz de contiguidad a veces se le conoce como tipo "reina" por que así es como se mueve una reina en el ajedrez
#style indica que tipo de estandarización. W es por filas, por ejemplo
list.queen<-poly2nb(colonias_shp, queen=TRUE)
Wqueen_row<-nb2listw(list.queen, style="W", zero.policy=TRUE)

#Un resumen de lo que acabamos de hacer
summary(Wqueen_row, zero.policy=T)

#Lo que acabamos de hacer es obtener los links entre cada punto con sus vecinos
plot(Wqueen_row,coordinates(colonias_shp))

#Si queremos la matriz usando el inverso de las distancias
#Primero obtengamos la distancia
coords <- coordinates(colonias_shp) # nos da los centroides de las colonias
dist.mat <- as.matrix(dist(coords, method = "euclidean"))
dist.mat[1:5, 1:5]

#Y obtenemos el inverso de cada entrada
dist.mat.inv <- 1 / dist.mat 
diag(dist.mat.inv) <- 0 # hacemos la diagonal igual a cero
dist.mat.inv[1:5, 1:5]

#La convertimos en un objeto para usarla en modelos espaciales
Winvd_row <- mat2listw(dist.mat.inv, style = "W", row.names = colonias_shp$cve_col) #Esto puede tardar un poco
summary(Winvd_row)




#Test de Moran
moran.mco<-lm.morantest(m.mco, Wqueen_row, alternative="two.sided", zero.policy = TRUE)
print(moran.mco)

#Se rechaza la H0 de aleatoriedad de los errores



#Regresiones espaciales
#Usamos paquete spatialreg

#Modelo SAR
m.sar<-lagsarlm(robos ~ totems ,
                data=colonias_shp@data,
                listw=Wqueen_row,
                zero.policy=T)
summary(m.sar)
#Noten que rho es el estimado de lo que en clase llamamos lambda

#Modelo Durbin
m.sdm<-lagsarlm(robos ~ totems ,
                data=colonias_shp@data,
                listw=Wqueen_row,
                Durbin=T,
                zero.policy=T)
summary(m.sdm)


#Modelo SEM
m.sem<-errorsarlm(robos ~ totems ,
                  data=colonias_shp@data,
                  listw=Wqueen_row,
                  zero.policy=T)
summary(m.sem)
#Aquí la salida llama lambda lo que en clase llamamos rho


#SAC
m.sac<-sacsarlm(robos ~ totems ,
                data=colonias_shp@data,
                listw=Wqueen_row,
                zero.policy=T)
summary(m.sac)

#Impactos totales con 10 simulaciones
#Pongan más, quizás 200, y déjenlo correr una noche para mejor inferencia
im <- impacts(m.sar,
              listw=Wqueen_row,
              R=10,
              zstats=TRUE)
print(im)