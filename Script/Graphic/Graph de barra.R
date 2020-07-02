#Construcción gráficos de: 
#https://www.youtube.com/watch?v=m7pbSX-yejk&list=PL-D6vWP6mF1FUdXJGxzVhdEOvo8pDFgzm&index=3&t=0s

library(ggplot2)
library(breakDown)
library(dplyr)
library(readxl)


# Se selecciona y crea un vector

BaseFVIME <- read_excel("Data/Original Data/Proy_usach.xlsx", sheet =2)
BaseFVIME <- filter(BaseFVIME, Region == "RM")

# ---- 1. Gráfico de comunas ----


dfcomunas<- select(BaseFVIME, Comuna)
dfcomunas<- mutate(dfcomunas, Comuna = as.factor(dfcomunas$Comuna))

sum(is.na(dfcomunas)) #Revisamos si tiene NA's
dfcomunas <- na.omit(dfcomunas)


## creamos nuevo data frame para seleccionar variable que cruza lo anterior y contarlas con "count" de dplyr

dfcomunascount <- dfcomunas %>%
  count(Comuna)


## gráfico usando df "dfcomunascount" 

ggplot(dfcomunascount, aes(x = reorder(Comuna, -n), y = n)) + #Comando reorder partiendo en -n para que sea decreciente
  geom_bar(stat = "identity", fill = "blue", color = "black") + #Identity es para mostrar la cantidad sobre la barra
  geom_text(aes(label = n), vjust = -0.50) + #Geom_text label = N es que va a poner el numero en "n" y vjust es para ver que tan abajo/arriba está el número
  labs(x = "Comunas", y = "Cantidad", 
       title = "Cantidad de Actores por Comuna")+
  theme_bw(base_family = "") +  
  theme(axis.text.x = element_blank())#borra etiquetas

## Mismo gráfico anterior, solo que en porcentajes, Primero creamos la variable luego el graf.

dfcomunascount100<-dfcomunascount %>%
  count(Comuna) %>% #Nuevo DF 
  mutate(per = n / sum(n), #Contiene n/variable dividido por total n
         per_label = paste0(round(per*100), "%")) #Aquí tiene el porcentaje de cada "label" round/100 y mostrado en %

#Gráfico reordenado con escalas de colores por "variable
ggplot(dfcomunascount100, aes(x = reorder(Comuna, -per), y = per, fill = reorder(Comuna,-per)  )) + #Se le agrega fill "problema_imp" para que llene esa variable
  geom_bar(stat = "identity") +
  geom_text(aes(label = per_label), vjust = -0.50) + 
  labs(x = "Problemas señalados", y = "Cantidad",
       title = "Gráfico de barras 3", subtitle = "Problemas más importantes (Primera mención) Año 2003") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw(base_family = "") +  
    theme(axis.text.x = element_blank())#borra etiquetas

## para guardar pe: ggsave("Output/Graphic/Actoresporcomuna.png")


# ---- 2. Gráfico por Sector


dfArea<- select(BaseFVIME, area)
#Ponemos etiquetas

sum(is.na(dfArea)) #Revisamos si tiene NA's
dfcomunas <- na.omit(dfArea)

## creamos nuevo data frame para seleccionar variable de problemas importantes y contarlas con "count" de dplyr

dfArea<- dfArea %>%
  count(area)


## Nuevo gráfico usando df "problemas_data03"

ggplot(dfArea, aes(x = reorder(area, -n), y = n)) + #Comando reorder partiendo en -n para que sea decreciente
  geom_bar(stat = "identity", fill = "blue", color = "black") + #Identity es para mostrar la cantidad sobre la barra
  geom_text(aes(label = n), vjust = -0.50) + #Geom_text label = N es que va a poner el numero en "n" y vjust es para ver que tan abajo/arriba está el número
  labs(x = "Comunas", y = "Cantidad", 
       title = "Cantidad de Actores por Comuna")

## porcentajes 

dfArea<-dfArea %>%
  count(area) %>% #Nuevo DF 
  mutate(per = n / sum(n), #Contiene n/variable dividido por total n
         per_label = paste0(round(per*100), "%")) #Aquí tiene el porcentaje de cada "label" round/100 y mostrado en %

#Gráfico reordenado con escalas de colores por "variable

ggplot(dfArea, aes(x = reorder(area, -per), y = per, fill = reorder(area, -per))) + #Se le agrega fill "problema_imp" para que llene esa variable
  geom_bar(stat = "identity") +
  geom_text(aes(label = per_label), vjust = -0.50) + 
  labs(x = " ", y = "Porcentaje de actores participantes",
       title = "Figura 3 Distribución de Actores por Área de Proyecto", 
       subtitle = "Porcenjate de actores distribuidos según ára de trabajo del proyecto",
       caption = "Fuente: Elaboración propia desde los datos de la Unidad de Estudios e Instrumentos VIME") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()

### ggsave("Output/Graphic/Actoresporarea.png")

# ----3. Grafico por sector ---- 


dfSector<- select(BaseFVIME, Sector)
#Ponemos etiquetas

sum(is.na(dfSector)) #Revisamos si tiene NA's
dfcomunas <- na.omit(dfSector)

## creamos nuevo data frame para seleccionar variable de problemas importantes y contarlas con "count" de dplyr

dfSector<- dfSector %>%
  count(Sector)


## Nuevo gráfico usando df "problemas_data03"

ggplot(dfSector, aes(x = reorder(Sector, -n), y = n)) + #Comando reorder partiendo en -n para que sea decreciente
  geom_bar(stat = "identity", fill = "blue", color = "black") + #Identity es para mostrar la cantidad sobre la barra
  geom_text(aes(label = n), vjust = -0.50) + #Geom_text label = N es que va a poner el numero en "n" y vjust es para ver que tan abajo/arriba está el número
  labs(x = "Comunas", y = "Cantidad", 
       title = "Cantidad de Actores por Comuna")

## Mismo gráfico anterior, solo que en porcentajes 

dfSector<-dfSector %>%
  count(Sector) %>% #Nuevo DF 
  mutate(per = n / sum(n), #Contiene n/variable dividido por total n
         per_label = paste0(round(per*100), "%")) #Aquí tiene el porcentaje de cada "label" round/100 y mostrado en %


#Gráfico reordenado con escalas de colores por "variable
ggplot(dfSector, aes(x = reorder(Sector, -per), y = per, fill = Sector)) + #Se le agrega fill "problema_imp" para que llene esa variable
  geom_bar(stat = "identity") +
  geom_text(aes(label = per_label), vjust = -0.50) + 
  labs(x = "Problemas señalados", y = "Cantidad",
       title = "Gráfico de barras 3", subtitle = "Problemas más importantes (Primera mención) Año 2003") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()
