# ---- 0. Aspectos Generales ----
#Construcción gráficos de: 
#https://www.youtube.com/watch?v=m7pbSX-yejk&list=PL-D6vWP6mF1FUdXJGxzVhdEOvo8pDFgzm&index=3&t=0s

library(ggplot2)
library(breakDown)
library(dplyr)
library(readxl)
library(RColorBrewer)

#Paletas:
prioridad <- c("#3B83BD", "#BDECB6", "#FDFD96","#FFA07A", "#FF0000")
mora2 <- c("#4A148C", "#7B1FA2", "#AB47BC", "#CE93D8", "#F3E5F5") 

# Se seleccionara y hará un vector
BaseFVIME <- readRDS("Data/Analysis Data/actoresrm.rds") 
# ---- 1. Graficos ----

# ---- Gráfico 1 ----

dfSectorArea<- select(BaseFVIME, Sector, Area)
#Ponemos etiquetas

sum(is.na(dfSectorArea)) #Revisamos si tiene NA's
dfSector <- na.omit(dfSectorArea)

## creamos nuevo data frame para seleccionar variable de problemas importantes y contarlas con "count" de dplyr. Luego calculamos porcentajes

dfSectorArea<- dfSectorArea %>%
  count(Sector, Area)
dfSectorArea<-dfSectorArea %>%
  count(Sector, Area) %>% #Nuevo DF 
  mutate(per = n / sum(n), #Contiene n/variable dividido por total n
         per_label = paste0(round(per*100), "%")) #Aquí tiene el porcentaje de cada "label" round/100 y mostrado en %

#Gráfico 1: actores extrauniversitarios distribuidos según sector y área de proyecto

grafico1 <- ggplot(dfSectorArea,
       aes(x = reorder(Sector, -per), y = per, fill = reorder(Area,-per),)) + #Se le agrega fill para que llene esa variable
  geom_bar(stat = "identity",width = 0.4) +
  scale_fill_brewer(palette = "Dark2", name = "Área del Proyecto")+  
    labs(x = "Sector", y = "Actores",
       title = "Gráfico 1:", subtitle = "Actores extrauniversitarios distribuidos según sector y área de proyecto",
       caption = "Fuente: elaboración propia a partir de los datos del Fondo VIME-USACH") +
    scale_y_continuous(labels = scales::percent) +
    theme_bw() 

# ---- Grafico 2 ----

dfSectorcircu<- select(BaseFVIME, Sector, Circunvalacion)
#Ponemos etiquetas

sum(is.na(dfSectorcircu)) #Revisamos si tiene NA's
dfSector <- na.omit(dfSectorcircu)

## creamos nuevo data frame para seleccionar variable de problemas importantes y contarlas con "count" de dplyr

dfSectorcircu<- dfSectorcircu %>%
  count(Sector, Circunvalacion)
#Porcentajes
dfSectorcircu<-dfSectorcircu %>%
  count(Sector, Circunvalacion) %>% #Nuevo DF 
  mutate(per = n / sum(n), #Contiene n/variable dividido por total n
         per_label = paste0(round(per*100), "%")) #Aquí tiene el porcentaje de cada "label" round/100 y mostrado en %

# Grafico 2:
grafico2 <- ggplot(dfSectorcircu,
       aes(x = reorder(Sector, -per), y = per, fill = reorder(Circunvalacion,-per),)) + #Se le agrega fill para que llene esa variable
  geom_bar(stat = "identity",width = 0.4) +
  scale_fill_manual(values=mora2, name = "Circunvalación")+  
  labs(x = "Sector", y = "Actores",
       title = "Gráfico 2:", subtitle = "Distribución porcentual según distancia del campus de actores del medio por sector",
       caption = "Fuente: elaboración propia a partir de los datos del Fondo VIME-USACH") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() 

#---- Grafico 3 ----

dfSectorIPS<- select(BaseFVIME, Sector, IPS)
#Ponemos etiquetas
sum(is.na(dfSectorIPS)) #Revisamos si tiene NA's
dfSector <- na.omit(dfSectorIPS)
## creamos nuevo data frame para seleccionar variable de problemas importantes y contarlas con "count" de dplyr
dfSectorIPS<- dfSectorIPS %>%
  count(Sector, IPS)
dfSectorIPS<-dfSectorIPS %>%
  count(Sector, IPS) %>% #Nuevo DF 
  mutate(per = n / sum(n), #Contiene n/variable dividido por total n
         per_label = paste0(round(per*100), "%")) #Aquí tiene el porcentaje de cada "label" round/100 y mostrado en %

#Grafico 3: distribución porcentual de actores según IPS 

grafico3 <- ggplot(dfSectorIPS,
       aes(x = reorder(Sector, -per), y = per, fill = reorder(IPS,-per),)) + #Se le agrega fill para que llene esa variable
  geom_bar(stat = "identity",width = 0.4) +
  scale_fill_manual(values=prioridad, name = "IPS")+  
  labs(x = "Sector", y = "Actores",
       title = "Gráfico 3:", subtitle = "Distribución porcentual de actores según IPS",
       caption = "Fuente: elaboración propia a partir de Gajardo (2019) y los datos del Fondo VIME-USACH") +
    scale_y_continuous(labels = scales::percent) +
    theme_bw() 

# ---- 2. Guardar en lista ----

graficos <- list(grafico1, grafico2, grafico3)
saveRDS(graficos , file = "Data/Analysis Data/graficos.rds")


# Limpiar entorno de trabajo
rm(list=ls())



