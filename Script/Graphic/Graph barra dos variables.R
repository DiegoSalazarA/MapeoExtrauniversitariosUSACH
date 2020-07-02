#Construcción gráficos de: 
#https://www.youtube.com/watch?v=m7pbSX-yejk&list=PL-D6vWP6mF1FUdXJGxzVhdEOvo8pDFgzm&index=3&t=0s

library(ggplot2)
library(breakDown)
library(dplyr)
library(readxl)
library(RColorBrewer)

# Se seleccionara y hará un vector
BaseFVIME <- readRDS("Data/Analysis Data/actoresrm.rds") 

#---- 1. IPS

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

#### ---- Grafico Final ----

prioridad <- c("#3B83BD", "#BDECB6", "#FDFD96","#FFA07A", "#FF0000")
colours = rev(prioridad)


ggplot(dfSectorIPS,
       aes(x = reorder(Sector, -per), y = per, fill = reorder(IPS,-per),)) + #Se le agrega fill para que llene esa variable
  geom_bar(stat = "identity",width = 0.4) +
  
  scale_fill_manual(values=c("#3B83BD", "#BDECB6", "#FDFD96","#FFA07A", "#FF0000", name = "Tig" ))+  
 
  
     labs(x = "Sector", y = "Actores",
       title = "  ", subtitle = " ",
       caption = "Fuente: elaboración propia a partir de los datos de la Unidad de Estudios e Instrumentos VIME") +
  
  scale_y_continuous(labels = scales::percent) +
  
    theme_bw() 

# ---- 2. Area ----

dfSectorArea<- select(BaseFVIME, Sector, Area)
#Ponemos etiquetas

sum(is.na(dfSectorArea)) #Revisamos si tiene NA's
dfSector <- na.omit(dfSectorArea)

## creamos nuevo data frame para seleccionar variable de problemas importantes y contarlas con "count" de dplyr

dfSectorArea<- dfSectorArea %>%
  count(Sector, Area)


## Mismo gráfico anterior, solo que en porcentajes 

dfSectorArea<-dfSectorArea %>%
  count(Sector, Area) %>% #Nuevo DF 
  mutate(per = n / sum(n), #Contiene n/variable dividido por total n
         per_label = paste0(round(per*100), "%")) #Aquí tiene el porcentaje de cada "label" round/100 y mostrado en %

#### ---- Grafico Final ----

ggplot(dfSectorArea, aes(x = reorder(Sector, -per), y = per, fill = reorder(Area, -per)  )) + #Se le agrega fill "problema_imp" para que llene esa variable
  geom_bar(stat = "identity") +
  geom_text(aes(label = per_label), vjust = 0)+
  labs(x = "Sector", y = "Cantidad",
       title = "Gráfco N°1 Distribución por Sector según área del proyecto ", subtitle = " ",
       caption = "Fuente: Elaboración propia desde los datos de la Unidad de Estudios e Instrumentos VIME") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw(base_size = 11,   base_line_size = 16/30) 


mora2 <- c("#4A148C", "#7B1FA2", "#AB47BC", "#CE93D8", "#F3E5F5") 



ggplot(dfSectorArea,
       aes(x = reorder(Sector, -per), y = per, fill = reorder(Area,-per),)) + #Se le agrega fill para que llene esa variable
  geom_bar(stat = "identity",width = 0.4) +
  
  scale_fill_brewer(palette = "Dark2")+  
  
  
  labs(x = "Sector", y = "Actores",
       title = "  ", subtitle = " ",
       caption = "Fuente: elaboración propia a partir de los datos de la Unidad de Estudios e Instrumentos VIME") +
  
  scale_y_continuous(labels = scales::percent) +
  
  theme_bw() 

# ---- 3. Circunv ----


dfSectorcircu<- select(BaseFVIME, Sector, Circunvalación)
#Ponemos etiquetas

sum(is.na(dfSectorcircu)) #Revisamos si tiene NA's
dfSector <- na.omit(dfSectorcircu)

## creamos nuevo data frame para seleccionar variable de problemas importantes y contarlas con "count" de dplyr

dfSectorcircu<- dfSectorcircu %>%
  count(Sector, Circunvalación)


## Mismo gráfico anterior, solo que en porcentajes 

dfSectorcircu<-dfSectorcircu %>%
  count(Sector, Circunvalación) %>% #Nuevo DF 
  mutate(per = n / sum(n), #Contiene n/variable dividido por total n
         per_label = paste0(round(per*100), "%")) #Aquí tiene el porcentaje de cada "label" round/100 y mostrado en %

#### ---- Grafico Final ----


ggplot(dfSectorcircu, aes(x = reorder(Sector, -per), y = per, fill = Circunvalación)) + #Se le agrega fill "problema_imp" para que llene esa variable
  geom_bar(stat = "identity") +
  
  labs(x = "Sector", y = "Cantidad",
       title = "Gráfco N°2 Distribución de actores según ubicación ", subtitle = " ",
       caption = "Fuente: Elaboración propia desde los datos de la Unidad de Estudios e Instrumentos VIME") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() + 
  scale_fill_brewer(palette = "Dark2")





ggplot(dfSectorcircu,
       aes(x = reorder(Sector, -per), y = per, fill = reorder(Circunvalación,-per),)) + #Se le agrega fill para que llene esa variable
  geom_bar(stat = "identity",width = 0.4) +
  
  scale_fill_manual(values=c("#4A148C","#7B1FA2", "#CE93D8","#F3E5F5", "#FF0000", name = "Tig" ))+  
  
  
  labs(x = "Sector", y = "Actores",
       title = "  ", subtitle = " ",
       caption = "Fuente: elaboración propia a partir de los datos de la Unidad de Estudios e Instrumentos VIME") +
  
  scale_y_continuous(labels = scales::percent) +
  
  theme_bw() 








