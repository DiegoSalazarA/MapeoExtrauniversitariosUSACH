#Cargamos las librerias
library(chilemapas)
library(ggplot2)
library(breakDown)
library(dplyr)
library(readxl)
library(RColorBrewer)
library(scales)
library(ggthemes)

# ---- 1. Preapramaos data Frame de las comunas del Fondo VIME ----

#Variable que nos ineresa mapear

Comunadf <- read_excel("Data/Analysis Data/Comunas.xlsx")

Comunadf <- mutate(Comunadf, Circu = factor(Comunadf$Circu,
                  labels = c("Circunvalación Proxima","Circunvalación Media",
                   "Fuerda de la Ciudad","Entorno inmediato")))

dfcomunas <- Comunadf %>%
  mutate (Comunadf,codigo_comuna=as.character(Comunadf$codigo_comuna))%>%
  group_by(codigo_comuna) 

#información geográfica 

comunas_rm <- mapa_comunas %>% #objeto mapa_comunas es del paquete chilemapas
  filter(codigo_region == 13) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))) %>% 
  left_join(dfcomunas)

# ---- 2. Preparamos otros df y colores ----

#Filtramos casos según la presencia

comunas_rmtodos <- filter(comunas_rm,Actores> 0) # dejamos solo las comunas con presencia
comunas_rmmas <- filter(comunas_rm,per> 5) # dejamos las comunas que concentren actores sobre el %5
comunas_EI <- filter(comunas_rm, Circu == "EI") # solo entorno inmediato
comunas_CC2 <- filter(comunas_rm, Circu == "CC2") # solo cc2
#Armamos paleta de colores para su empleo
  
paleta <- c("#DCA761", "#CFB567", "#BFBC71", "#9EA887", "#819897")
paleta2 <- c("#222222", "#666666", "#999999", "#CCCCCC", "#FAFAFA")
Greys<- c("#212121", "#616161", "#BDBDBD", "#EEEEEE", "#F5F5F5", "#FAFAFA")
mora <- c("#4A148C", "#7B1FA2", "#9C27B0", "#CE93D8", "#F3E5F5") 
prioridad <- c("#3B83BD", "#BDECB6", "#FDFD96","#FFA07A", "#FF0000")

# ---- 3. Graficos definitivos ----


#Mapa de comunas de la RM

ggplot(comunas_rm) + 
  geom_sf(aes(fill = Actores, geometry = geometry)) +
    scale_fill_gradientn(colours = rev(paleta2), name = "Cantidad de actores") +
  labs(title = "Actores en Comunas") +
  theme_bw(base_size = 13)


#mapa solo de las comunas con participación

ggplot(comunas_rmtodos) + 
  geom_sf(aes(fill = per, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(mora), name = "Porcentaje\nde actores") +
  labs(x = " ", y = " ",
       title = "Mapa N°3: Actores participantes en las comunas de la RM", 
       subtitle = "Porcentaje de Actores extrauniversitarios participantes en el Fondo VIME 2018-2020 y comuna de origen ")+
  theme_bw(base_size = 10)


#mapa con las comunas con mayor % de participación

ggplot(comunas_rmmas) + 
  geom_sf(aes(fill = per, geometry = geometry)) +
  geom_sf_label(aes(label = nombre_comuna, geometry = geometry),
                label.size = 1.2,) +
  geom_sf_label(aes(label = per, geometry = geometry),
                label.size = 1.2,nudge_y = -0.012) +
  scale_fill_gradientn(colours = rev(mora), name = "Porcentaje\nde actores") +
  labs(x = " ", y = " ",
       title = "Mapa N°1: Actores participantes en las comunas de la RM", 
       subtitle = "Porcentaje de Actores extrauniversitarios participantes en el Fondo VIME 2018-2020 y comuna de origen ")+
  theme_bw(base_size = 10)


ggplot(comunas_rmmas) + 
  geom_sf(aes(fill = per, geometry = geometry)) +
  geom_sf_label(aes(label = nombre_comuna, geometry = geometry),
                label.size = 1.2,) +
  geom_sf_label(aes(label = per, geometry = geometry),
                label.size = 1.2,nudge_y = -0.012) +
  scale_fill_gradientn(colours = rev(mora), name = "Porcentaje\nde actores") +
  labs(x = " ", y = " ",
       title = "Mapa N°1: Actores participantes en las comunas de la RM", 
       subtitle = "Porcentaje de Actores extrauniversitarios participantes en el Fondo VIME 2018-2020 y comuna de origen ")+
  theme_bw(base_size = 10)


# ---- Graficos con IPS y Circunvalación ----

##Circulos concentrícos

ggplot(comunas_rm) + 
  geom_sf(aes( fill = Circu, geometry = geometry)) +
  scale_fill_discrete(name = "  ")+
  theme(legend.position = "bottom")+
    labs(title = "Mapa N°1: Áreas de Influencia Fondo VIME - USACH", 
         subtitle = "Distribución de comunas de la RM según círculos concéntricos",
         caption = "Fuente: Elaboración propia desde los datos de la Unidad de Estudios e Instrumentos VIME")+
  theme_bw(base_size = 13)

### imprime en W1000 l 619

#IPS

ggplot(comunas_rm) + 
  geom_sf(aes( fill = IPSnum, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(prioridad), name = " ",
                       labels = c("Alta","Media Alta","Media Baja","Baja","Sin Prioridad"))+
  theme(legend.position = "bottom")+
  labs(title = "Mapa N°2: Comunas según IPS", 
       subtitle = "Indice de Prioridad Social",
       caption = "Fuente: Elaboración propia según Gajardo (2019)")+
  theme_bw(base_size = 13)


#IPS participanetes

ggplot(comunas_rmtodos) + 
  geom_sf(aes( fill = IPSnum, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(prioridad), name = " ",
                       labels = c("Alta","Media Alta","Media Baja","Baja","Sin Prioridad"))+
  theme(legend.position = "bottom")+
  labs(title = "Mapa N°5: Comunas e IPS", 
       subtitle = "Comunas donde se darrollan proyectos según Indice de Prioridad Social",
       caption = "Fuente: Elaboración propia según Gajardo (2019) y datosde la Unidad de Estudios e Instrumentos VIME")+
  theme_bw(base_size = 13)

ggplot(comunas_rmmas) + 
  geom_sf(aes( fill = IPSnum, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(prioridad), name = " ",
                       labels = c("Alta","Media Alta","Media Baja","Baja","Sin Prioridad"))+
  theme(legend.position = "bottom")+
  labs(title = "Mapa N°5: Comunas e IPS", 
       subtitle = "Comunas donde se darrollan proyectos según Indice de Prioridad Social",
       caption = "Fuente: Elaboración propia según Gajardo (2019) y datosde la Unidad de Estudios e Instrumentos VIME")+
  theme_bw(base_size = 13)


#### ---- Mapa 4

ggplot(comunas_rmtodos) + 
  geom_sf(aes( fill = Circu, geometry = geometry)) +
  scale_fill_discrete(name = "  ")+
  theme(legend.position = "bottom")+
  labs(title = "Mapa N°3: Distribución de Actores", 
       subtitle = "Distribución de actores según círculos concéntricos",
       caption = "Fuente: Elaboración propia desde los datos de la Unidad de Estudios e Instrumentos VIME")+
  theme_bw(base_size = 13)

### ---- Mapa 5

ggplot(comunas_rmmas) + 
  geom_sf(aes( fill = Circu, geometry = geometry)) +
  geom_sf_label(aes(label = nombre_comuna, geometry = geometry),
                label.size = 1.2,) +
  geom_sf_label(aes(label = per, geometry = geometry),
                label.size = 1.2,nudge_y = -0.012) +
  scale_fill_discrete(name = "  ")+
  labs(x = " ", y = " ",
    title = "Mapa N°4: Comunas que concentran los actores participantes del Fondo VIME", 
       subtitle = "Porcentaje de actores por comunas",
       caption = "Fuente: Elaboración propia desde los datos de la Unidad de Estudios e Instrumentos VIME")+
  theme_bw(base_size = 13)


### ---- Mapa 7


ggplot(comunas_rmtodos) + 
  geom_sf(aes( fill = IPSnum, geometry = geometry)) +
  scale_fill_discrete(name = "  ")+
  theme(legend.position = "bottom")+
  labs(title = "Mapa N°3: Distribución de Actores", 
       subtitle = "Distribución de actores según círculos concéntricos",
       caption = "Fuente: Elaboración propia desde los datos de la Unidad de Estudios e Instrumentos VIME")+
  theme_bw(base_size = 13)

### ---- Mapa 7

ggplot(comunas_rmmas) + 
  geom_sf(aes( fill = Circu, geometry = geometry)) +
  geom_sf_label(aes(label = nombre_comuna, geometry = geometry),
                label.size = 1.2,) +
  geom_sf_label(aes(label = per, geometry = geometry),
                label.size = 1.2,nudge_y = -0.012) +
  scale_fill_discrete(name = "  ")+
  labs(x = " ", y = " ",
       title = "Mapa N°4: Comunas que concentran los actores participantes del Fondo VIME", 
       subtitle = "Porcentaje de actores por comunas",
       caption = "Fuente: Elaboración propia desde los datos de la Unidad de Estudios e Instrumentos VIME")+
  theme_bw(base_size = 13)

#IPS

ggplot(comunas_rm) + 
  geom_sf(aes( fill = IPSnum, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(prioridad), name = " ",
                       labels = c("Alta","Media Alta","Media Baja","Baja","Sin Prioridad"))+
  theme(legend.position = "bottom")+
  labs(title = "Mapa N°2: Comunas según IPS", 
       subtitle = "Indice de Prioridad Social",
       caption = "Fuente: Elaboración propia según Gajardo (2019)")+
  theme_bw(base_size = 13)

ggplot(comunas_rmtodos) + 
  geom_sf(aes( fill = IPS, geometry = geometry, reorder(IPS,))) +
  scale_fill_discrete()+
  labs(title = "Actores en Comunas") +
  theme_bw(base_size = 13)

ggplot(comunas_rmmas) + 
  geom_sf(aes( fill = IPS, geometry = geometry)) +
  scale_fill_discrete()+
  labs(title = "Actores en Comunas") +
  theme_bw(base_size = 13)



#### ---- Presentación ----

mora2 <- c("#4A148C", "#7B1FA2", "#AB47BC", "#CE93D8", "#F3E5F5") 

ggplot(comunas_rmtodos) + 
  geom_sf(aes(fill = per, geometry = geometry)) +
 
  scale_fill_gradientn(colours = rev(mora2), name = "Porcentaje\nde actores") +
  labs(x = " ", y = " ")+
  labs(title = "  ", 
       subtitle = "  ",
       caption = "Fuente: elaboración propia a partir Gajardo (2019)")+
  theme_bw(base_size = 14)



ggplot(comunas_rmmas) + 
  geom_sf(aes(fill = per, geometry = geometry)) +
  geom_sf_label(aes(label = Comuna, geometry = geometry),
                label.size = 1.2,) +
  geom_sf_label(aes(label = per, geometry = geometry),
                label.size = 1.2,nudge_y = -0.012) +
  scale_fill_gradientn(colours = rev(mora2), name = "Porcentaje\nde actores") +
  labs(x = " ", y = " ")+
  labs(title = "  ", 
       subtitle = "  ",
       caption = "Fuente: elaboración propia a partir Gajardo (2019)")+
  theme_bw(base_size = 14)

##Circulos concentrícos

ggplot(comunas_rm) + 
  geom_sf(aes( fill = reorder(Circu,-per), geometry = geometry)) +
  scale_fill_discrete(name = "  ",labels = c("Comunas Intermedias","Comunas Distantes","Fuera de la Ciudad","Entorno Inmediato"))+
  theme(legend.position = "bottom")+
  labs(title = "  ", 
       subtitle = "  ",
       caption = "Fuente: Elaboración propia a partir los datos de la Unidad de Estudios e Instrumentos VIME")+
  theme_bw(base_size = 13)


+
  scale_fill_manual(values=c("gree n4", "purple4","black","blue"))


#### ---- Mapa 4






ggplot(comunas_rm) + 
  geom_sf(aes( fill = reorder(Circu,-per), geometry = geometry)) +
  scale_fill_discrete(name = "  ",labels = c ("Entorno Inmediato","Comunas Intermedias", "Comunas Distantes", "Fuera de la Ciudad"))+
  theme(legend.position = "bottom")+
  labs(title = " ", 
       subtitle = " ",
       caption = "Fuente: Elaboración propia a partir los datos de la Unidad de Estudios e Instrumentos VIME")+
  theme_bw(base_size = 13)+
  scale_fill_manual(values=c("#4A148C", "#7B1FA2","#CE93D8","#F3E5F5"))

### ---- Mapa 5







ggplot(comunas_rmmas) + 
  geom_sf(aes( fill = reorder(Circu,-per), geometry = geometry)) +
  geom_sf_label(aes(label = Comuna, geometry = geometry),
                label.size = 0.8,) +
  geom_sf_label(aes(label = per, geometry = geometry),
                label.size = 0.8,nudge_y = -0.015) +
  scale_fill_discrete(name = "  ",labels = c ("Entorno Inmediato","Comunas Intermedias"))+
  
  labs(x = " ", y = " ",
       title = "  ", 
       subtitle = "  ",
       caption = "Fuente: Elaboración propia a partir los datos de la Unidad de Estudios e Instrumentos VIME")+
  theme_bw(base_size = 13)+
  scale_fill_manual(values=c("#4A148C", "#7B1FA2","#CE93D8"))


#IPS

ggplot(comunas_rm) + 
  geom_sf(aes( fill = IPSnum, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(prioridad), name = " ",
                       labels = c("Alta","Media Alta","Media Baja","Baja","Sin Prioridad"))+
  theme(legend.position = "bottom")+
  labs(title = "  ", 
       subtitle = "  ",
       caption = "Fuente: elaboración propia a partir Gajardo (2019)")+
  theme_bw(base_size = 13)




ggsave("Output/Graphic/Comunas con mas de 5p Actores.png")

