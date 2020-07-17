# ---- 0. Aspectos Generales ----
#Cargamos las librerias
library(chilemapas)
library(ggplot2)
library(breakDown)
library(dplyr)
library(RColorBrewer)
library(scales)
library(ggthemes)

# ---- 1. Preparamaos data Frame de las comunas del Fondo VIME ----

#Variable que nos ineresa mapear

Comunadf <- readRDS("Data/Analysis Data/Comunas.rds")
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

#Filtramos casos según la presencia

comunas_rmtodos <- filter(comunas_rm,Actores> 0) # dejamos solo las comunas con presencia
comunas_rmmas <- filter(comunas_rm,per> 5) # dejamos las comunas que concentren actores sobre el %5

#Paleta de colores 

prioridad <- c("#3B83BD", "#BDECB6", "#FDFD96","#FFA07A", "#FF0000")
mora2 <- c("#4A148C", "#7B1FA2", "#AB47BC", "#CE93D8", "#F3E5F5") 

# ---- 2. Mapas ----

#Mapa 1: comunas de la Región Metropolitana según distancia del campus universitario

mapa1 <- ggplot(comunas_rm) + 
  geom_sf(aes( fill = Circu, geometry = geometry)) +
  scale_fill_discrete(name = "  ")+
  theme(legend.position = "bottom")+
  labs(title = "Mapa 1:", 
       subtitle = "Comunas de la Región Metropolitana según distancia del campus universitario",
       caption = "Fuente: Elaboración propia desde los datos del Fondo VIME-USACH")+
  theme_bw(base_size = 13)

#Mapa 2: comunas de la Región Metropolitana según distancia Índice de Prioridad Social

mapa2 <- ggplot(comunas_rm) + 
  geom_sf(aes( fill = IPSnum, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(prioridad), name = " ",
                       labels = c("Alta","Media Alta","Media Baja","Baja","Sin Prioridad"))+
  theme(legend.position = "bottom")+
  labs(title = "Mapa 2:", 
       subtitle = "Comunas de la Región Metropolitana según distancia Índice de Prioridad Social",
       caption = "Fuente: Elaboración propia según Gajardo (2019)")+
  theme_bw(base_size = 13)
#Mapa3 4:presencia de Actores del medio participantes de proyectos en las Comunas de la RM

mapa3 <- ggplot(comunas_rmtodos) + 
  geom_sf(aes(fill = per, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(mora2), name = "Porcentaje\nde actores") +
  labs(x = " ", y = " ")+
  labs(title = "Mapa 3:", 
       subtitle = "Presencia de Actores del medio participantes de proyectos en las Comunas de la RM",
       caption = "Fuente: Elaboración propia desde los datos del Fondo VIME-USACH")+
  theme_bw(base_size = 13)

#Mapa 4: comunas con la mayor cantidad de actores del medio

mapa4 <- ggplot(comunas_rmmas) + 
  geom_sf(aes(fill = per, geometry = geometry)) +
  geom_sf_label(aes(label = Comuna, geometry = geometry),
                label.size = 0.2,) +
  geom_sf_label(aes(label = per, geometry = geometry),
                label.size = 0.4,nudge_y = -0.016) +
  scale_fill_gradientn(colours = rev(mora2), name = "Porcentaje\nde actores") +
  labs(x = " ", y = " ")+
  labs(title = "Mapa 4:", 
       subtitle = "Comunas con la mayor cantidad de actores del medio",
       caption = "Fuente: Elaboración propia desde los datos del Fondo VIME-USACH")+
  theme_bw(base_size = 13)

#Mapa 5: comunas en las que se desarrollan proyectos, según IPS

mapa5 <- ggplot(comunas_rmtodos) + 
  geom_sf(aes( fill = IPSnum, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(prioridad), name = " ",
                       labels = c("Alta","Media Alta","Media Baja","Baja","Sin Prioridad"))+
  theme(legend.position = "bottom")+
  labs(title = "Mapa 5:", 
       subtitle = "Comunas donde se darrollan proyectos según Indice de Prioridad Social",
       caption = "Fuente: Elaboración propia según Gajardo (2019) y datos de la Unidad de Estudios e Instrumentos VIME")+
  theme_bw(base_size = 13)



# ---- 3. Guardar en lista ----

mapas <- list(mapa1 ,mapa2, mapa3, mapa4, mapa5)

saveRDS(mapas , file = "Data/Analysis Data/mapas.rds")


# Limpiar entorno de trabajo
rm(list=ls())
