# ---- 0. Aspectos Generales ----



#Cargar paquetes
library(dplyr)
library(ggplot2)
library(summarytools)

#Abrimos base para el analisis  creamos objeto
actoresRM <- readRDS("Data/Analysis Data/actoresrm.rds")

# ---- 1. Analisis ----

# ---- 1.1 Analisis por distribucion territorial  ----

#  Por Comuna (Esta informacion queda disponible para el desarrollo del mapa)

freq(actoresRM$Comuna)

# Tablas cruzando Comuna y Sector (publico,social,productivo o educativo no universitario)
# prop = "c" se usa para que la proporcion se presente en las columnas.

ctable(actoresRM$Comuna, actoresRM$Sector, prop = "t" , totals = F)

#  Por Zona y Sector

ctable(actoresRM$zona,actoresRM$Sector,  prop = "t" , totals = T)

# Por Zona y Facultad

ctable(actoresRM$Facultad,actoresRM$zona, prop = "t" , totals = F)

# Por IPS

freq(actoresRM$IPS)

ctable(actoresRM$IPS_factor,actoresRM$Sector, prop = "t" , totals = T)

ctable(actoresRM$IPS_factor,actoresRM$Tipo , prop = "t" , totals = F)

# ---- 2.2 Analisis por Sector ----

#Analisis por Sector y subsector de actor

freq(actoresRM$Sector)

# por Sector y Facultad

ctable(actoresRM$Facultad, actoresRM$Sector, prop = "c" , totals = F)

round(prop.table(table(actoresRM$Facultad, actoresRM$Sector))*100, digits = 1)


# Creamos objetos por cada uno de los Sectores, para analisar sus subsectores

EducacionRM <- filter(actoresRM, Sector == "Educacion")
freq(EducacionRM$Tipo)
ctable(EducacionRM$Tipo, EducacionRM$zona, prop = "t" , totals = T)


ProductivosRM <- filter(actoresRM, Sector == "Productivo")
freq(ProductivosRM$Tipo)
ctable(ProductivosRM$Tipo, ProductivosRM$zona, prop = "t" , totals = T)

PublicosRM <- filter(actoresRM, Sector == "publico")
freq(PublicosRM$Tipo)
ctable(PublicosRM$Tipo, PublicosRM$zona, prop = "t" , totals = T)

SocialesRM <- filter(actoresRM, Sector == "Social")
freq(SocialesRM$Tipo)
ctable(SocialesRM$Tipo, SocialesRM$zona, prop = "t" , totals = T)

# ---- 3. Imprimir Resultados ----

# Gráfico de Cantidad de actores por zona

ggplot(actoresRM, aes(x=zona_factor)) +
  geom_bar(width = 0.3,  fill=rgb(1,0,0.4,0.8)) +
  scale_x_discrete("Circulo Concentrico") + scale_y_continuous("Cantidad de Actores") + 
  labs(title = "Cantidad de actores por zona")

ggsave("Output/actoresporzona.png")

# Gráfico de Cantidad de actores por Sector

actoresRM <- mutate(actoresRM, sector_factor = factor(actoresRM$Sector,
                                                      labels = c("Educacion","Productivo",
                                                                 "Publico","Social"))) 

ggplot(actoresRM, aes(x=sector_factor)) + 
  geom_bar(width = 0.3,  fill=rgb(1,0,0.4,0.8)) +
  scale_x_discrete("Sector") + scale_y_continuous("Cantidad de Actores") + 
  labs(title = "Cantidad de actores por sector")

ggsave("Output/actoresporsector.png")
