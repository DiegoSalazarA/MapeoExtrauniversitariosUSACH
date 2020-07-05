# ---- 0. Aspectos Generales ----

#Cargar paquetes
library(dplyr)
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

ctable(actoresRM$Circunvalación,actoresRM$Sector,  prop = "t" , totals = T)

# Por Zona y Facultad

ctable(actoresRM$Facultad,actoresRM$Circunvalación, prop = "t" , totals = F)

# Por IPS

freq(actoresRM$IPS)

ctable(actoresRM$IPS,actoresRM$Sector, prop = "t" , totals = T)

ctable(actoresRM$IPS,actoresRM$Subsector , prop = "t" , totals = F)

# ---- 2.2 Analisis por Sector ----
#Analisis por Sector y subsector de actor

freq(actoresRM$Sector)

# por Sector y Facultad

ctable(actoresRM$Facultad, actoresRM$Sector, prop = "c" , totals = F)

round(prop.table(table(actoresRM$Facultad, actoresRM$Sector))*100, digits = 1)

# Creamos objetos por cada uno de los Sectores, para analisar sus subsectores

EducacionRM <- filter(actoresRM, Sector == "Educacion")
freq(EducacionRM$Subsector)
ctable(EducacionRM$Subsector, EducacionRM$Circunvalación, prop = "t" , totals = T)

ProductivosRM <- filter(actoresRM, Sector == "Productivo")
freq(ProductivosRM$Subsector)
ctable(ProductivosRM$Subsector, ProductivosRM$Circunvalación, prop = "t" , totals = T)

PublicosRM <- filter(actoresRM, Sector == "publico")
freq(PublicosRM$Subsector)
ctable(PublicosRM$Subsector, PublicosRM$Circunvalación, prop = "t" , totals = T)

SocialesRM <- filter(actoresRM, Sector == "Social")
freq(SocialesRM$Subsector)
ctable(SocialesRM$Subsector, SocialesRM$Circunvalación, prop = "t" , totals = T)
