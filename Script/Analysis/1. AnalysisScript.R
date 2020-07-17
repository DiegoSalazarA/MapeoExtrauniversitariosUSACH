# ---- 0. Aspectos Generales ----

#Cargar paquetes
library(dplyr)
library(summarytools)

#Abrimos base para el analisis  creamos objeto
actoresRM <- readRDS("Data/Analysis Data/actoresrm.rds")

# ---- 1. Tablas ----

# Tabla 1: porcentaje de actores extrauniversitarios según sector

tabla1 <- freq(actoresRM$Sector)

#Tabla 2: distribución porcentual de actores extrauniversitarios según sector y área de proyecto:

tabla2 <- ctable(actoresRM$Sector, actoresRM$Area, prop = "t" , totals = T, style = 'rmarkdown', headings = F)

#Tabla 3: porcentajes de distribución porcentual según distancia del campus de actores del medio por sector:

tabla3 <- ctable(actoresRM$Sector, actoresRM$Circunvalacion, prop = "t" , totals = T, style = 'rmarkdown', headings = F)

#Tabla 4: porcentajes de distribución porcentual de actores del medio según IPS

tabla4 <- ctable(actoresRM$Sector, actoresRM$IPS, prop = "t" , totals = T, style = 'rmarkdown', headings = F)

# ---- 2. Guardar en lista ----

tablas <- list(tabla1, tabla2, tabla3, tabla4)

saveRDS(tablas , file = "Data/Analysis Data/tablas.rds")


# Limpiar entorno de trabajo
rm(list=ls())













