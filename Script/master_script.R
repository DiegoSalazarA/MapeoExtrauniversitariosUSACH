# ---- 0. Aspectos Generales ----
# Instalación de paquetes
## Instalar paquetes para el espacio de trabajo
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(readxl,
               dplyr,
               car,
               summarytools,
               ggplot2,
               breakDown,
               RColorBrewer,
               chilemapas,
               scales,
               ggthemes)

# ---- 1. Script ----

#Ejecuta Script de procesamiento de datos
source("Script/Proccesing/0. ProccesingScript.R")

# Ejecuta Script 2 analisis de datos y creación de tablas
source("Script/Analysis/1. AnalysisScript.R")

# Ejecuta Script 3 analisis de datos y creación de graficos
source("Script/Analysis/2. Graph.R")

# Ejecuta Script 4 analisis de datos y creación de mapas
source("Script/Analysis/3. Map.R")

# ---- 2.Reporte----

# Ejecuta código 4: construcción de reporte reproducible
# En base a lista de resultados, crea archivo de reporte en PDF
rmarkdown::render('Reporte.Rmd')

# Limpiar entorno de trabajo
rm(list=ls())