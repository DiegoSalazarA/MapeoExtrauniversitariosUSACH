#Codigo de trabajo "Ciencia Abierta y Software Libre"
#Estudiante: Diego Salazar Alvarado



# ---- 0. Aspectos Generales ----



#Cargar paquetes
library(readxl)
library(dplyr)
library(tidyr)
library(car)
library(ggplot2)
library(summarytools)

#Creamos objeto desde la base de datos, seleccionado hoja numero 2
actoresproy1820 <- read_excel("Data/Original Data/proy_usach.xlsx",2)

#Revisar base
names(actoresproy1820)
summary(actoresproy1820)

# ---- 1. Preparar objetos para el analisis ----

#Seleccion de varibles
actores <- select(actoresproy1820, Inicio, ID, titulo,
                  Facultad, Sector, Tipo, Comuna, Region)

#unir ID y fecha de inicio, para generar un nuevo ID que incluya el año en el mismo vector

actores <- unite(actores, Inicio, ID, c(1:2), sep= "") 
actores <- select(actores, ID = Inicio, titulo, Facultad, Sector, Tipo, Comuna, Region)

#Crea objeto para  RM, eliminando los casos internacionales y de otras regiones
actoresRM <- filter(actores, Region == "RM")

#Crear Columna Zona, desde la categorizacion de las comunas en tres circulos concentricos
# EI: Entorno inmediato
# CC2: otras comunas dentro de cicunvalacion vespucio
# CC3: Comunas fuera de cicunvalacion vespucio

actoresRM <- mutate(actoresRM, zona = car::recode(actoresRM$Comuna,
            "'Estacion Central' = 'EI'; 'Cerrillos' = 'EI'; 'Maipu'= 'EI'; 
            'Pudahuel' = 'EI'; 'Lo Prado' = 'EI'; 'Quinta Normal' = 'EI';
            'Santiago'= 'EI'; 'Pedro Aguirre Cerda' = 'EI'; 'Nunoa' = 'CC2'; 
            'Cerro Navia' = 'CC2';'La Granja' = 'CC2'; 'Providencia' = 'CC2'; 
            'Conchali' = 'CC2'; 'La Cisterna' = 'CC2'; 'San Miguel' = 'CC2';
            'San Bernardo' = 'CC3'; 'Isla de Maipo' = 'CC3';'Huechuraba' = 'CC3';
            'La Florida' = 'CC3'; 'Las Condes' = 'CC3'; 'Vitacura' = 'CC3'; 'La Pintana' = 'CC3';
            'Puente Alto' = 'CC3' ; 'San Jose de Maipo' = 'CC3'; 'El Bosque' = 'CC3';
            'Lampa' = 'CC3';'Melipilla' = 'CC3'; 'Quilicura' = 'CC3'"))

#Guardar base de datos con los casos RM
saveRDS(actoresRM, file = "Data/Analysis Data/actoresrm.rds" ,compress = F )

# ---- 2. Analisis ----

# ---- 2.1 Analisis por distribucion territorial  ----

#  Por Comuna (Esta informacion queda disponible para el desarrollo del mapa)

table(actoresRM$Comuna)

# Tablas cruzando Comuna y Sector (publico,social,productivo o educativo no universitario)

table(actoresRM$Comuna, actoresRM$Sector)
round(prop.table(table(actoresRM$Comuna, actoresRM$Sector))*100, digits = 1)

#  Por Zona y Sector

table(actoresRM$zona,actoresRM$Sector)
round(prop.table(table(actoresRM$zona,actoresRM$Sector))*100, digits = 1)


# Por Zona y Facultad

table(actoresRM$Facultad,actoresRM$zona)
round(prop.table(table(actoresRM$Facultad,actoresRM$zona))*100, digits = 1)

# ---- 2.2 Analisis por Sector ----


#Analisis por Sector y subsector de actor

table(actoresRM$Sector)
round(prop.table(table(actoresRM$Sector))*100, digits = 1)

# por Sector y Facultad

table(actoresRM$Facultad, actoresRM$Sector)
round(prop.table(table(actoresRM$Facultad, actoresRM$Sector))*100, digits = 1)


# Creamos objetos por cada uno de los Sectores, para analisar sus subsectores

EducacionRM <- filter(actoresRM, Sector == "Educacion")
table(EducacionRM$Tipo)
round(prop.table(table(EducacionRM$Tipo, EducacionRM$zona))*100, digits = 1)

ProductivosRM <- filter(actoresRM, Sector == "Productivo")
table(ProductivosRM$Tipo)
round(prop.table(table(ProductivosRM$Tipo, ProductivosRM$zona))*100, digits = 1)

PublicosRM <- filter(actoresRM, Sector == "publico")
table(PublicosRM$Tipo)
round(prop.table(table(PublicosRM$Tipo, PublicosRM$zona))*100, digits = 1)

SocialesRM <- filter(actoresRM, Sector == "Social")
table(SocialesRM$Tipo)
round(prop.table(table(SocialesRM$Tipo, SocialesRM$zona))*100, digits = 1)

# ---- 3. Imprimir Resultados ----

# Gráfico de Cantidad de actores por zona
# Transformamos el vestor zona en factor, las etiquetas se orenan segun el alfabeto
actoresRM <- mutate(actoresRM, zona_factor = factor(actoresRM$zona,
                                                    labels = c("CC2","CC3","EI"))) 

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
