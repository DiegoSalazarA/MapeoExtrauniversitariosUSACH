#Codigo de trabajo "Ciencia Abierta y Software Libre"
#Estudiante: Diego Salazar Alvarado

# ---- 0. Aspectos Generales ----

#Cargar paquetes
library(readxl)
library(dplyr)
library(car)

#Creamos objeto desde la base de datos, seleccionado hoja numero 2
basefvime <- read_excel("Data/Original Data/Proy_usach.xlsx", sheet = 2)

#Revisar base
names(basefvime)
summary(basefvime)
dim(basefvime)
str(basefvime)

# ---- 1. Preparar objetos para el analisis ----

#Seleccion de varibles
basefvime <- select(basefvime, Inicio, ID, titulo, area,
                  Facultad, Sector, Tipo, Comuna, Region)


#Crea objeto para  RM, eliminando los casos internacionales y de otras regiones
actoresRM <- filter(basefvime, Region == "RM")

#Crear Columna Zona, desde la categorizacion de las comunas en tres circulos concentricos
# EI: Entorno inmediato
# CC2: otras comunas dentro de cicunvalacion vespucio
# CC3: Comunas fuera de cicunvalacion vespucio

actoresRM <- mutate(actoresRM, zona = car::recode(actoresRM$Comuna,
            "'Estacion Central' = 'EI'; 'Cerrillos' = 'EI'; 
            'Lo Prado' = 'EI'; 'Quinta Normal' = 'EI';
            'Santiago'= 'EI'; 'Pedro Aguirre Cerda' = 'EI'; 
            
            
            'Cerro Navia' = 'CC2';'La Granja' = 'CC2'; 'Providencia' = 'CC2'; 
            'Conchali' = 'CC2'; 'La Cisterna' = 'CC2'; 'San Miguel' = 'CC2';'Maipu'= 'CC2';
            'Independencia' = 'CC2'; 'Lo Espejo' = 'CC2'; 'Macul' = 'CC2'; 'Maipu' ='CC2';
            'Nunoa' = 'CC2'; 'Pudahuel' = 'CC2'; 'Recoleta' = 'CC2'; 'Renca' = 'CC2';
            'San Joaquin' = 'CC2'; 'San Ramon' = 'CC2';
            
            
            'San Bernardo' = 'CC3'; 'Huechuraba' = 'CC3';
            'La Florida' = 'CC3'; 'Las Condes' = 'CC3'; 'Vitacura' = 'CC3'; 'La Pintana' = 'CC3';
            'Puente Alto' = 'CC3' ; 'El Bosque' = 'CC3';
            'Quilicura' = 'CC3'; 'La Reina' = 'CC3';
            'Penalolen' = 'CC3'; 
            
            'Isla de Maipo' = 'CC4';'Lampa' = 'CC4';'Melipilla' = 'CC4';'San Jose de Maipo' = 'CC4'"))

#Crea Columa de prioridad según indice de prioridad social

actoresRM <- mutate(actoresRM, IPS = car::recode(actoresRM$Comuna,
                                                "'La Pintana' = 'Alta'; 'Lo Espejo' = 'Alta'; 'Cerro Navia' = 'Alta' ;
                                                'San Ramon'= 'Alta';'Isla de Maipo'= 'Alta'; 'Maria Pinto' = 'Alta' ;
                                                
                                                'Conchali' = 'Media Alta';'Melipilla' = 'Media Alta';'Lo Prado' = 'Media Alta';
                                                'San Bernardo' = 'Media Alta' ; 'El Bosque' = 'Media Alta' ; 'San Jose de Maipo' = 'Media Alta';
                                                
                                                'Lampa' = 'Media Baja' ; 'Quinta Normal'= 'Media Baja' ; 'La Granja'='Media Baja' ;  
                                                'Estacion Central' = 'Media Baja' ; 'Pedro Aguirre Cerda' = 'Media Baja';
                                                'La Cisterna'= 'Media Baja'; 'Pudahuel' = 'Media Baja'; 
                                                
                                                'Cerrillos' = 'Baja' ; 'Puente Alto' = 'Baja'; 'La Florida' = 'Baja';
                                                'Maipu' = 'Baja';'Huechuraba' = 'Baja'; 'Santiago' = 'Baja'; 'Quilicura'= 'Baja';
                                                'San Miguel' = 'Baja';
                                                'Nunoa' = 'Sin Prioridad';'Providencia'  = 'Sin Prioridad' ; 'Las Condes'  = 'Sin Prioridad';
                                                'Vitacura' = 'Sin Prioridad'"))


# Transformamos los vectores zona e IPS en factor, las etiquetas se ordenan segun el alfabeto

actoresRM <- mutate (actoresRM, Area = as.factor(actoresRM$area)) 

actoresRM <- mutate (actoresRM, Comuna = as.factor(actoresRM$Comuna)) 

actoresRM <- mutate (actoresRM, Sector = as.factor(actoresRM$Sector)) 

actoresRM <- mutate (actoresRM, Subsector = as.factor(actoresRM$Tipo))

actoresRM <- mutate(actoresRM, Circunvalacion = factor(actoresRM$zona,
                                                    labels = c("Circunvalación Proxima","Circunvalación Media","Fuerda de la Ciudad","Entorno inmediato"))) 

actoresRM <- mutate(actoresRM, IPS = factor(actoresRM$IPS,
                                                    labels = c("Alta","Baja","Media Alta", "Media Baja", "Sin Prioridad"))) 


# Seleccionamos las variables Definitivas

actoresRM <-select(actoresRM, Inicio, ID, titulo, Area,
                   Facultad, Sector, Subsector, Comuna, Circunvalacion, IPS,zona)


#Guardar base de datos con los casos RM
saveRDS(actoresRM, file = "Data/Analysis Data/actoresrm.rds" ,compress = F )


# ---- 2. Hoja con información territorial
Comunas <- read_excel("Data/Original Data/Proy_usach.xlsx", sheet = 3)

saveRDS(Comunas, file = "Data/Analysis Data/Comunas.rds" ,compress = F )

# Limpiar entorno de trabajo
rm(list=ls())

