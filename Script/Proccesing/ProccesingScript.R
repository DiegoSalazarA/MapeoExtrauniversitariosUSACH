#Codigo de trabajo "Ciencia Abierta y Software Libre"
#Estudiante: Diego Salazar Alvarado



# ---- 0. Aspectos Generales ----



#Cargar paquetes
library(readxl)
library(dplyr)
library(car)


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
            'Santiago'= 'EI'; 'Pedro Aguirre Cerda' = 'EI'; 
            
            'Nunoa' = 'CC2'; 'Cerro Navia' = 'CC2';'La Granja' = 'CC2'; 'Providencia' = 'CC2'; 
            'Conchali' = 'CC2'; 'La Cisterna' = 'CC2'; 'San Miguel' = 'CC2';
            
            'San Bernardo' = 'CC3'; 'Isla de Maipo' = 'CC3';'Huechuraba' = 'CC3';
            'La Florida' = 'CC3'; 'Las Condes' = 'CC3'; 'Vitacura' = 'CC3'; 'La Pintana' = 'CC3';
            'Puente Alto' = 'CC3' ; 'San Jose de Maipo' = 'CC3'; 'El Bosque' = 'CC3';
            'Lampa' = 'CC3';'Melipilla' = 'CC3'; 'Quilicura' = 'CC3'"))

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
                                                 

# Transformamos los vestores zona e IPS en factor, las etiquetas se ordenan segun el alfabeto
actoresRM <- mutate(actoresRM, zona_factor = factor(actoresRM$zona,
                                                    labels = c("CC2","CC3","EI"))) 

actoresRM <- mutate(actoresRM, IPS_factor = factor(actoresRM$IPS,
                                                    labels = c("Alta","Baja","Media Alta", "Media Baja", "Sin Prioridad"))) 


#Guardar base de datos con los casos RM
saveRDS(actoresRM, file = "Data/Analysis Data/actoresrm.rds" ,compress = F )
