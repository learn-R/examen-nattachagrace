###############################
##Examen
#Universidad Alberto Hurtado
#Curso: Opt. Análisis de datos estadísticos en R
#Profesora:Valentina Andrade
#Ayudantes:Dafne Jaime y Nicolás Godoy
#22 de noviembre,2021
###############################


#1. Cargar paquetes

pacman::p_load(tidyverse, #colección de paquetes, del cuál utilizaremos dplyr y haven
               magrittr,  #procesamiento de datos
               sjPlot, #visualización
               sjmisc, #explorar datos
               dplyr,  #nos permite seleccionar variables de un set de datos
               haven)  #cargar y exportar bases de datos en formatos .sav y .dta

#2. Cargar base de datos

BP <- read_dta("input/data/base-personas-viii-epf-(stata).dta")

names(BP) #Revisar las variables que componen la base de datos

#3. Cambiar nombres de variables y recortar base de datos

##SEXO: Señala el sexo de cada uno de los miembros del hogar, hombre o mujer.
##GASTOT_HD: Gasto total por hogar imputado por hot-deck (sin arriendo imputado). Se le cambia nombre a "gastos"
##ING_DISP_HOG_HD: Ingreso disponible del hogar imputado por hot-deck (sin arriendo imputado). Se le cambia nombre a "ingreso_liquido"
##EDUNIVEL: Identifica el nivel educacional más alto alcanzado, para cada miembro del hogar
##VP: Identifica el tipo de vivienda que habitan los miembros del hogar
##TVP: Identifica el tipo de tenencia de la vivienda principal que habitan los miembros del hogar.
##ZONA: Zona geográfica a la que pertenece el hogar, separa entre "Gran Santiago" y "Resto de las capitales regionales"
##NPERSONAS: Número de personas en el hogar
##CAE: Indica si las personas del hogar están "Ocupados", "Desocupados" o "Inactivos" 


datos_proc <- BP%>% 
  select(SEXO,
         EDAD,
         gastos=GASTOT_HD, 
         ingreso_liquido=ING_DISP_HOG_HD, 
         EDUNIVEL,
         vivienda_tipo=VP,
         vivienda_tenencia=TVP,
         ZONA, 
         NPERSONAS,
         FE,
         VARSTRAT, 
         VARUNIT)


##4. Recodificar variable

### Recodificar variable EDUNIVEL, en tramos de escolaridad

datos_proc <- datos_proc %>% 
  mutate(N_educ = car::recode(EDUNIVEL, c("c(1,2)=1;
                                          c(3,4,5,6)=2;
                                          c(7,8)=3;
                                          c(9,10,11,12)=4;
                                          c(13,14)=5;
                                          c(15,15,17)=6;
                                          c(-88,-99)=NA"),
                              levels = c(1,2,3,4,5,6)))

### Recodificar variable EDAD

datos_proc <- datos_proc%>%
  mutate(edad_tramos = case_when(EDAD <= 18  ~  "Joven",
                                 EDAD >= 19 & EDAD <=64 ~ "Adulto",
                                 EDAD > 65 ~ "Adulto mayor",
                                 TRUE ~ NA_character_))

### Predictor categorico: edad_tramos

datos_proc$edad_tramos <-  forcats::as_factor(datos_proc$edad_tramos)

### Recodificar variable sexo

datos_proc<-datos_proc %>% 
  mutate(sexo_cat = case_when(SEXO == 1 ~ "Hombre",
                              SEXO ==2 ~ "Mujer",
                              TRUE ~ NA_character_))
### Predictor categorico: sexo_cat

datos_proc$sexo_cat <-  forcats::as_factor(datos_proc$sexo_cat)

### Recodificar zona

datos_proc<-datos_proc %>% 
  mutate(zona_cat = case_when(ZONA == 1 ~ "Gran Santiago",
                              ZONA ==2 ~ "Otras capitales regionales",
                              TRUE ~ NA_character_))

### Predictor categorico: zona_cat

datos_proc$zona_cat <-  forcats::as_factor(datos_proc$zona_cat)


##5. Guardar base de datos recortada

saveRDS(datos_proc, file = "output/data/datos_proc.rds")

