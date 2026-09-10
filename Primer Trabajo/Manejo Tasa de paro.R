################################################################################################################################################################
#########################                                                                                                      #################################
#                                                           UNIVERSIDAD DEL QUINDIO                                                                            #
#                                                                  ECONOMIA                                                                                    #
#                                                               MACROECONOMIA I                                                                                #
#########################                                                                                                      #################################
################################################################################################################################################################

# By:Juan Diego Cardona Betancourt
# Juand.cardonab@uqvirtual.edu.co
# +57 3113553717

#Code to clean Git 
system("git add -u")

print("Trabajo con la base de datos GEIH año 2025")

#
getwd()
options(scipen = 100,digits = 4)
rm(list = ls())
version

#librerias o paquetes
library("skimr")
library("readxl")
library("stringr")
library("stringi")
library("plyr")
library("haven")
library("tidyverse")
library("readr")

#Directorio-----
##https://microdatos.dane.gov.co/index.php/catalog/853
GEIH25= "C:/Users/Juan Diego/Downloads/GEIH 2025"

#ENERO

ENERO = merge(
  x = read_delim( file = paste0( GEIH25 ,
                                "/Enero 2025/Características generales, seguridad social en salud y educación.CSV") , delim = ";" ,
                  locale = locale( decimal_mark = "." ) , col_names = TRUE ) ,
  y = read_delim(
    file = paste0( GEIH25 , "/Enero 2025/Ocupados.CSV") , delim = ";" ,
    locale = locale( decimal_mark = ".", encoding = "Latin1" ) , col_names = TRUE ) , 
  by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
  all.x = T , suffixes = c( "" ,".2" )
) |>  
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Enero 2025/No ocupados.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Enero 2025/Fuerza de trabajo.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Enero 2025/Otros ingresos e impuestos.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Enero 2025/Otras formas de trabajo.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> data.frame()

#FEBRERO

FEBRERO = merge(
  x = read_delim( file = paste0( GEIH25 ,
                                 "/Febrero 2025/Características generales, seguridad social en salud y educación.CSV") , delim = ";" ,
                  locale = locale( decimal_mark = "." ) , col_names = TRUE ) ,
  y = read_delim(
    file = paste0( GEIH25 , "/Febrero 2025/Ocupados.CSV") , delim = ";" ,
    locale = locale( decimal_mark = ".", encoding = "Latin1" ) , col_names = TRUE ) , 
  by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
  all.x = T , suffixes = c( "" ,".2" )
) |>  
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Febrero 2025/No ocupados.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Febrero 2025/Fuerza de trabajo.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Febrero 2025/Otros ingresos e impuestos.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Febrero 2025/Otras formas de trabajo.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> data.frame()

#MARZO

MARZO = merge(
  x = read_delim( file = paste0( GEIH25 ,
                                 "/Marzo 2025/Características generales, seguridad social en salud y educación.CSV") , delim = ";" ,
                  locale = locale( decimal_mark = "." ) , col_names = TRUE ) ,
  y = read_delim(
    file = paste0( GEIH25 , "/Marzo 2025/Ocupados.CSV") , delim = ";" ,
    locale = locale( decimal_mark = ".", encoding = "Latin1" ) , col_names = TRUE ) , 
  by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
  all.x = T , suffixes = c( "" ,".2" )
) |>  
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Marzo 2025/No ocupados.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Marzo 2025/Fuerza de trabajo.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Marzo 2025/Otros ingresos e impuestos.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Marzo 2025/Otras formas de trabajo.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> data.frame()

#ABRIL

ABRIL = merge(
  x = read_delim( file = paste0( GEIH25 ,
                                 "/Abril 2025/Características generales, seguridad social en salud y educación.CSV") , delim = ";" ,
                  locale = locale( decimal_mark = "." ) , col_names = TRUE ) ,
  y = read_delim(
    file = paste0( GEIH25 , "/Abril 2025/Ocupados.CSV") , delim = ";" ,
    locale = locale( decimal_mark = ".", encoding = "Latin1" ) , col_names = TRUE ) , 
  by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
  all.x = T , suffixes = c( "" ,".2" )
) |>  
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Abril 2025/No ocupados.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Abril 2025/Fuerza de trabajo.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Abril 2025/Otros ingresos e impuestos.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Abril 2025/Otras formas de trabajo.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> data.frame()

#MAYO

MAYO = merge(
  x = read_delim( file = paste0( GEIH25 ,
                                 "/Mayo 2025/Características generales, seguridad social en salud y educación.CSV") , delim = ";" ,
                  locale = locale( decimal_mark = "." ) , col_names = TRUE ) ,
  y = read_delim(
    file = paste0( GEIH25 , "/Mayo 2025/Ocupados.CSV") , delim = ";" ,
    locale = locale( decimal_mark = ".", encoding = "Latin1" ) , col_names = TRUE ) , 
  by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
  all.x = T , suffixes = c( "" ,".2" )
) |>  
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Mayo 2025/No ocupados.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Mayo 2025/Fuerza de trabajo.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Mayo 2025/Otros ingresos e impuestos.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Mayo 2025/Otras formas de trabajo.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> data.frame()

#JUNIO

JUNIO = merge(
  x = read_delim( file = paste0( GEIH25 ,
                                 "/Junio 2025/Características generales, seguridad social en salud y educación.CSV") , delim = ";" ,
                  locale = locale( decimal_mark = "." ) , col_names = TRUE ) ,
  y = read_delim(
    file = paste0( GEIH25 , "/Junio 2025/Ocupados.CSV") , delim = ";" ,
    locale = locale( decimal_mark = ".", encoding = "Latin1" ) , col_names = TRUE ) , 
  by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
  all.x = T , suffixes = c( "" ,".2" )
) |>  
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Junio 2025/No ocupados.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Junio 2025/Fuerza de trabajo.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Junio 2025/Otros ingresos e impuestos.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Junio 2025/Otras formas de trabajo.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> data.frame()

#JULIO

JULIO = merge(
  x = read_delim( file = paste0( GEIH25 ,
                                 "/Julio 2025/Características generales, seguridad social en salud y educación.CSV") , delim = ";" ,
                  locale = locale( decimal_mark = "." ) , col_names = TRUE ) ,
  y = read_delim(
    file = paste0( GEIH25 , "/Julio 2025/Ocupados.CSV") , delim = ";" ,
    locale = locale( decimal_mark = ".", encoding = "Latin1" ) , col_names = TRUE ) , 
  by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
  all.x = T , suffixes = c( "" ,".2" )
) |>  
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Julio 2025/No ocupados.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Julio 2025/Fuerza de trabajo.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Julio 2025/Otros ingresos e impuestos.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Julio 2025/Otras formas de trabajo.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> data.frame()

#AGOSTO

AGOSTO = merge(
  x = read_delim( file = paste0( GEIH25 ,
                                 "/Agosto 2025/Características generales, seguridad social en salud y educación.CSV") , delim = ";" ,
                  locale = locale( decimal_mark = "." ) , col_names = TRUE ) ,
  y = read_delim(
    file = paste0( GEIH25 , "/Agosto 2025/Ocupados.CSV") , delim = ";" ,
    locale = locale( decimal_mark = ".", encoding = "Latin1" ) , col_names = TRUE ) , 
  by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
  all.x = T , suffixes = c( "" ,".2" )
) |>  
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Agosto 2025/No ocupados.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Agosto 2025/Fuerza de trabajo.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Agosto 2025/Otros ingresos e impuestos.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Agosto 2025/Otras formas de trabajo.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> data.frame()

#SEPTIEMBRE

SEPTIEMBRE = merge(
  x = read_delim( file = paste0( GEIH25 ,
                                 "/Septiembre 2025/Características generales, seguridad social en salud y educación.CSV") , delim = ";" ,
                  locale = locale( decimal_mark = "." ) , col_names = TRUE ) ,
  y = read_delim(
    file = paste0( GEIH25 , "/Septiembre 2025/Ocupados.CSV") , delim = ";" ,
    locale = locale( decimal_mark = ".", encoding = "Latin1" ) , col_names = TRUE ) , 
  by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
  all.x = T , suffixes = c( "" ,".2" )
) |>  
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Septiembre 2025/No ocupados.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Septiembre 2025/Fuerza de trabajo.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Septiembre 2025/Otros ingresos e impuestos.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Septiembre 2025/Otras formas de trabajo.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> data.frame()

#OCTUBRE

OCTUBRE = merge(
  x = read_delim( file = paste0( GEIH25 ,
                                 "/Octubre 2025/Características generales, seguridad social en salud y educación.CSV") , delim = ";" ,
                  locale = locale( decimal_mark = "." ) , col_names = TRUE ) ,
  y = read_delim(
    file = paste0( GEIH25 , "/Octubre 2025/Ocupados.CSV") , delim = ";" ,
    locale = locale( decimal_mark = ".", encoding = "Latin1" ) , col_names = TRUE ) , 
  by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
  all.x = T , suffixes = c( "" ,".2" )
) |>  
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Octubre 2025/No ocupados.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Octubre 2025/Fuerza de trabajo.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Octubre 2025/Otros ingresos e impuestos.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Octubre 2025/Otras formas de trabajo.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> data.frame()

#NOVIEMBRE

NOVIEMBRE = merge(
  x = read_delim( file = paste0( GEIH25 ,
                                 "/Noviembre 2025/Características generales, seguridad social en salud y educación.CSV") , delim = ";" ,
                  locale = locale( decimal_mark = "." ) , col_names = TRUE ) ,
  y = read_delim(
    file = paste0( GEIH25 , "/Noviembre 2025/Ocupados.CSV") , delim = ";" ,
    locale = locale( decimal_mark = ".", encoding = "Latin1" ) , col_names = TRUE ) , 
  by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
  all.x = T , suffixes = c( "" ,".2" )
) |>  
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Noviembre 2025/No ocupados.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Noviembre 2025/Fuerza de trabajo.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Noviembre 2025/Otros ingresos e impuestos.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Noviembre 2025/Otras formas de trabajo.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> data.frame()

#DICIEMBRE

DICIEMBRE = merge(
  x = read_delim( file = paste0( GEIH25 ,
                                 "/Diciembre 2025/Características generales, seguridad social en salud y educación.CSV") , delim = ";" ,
                  locale = locale( decimal_mark = "." ) , col_names = TRUE ) ,
  y = read_delim(
    file = paste0( GEIH25 , "/Diciembre 2025/Ocupados.CSV") , delim = ";" ,
    locale = locale( decimal_mark = ".", encoding = "Latin1" ) , col_names = TRUE ) , 
  by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
  all.x = T , suffixes = c( "" ,".2" )
) |>  
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Diciembre 2025/No ocupados.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Diciembre 2025/Fuerza de trabajo.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Diciembre 2025/Otros ingresos e impuestos.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> 
  merge(
    x = ,
    y = read_delim(
      file = paste0( GEIH25 , "/Diciembre 2025/Otras formas de trabajo.CSV") , delim = ";" ,
      locale = locale( decimal_mark = "." ) , col_names = TRUE ) , 
    by = c( "DIRECTORIO" , "SECUENCIA_P" , "ORDEN" ) , 
    all.x = T , suffixes = c( "" ,".4" )
  ) |> data.frame()



xtabs( FEX_C18 ~ MES , data= ENERO , addNA = TRUE)



GEIH = plyr::rbind.fill(ENERO, FEBRERO , MARZO , ABRIL , MAYO , JUNIO , JULIO , AGOSTO , SEPTIEMBRE , OCTUBRE , NOVIEMBRE , DICIEMBRE)

rm(GEIH25,ENERO, FEBRERO , MARZO , ABRIL , MAYO , JUNIO , JULIO , AGOSTO , SEPTIEMBRE , OCTUBRE , NOVIEMBRE , DICIEMBRE , GEIH22 , list)

## Borramos variables repetidas----
GEIH = GEIH |> dplyr::select(-ends_with(c( ".2" , ".4", ".6" , ".8" , ".10" , ".12" ,".14" ))) |> 
  mutate( MES = as.character(substr( PERIODO , start = 5, stop = 6)))
