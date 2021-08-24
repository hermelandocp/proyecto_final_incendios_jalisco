library(tidyverse)
library(sp)
library(sf)
library(raster)
library(aspace)##no jalo

library(spatstat)
library(lubridate)
library(rgdal)


#####################abril 20

datos_abr3 <- 
  read_csv("C:/Users/CIG/Desktop/geoinfo/abril_inc17.csv") %>% #ojo con la ruta en la que se encuentra el archivo.
  glimpse()


inc_abril21 <-
  st_as_sf(datos_abr3, 
           coords = c("longitud", "latitud"), 
           crs = 4326, 
           na.fail = FALSE)


plot(inc_abril21$geometry)



###Ventana

camp_abr23  <- st_read("C:/Users/CIG/Desktop/sigpp/campeche/jalisco.shp")

###verificar SRC

crs(camp_abr23 )

##reproyección de ambas capas
camp_abr3_itrf <- st_transform(camp_abr23, crs = 3857)
wab1  <- as.owin(camp_abr3_itrf)


inc_abr3_shapet_itrf <- 
  st_transform(inc_abril21, crs = 3857)



####
#union de las dos capas
inc_abr3_shape_itrfcoord <- st_coordinates(inc_abr3_shapet_itrf)
inc_abr3_shape_itrf_ppp <- ppp(x = inc_abr3_shape_itrfcoord [,1], y = inc_abr3_shape_itrfcoord [,2], window = wab1)



plot(inc_abr3_shape_itrf_ppp, main="Incendios Campeche año 2020")




##Prueba de chi cuadrada
quadrat.test(plot(inc_abr3_shape_itrf_ppp, nx = 100, ny = 100)
             
             
             ##Prueba Función k de ripley
             
k_abr3 <- envelope(inc_abr3_shape_itrf_ppp, fun=Kest, nrank=2, nsim=99, correction = "none")
             
             
plot(k_abr3, main="K de Ripley para Incendios en Campeche en el mes de abril del año 2020")
             
             
             ##K de ripley corregido
             inc_maypppcorr <- envelope(inc_may_shape_itrf_ppp, fun=Kest, nrank=2, nsim=99, correction = "best")
             
             
             ##no los corri             
             plot(inc_maypppcorr, main="K de Ripley para Incendios en Campeche en el mes de mayo del año 2011")
             
             
             #############
             G_inc_abr2 <- envelope(inc_abr2_shape_itrf_ppp, fun=Gest, nrank=2, nsim=99)
             
             
             plot(G_inc_abr2, main="G obs vs G teo para Incendios en Campeche en el mes de abril del 2020")
             
             
             ###############densidad
             
global_density_abril21 <- length(inc_abr3_shapet_itrf$delito)/sum(st_area(camp_abr3_itrf))
global_density_abril21
             
             
             
             
incet_cuadrante_abr21 <- quadratcount(inc_abr3_shape_itrf_ppp, nx = 100, ny = 100)
plot(incet_cuadrante_abr21)
             
             
             
incendios_kd_100_abr21 <- density.ppp(inc_abr3_shape_itrf_ppp, sigma = 13000) %>% 
plot(main = "Densidad de Incendios en Jalisco en el mes de abri del año 2017")
             
             
inc_abr203 <- density.ppp(inc_abr3_shape_itrf_ppp, sigma = 13000, edge = T) %>%
raster()
plot(inc_abr203)


crs(inc_abr203) <- "+proj=utm +zone=13 +ellps=GRS80 +units=m +no_defs"


writeRaster(inc_abr203, "inc_abr203.tiff",
            format="GTiff",
            overwrite=TRUE,
            NAflag=-9999)



