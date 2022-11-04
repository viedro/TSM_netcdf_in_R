#Promedios mensuales climatologicos

#Estableciendo directorio de trabajo
setwd("C:/Users/USUARIO/Downloads")
#Librerías a usar
library(raster)  #Manejo de data raster 
library(fields)  #Mapeo simple de matrices
library(ncdf4)   #Manejo de data grillada
library(maps)    #Mapa continental

#Importación de data TSM mensual de 1991 a 2020
nc <- brick('sst(91-20).nc')
nc#Separación de capas por fechas

#Obtención de fechas
dates <- getZ(nc) 
dates

#Obtención de solo meses
months <-  as.integer(format(as.Date(dates), "%m"))
months

#Promedio mensual climatológico
s <- stackApply(nc, months, fun=mean)

#Cambio de nombre de capas a meses
names(s) <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")


#Esta es una forma simple de ploteo pero no permite agregar mas funcionalidades, por ello convertiremos el raster a netcdf

#Conversión raster a netcdf
writeRaster(s-273, "tsm(91-20)_brick.nc", overwrite=TRUE, format="CDF",     varname="tsm", varunit="degC", 
            longname="Temperature -- raster stack to netCDF", xname="Lon",   yname="Lat", zname="Time (Month)")

#Abriendo el archivo netcdf recien creado
temp.nc <- nc_open("tsm(91-20)_brick.nc")
temp.nc

#Obteniendo las varriables del netcdf
temp <- ncvar_get(temp.nc,"tsm")
lat <- ncvar_get(temp.nc, "Lat")
lon <- ncvar_get(temp.nc, "Lon")
time <- ncvar_get(temp.nc, "Time (Month)")

#Inversión de latitudes
lat <- rev(lat)  # de 90 -90 a -90 90

#Inversión también de latitudes en el arreglo
temp <- temp[, ncol(temp):1,] 

#Eligiendo la primera fecha - Enero
temp11 <- temp[ , , 1]

#Estableciendo escala de colores
col=designer.colors(100, c( "blue","lightblue","white","orange","red") )

#Grafica Climatología Enero
image.plot(lon,lat,temp11,xlab="Longitud (°)",ylab="Latitud (°)",nlevel=100,col=col,main= paste0("Climatología TSM (ºC) Enero (1991-2020)" ))
maps::map("world2", add = TRUE)
contour(lon,lat,temp11,add=T,nlevels =20 ,col="gray30",maxpixels=100000)

#De esta forma se podría seguir generando las graficas, pero para agilizar el proceso se hara uso de una funcion

#Función para la creación de gráficas climatologicas por mes
tsm_gr <- function (i){
  temp11 <- temp[ , , i] 
  mes <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio"  ,"Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
  
  col=designer.colors(100, c( "blue","lightblue","white","orange","red") )
  
  image.plot(lon,lat,temp11,xlab="Longitud (°)",ylab="Latitud (°)",nlevel=100,col=col,main= paste0("Climatología TSM (ºC) ",mes[i], " (1991-2020)" ))
  maps::map("world2", add = TRUE)
  contour(lon,lat,temp11,add=T,nlevels =20 ,col="gray30",maxpixels=100000)
}

#Ejemplo de uso Diciembre
tsm_gr(12)

#Vector de meses a iterar
mes <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio"  ,"Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")

#Bucle para el guardado de graficas
for (i in 1:12){
  png(paste("TSM_",mes[i],".png",sep=""), width = 10, height = 6, units = 'in', res = 700)
  tsm_gr(i)
  # Make plot
  dev.off()
}


##################################################

#Promedios estacionales climatologicos

#Importación de data TSM mensual de 1991 a 2020
nc <- brick('sst(91-20).nc')
nc#Separación de capas por fechas

#Obtención de fechas
dates <- getZ(nc) 
dates

#Obtención de solo meses
months <-  as.integer(format(as.Date(dates), "%m"))
months

#Bucle para obtener climatologias estacionales
for (i in 1:length(months)){
  if(months[i] == 12|months[i]== 1 | months[i]==2){
    months[i]= 1
  }
  if(months[i] == 3|months[i]== 4 | months[i]==5){
    months[i]= 2
  }
  if(months[i] == 6|months[i]== 7 | months[i]==8){
    months[i]= 3
  }
  if(months[i] == 9|months[i]== 10 | months[i]==11){
    months[i]= 4
  }
}
months

#Prormedios estacionales
s <- stackApply(nc, months, fun=mean)

#Cambio de nombre de fechas
names(s) <- c("verano","Otoño","Invierno","Primavera")

#Imagen simple de la primea estacion
image(s[[1]]-273,main="TSM Climatogía Verano (1991-2020)",xlab="Longitud",ylab="Latitud", maxpixels=100000)

#Conversión raster a netcdf
writeRaster(s-273, "tsm(91-20)_est_brick.nc", overwrite=TRUE, format="CDF",     varname="tsm", varunit="degC", 
            longname="Temperature -- raster stack to netCDF", xname="Lon",   yname="Lat", zname="Time (Month)")

#Importación de netcdf estacional recien creado 
temp.nc <- nc_open("tsm(91-20)_est_brick.nc")
temp.nc

#Obteniendo las variables del netcdf
temp <- ncvar_get(temp.nc,"tsm")
lat <- ncvar_get(temp.nc, "Lat")
lon <- ncvar_get(temp.nc, "Lon")
time <- ncvar_get(temp.nc, "Time (Month)")

#Inversión de latitudes
lat <- rev(lat)  # de 90 -90 a -90 90

#Inversión también de latitudes en el arreglo
temp <- temp[, ncol(temp):1,] 

#Eligiendo la primera fecha - Enero
temp11 <- temp[ , , 1]

#Estableciendo escala de colores
col=designer.colors(100, c( "blue","lightblue","white","orange","red") )

#Grafica Climatología Verano
image.plot(lon,lat,temp11,xlab="Longitud (°)",ylab="Latitud (°)",nlevel=100,col=col,main= paste0("Climatología TSM (ºC) Verano (1991-2020)" ))
maps::map("world2", add = TRUE)
contour(lon,lat,temp11,add=T,nlevels =20 ,col="gray30",maxpixels=100000)

#Función para la creación de gráficas climatológicas por estación
tsm_g_est <- function (i){
  temp11 <- temp[ , , i] 
  mes <- c("verano","Otoño","Invierno","Primavera")
  
  col=designer.colors(100, c( "blue","lightblue","white","orange","red") )
  
  image.plot(lon,lat,temp11,xlab="Longitud (°)",ylab="Latitud (°)",nlevel=100,col=col,main= paste0("Climatología TSM (ºC) ",mes[i], " (1991-2020)" ))
  maps::map("world2", add = TRUE)
  contour(lon,lat,temp11,add=T,nlevels =20 ,col="gray30",maxpixels=100000)
}

#Probando función Invierno Climatológico
tsm_g_est(3)

#Vector de estaciones a iterar
mes <- c("verano","Otoño","Invierno","Primavera")

#Bucle para el guardado de graficas
for (i in 1:4){
  png(paste("TSM_",mes[i],".png",sep=""), width = 10, height = 6, units = 'in', res = 700)
  tsm_g_est(i)
  # Make plot
  dev.off()
}