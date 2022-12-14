#Promedios mensuales climatologicos

#Estableciendo directorio de trabajo
setwd("C:/Users/USUARIO/Downloads")
#Librer?as a usar
library(raster)  #Manejo de data raster 
library(fields)  #Mapeo simple de matrices
library(ncdf4)   #Manejo de data grillada
library(maps)    #Mapa continental

#Importaci?n de data TSM mensual de 1991 a 2020
nc <- brick('sst(91-20).nc')
nc#Separaci?n de capas por fechas

#Obtenci?n de fechas
dates <- getZ(nc) 
dates

#Obtenci?n de solo meses
months <-  as.integer(format(as.Date(dates), "%m"))
months

#Promedio mensual climatol?gico
s <- stackApply(nc, months, fun=mean)

#Cambio de nombre de capas a meses
names(s) <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")


#Esta es una forma simple de ploteo pero no permite agregar mas funcionalidades, por ello convertiremos el raster a netcdf

#Conversi?n raster a netcdf
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

#Inversi?n de latitudes
lat <- rev(lat)  # de 90 -90 a -90 90

#Inversi?n tambi?n de latitudes en el arreglo
temp <- temp[, ncol(temp):1,] 

#Eligiendo la primera fecha - Enero
temp11 <- temp[ , , 1]

#Estableciendo escala de colores
col=designer.colors(100, c( "blue","lightblue","white","orange","red") )

#Grafica Climatolog?a Enero
image.plot(lon,lat,temp11,xlab="Longitud (?)",ylab="Latitud (?)",nlevel=100,col=col,main= paste0("Climatolog?a TSM (?C) Enero (1991-2020)" ))
maps::map("world2", add = TRUE)
contour(lon,lat,temp11,add=T,nlevels =20 ,col="gray30",maxpixels=100000)

#De esta forma se podr?a seguir generando las graficas, pero para agilizar el proceso se hara uso de una funcion

#Funci?n para la creaci?n de gr?ficas climatologicas por mes
tsm_gr <- function (i){
  temp11 <- temp[ , , i] 
  mes <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio"  ,"Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
  
  col=designer.colors(100, c( "blue","lightblue","white","orange","red") )
  
  image.plot(lon,lat,temp11,xlab="Longitud (?)",ylab="Latitud (?)",nlevel=100,col=col,main= paste0("Climatolog?a TSM (?C) ",mes[i], " (1991-2020)" ))
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

#Importaci?n de data TSM mensual de 1991 a 2020
nc <- brick('sst(91-20).nc')
nc#Separaci?n de capas por fechas

#Obtenci?n de fechas
dates <- getZ(nc) 
dates

#Obtenci?n de solo meses
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
names(s) <- c("verano","Oto?o","Invierno","Primavera")

#Imagen simple de la primea estacion
image(s[[1]]-273,main="TSM Climatog?a Verano (1991-2020)",xlab="Longitud",ylab="Latitud", maxpixels=100000)

#Conversi?n raster a netcdf
writeRaster(s-273, "tsm(91-20)_est_brick.nc", overwrite=TRUE, format="CDF",     varname="tsm", varunit="degC", 
            longname="Temperature -- raster stack to netCDF", xname="Lon",   yname="Lat", zname="Time (Month)")

#Importaci?n de netcdf estacional recien creado 
temp.nc <- nc_open("tsm(91-20)_est_brick.nc")
temp.nc

#Obteniendo las variables del netcdf
temp <- ncvar_get(temp.nc,"tsm")
lat <- ncvar_get(temp.nc, "Lat")
lon <- ncvar_get(temp.nc, "Lon")
time <- ncvar_get(temp.nc, "Time (Month)")

#Inversi?n de latitudes
lat <- rev(lat)  # de 90 -90 a -90 90

#Inversi?n tambi?n de latitudes en el arreglo
temp <- temp[, ncol(temp):1,] 

#Eligiendo la primera fecha - Enero
temp11 <- temp[ , , 1]

#Estableciendo escala de colores
col=designer.colors(100, c( "blue","lightblue","white","orange","red") )

#Grafica Climatolog?a Verano
image.plot(lon,lat,temp11,xlab="Longitud (?)",ylab="Latitud (?)",nlevel=100,col=col,main= paste0("Climatolog?a TSM (?C) Verano (1991-2020)" ))
maps::map("world2", add = TRUE)
contour(lon,lat,temp11,add=T,nlevels =20 ,col="gray30",maxpixels=100000)

#Funci?n para la creaci?n de gr?ficas climatol?gicas por estaci?n
tsm_g_est <- function (i){
  temp11 <- temp[ , , i] 
  mes <- c("verano","Oto?o","Invierno","Primavera")
  
  col=designer.colors(100, c( "blue","lightblue","white","orange","red") )
  
  image.plot(lon,lat,temp11,xlab="Longitud (?)",ylab="Latitud (?)",nlevel=100,col=col,main= paste0("Climatolog?a TSM (?C) ",mes[i], " (1991-2020)" ))
  maps::map("world2", add = TRUE)
  contour(lon,lat,temp11,add=T,nlevels =20 ,col="gray30",maxpixels=100000)
}

#Probando funci?n Invierno Climatol?gico
tsm_g_est(3)

#Vector de estaciones a iterar
mes <- c("verano","Oto?o","Invierno","Primavera")

#Bucle para el guardado de graficas
for (i in 1:4){
  png(paste("TSM_",mes[i],".png",sep=""), width = 10, height = 6, units = 'in', res = 700)
  tsm_g_est(i)
  # Make plot
  dev.off()
}