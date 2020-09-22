

getwd()


library(readr)
library(dplyr)
library(tidyverse)
# for spatial data
library(raster)
library(rgdal)
library(sp)
library(sf)

#single file
df <-read.csv("C:/Users/THH/OneDrive - NIVA/EEA task 1.6.2.6/EEA_ES_capacity/R_SJ_THH/Output/cultural_bathing_sites_CEA_area_as_weight_coastal.csv", sep=";")
dfgrid<- df

folderin<-"Output"
folderout<-"GIS"
filelist<-list.files(path=folderin,pattern="*.csv")

crsEEA <- CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs") #original proj in EEA grid

 for(file in filelist){

  dfgrid <- read.table(paste0(folderin,"/",file),quote='"',sep=";", header=TRUE,stringsAsFactors=T)
  
  cat(paste0(file))
  dfgrid<-dplyr::filter(dfgrid, final_capacity!="", final_capacity!="NA")
  
  #dfgrid$EofOrigin.x <- as.double(dfgrid$EofOrigin.x)
  #dfgrid$NofOrigin.x <- as.double(dfgrid$NofOrigin.x)
  
  
  dfgrid<- dplyr::rename(dfgrid, x='EofOrigin.x', y='NofOrigin.x')
  dfgrid<-dfgrid[complete.cases(dfgrid),]
  dfgrid <-dfgrid %>% dplyr::select(-CellCode)

  ras<-rasterFromXYZ(dfgrid)
  crs(ras) <- crsEEA
  projection(ras)
 # plot(ras)
  
  writeRaster(ras,file=paste0(folderout,"/",file), overwrite=F,"GTiff") #  
         
 }




dat$NAME_1 <- as.factor(dat$NAME_1)

# Define RasterLayer object
r.raster <- raster()

# Define raster extent
extent(r.raster) <- extent(dat)

# Define pixel size
res(r.raster) <- 0.1

# rasterize a factor
r <- ratify(ras)
rat <- levels(r)[[1]]
rat$landcover <- c('Pine', 'Oak', 'Meadow')
rat$code <- c(12,25,30)
levels(r) <- rat
r



ras <- rasterize(x = dat, y = r.raster, field = "NAME_1")

  setwd("GIS")
  writeRaster(ras,file=fileout, overwrite=F, "GTiff") 
  


cnames = c('EofOrigin' = 'EofOrigin.x', 'NofOrigin' = 'NofOrigin.x')
flood_capacity<-flood_capacity %>% rename(!!!cnames)


cnames = c('X' = 'x_etrs', 'Y' = 'y_etrs')
flood_capacity<-flood_capacity %>% rename(!!!cnames)