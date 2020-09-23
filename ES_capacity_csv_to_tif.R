

getwd() # check  wd


library(readr)
library(dplyr)
library(tidyverse)
# for spatial data
library(raster)
library(rgdal)
library(sp)
library(sf)

#single file test
# df <-read.csv("R_SJ_THH/Output/Ready/cultural_bathing_sites_BEAT_area_as_weight_coastal.csv", sep=";")
# dfgrid<- df

folderin<-"R_SJ_THH/Output" # change accordingly
folderout<-"GIS" #change accordingly


filelistin <- list.files(folderin)
filelistout <- list.files(folderout)

#filelist<-list.files(path=folderin,pattern="*.csv")
#filelist_out<-list.files(path=folderout,pattern="*.tif")


#projection of tif file
crsEEA <- CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs") #original proj in EEA grid



# the following line excludes files already found in the output folder
# if you want to rerun a particular file, delete it from the output folder
filelist<-filelistin[!sub('\\..[^\\.]*$', '', filelistin) %in% sub('\\..[^\\.]*$', '', filelistout)] 

#read in all results and create as tif files

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
