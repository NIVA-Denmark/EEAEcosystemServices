
###   Calculating ES capacities for Europe ###
### Script for file preparation ###

# Created by Susanna Jernberg 13.8.2020
# Last updated 9.9.2020

# This file is for preparation of files that are used for all services

# empty the R history
rm(list=ls())

# Check and set working directory and download packages
setwd("U:/Documents/7. ETC/Data/")

library(tidyverse)
library(plyr)

# ***************************************************************************
# *********   Read files in ***********


### ECOSYSTEM LAYERS

# All ecosystem layers and their area per cent (broad habitats) or abundance (other components) per EEA grids in all marine areas
baltic<-read.csv("Input/Preparation input/Ecosystem layers/Ecosystem_components_Baltic_grid.csv", header=T, sep=";" )
blacksea<-read.csv("Input/Preparation input/Ecosystem layers/Ecosystem_components_Black_grid.csv", header=T, sep=";" )
mediterranean<-read.csv("Input/Preparation input/Ecosystem layers/Ecosystem_components_Med_grid.csv", header=T, sep=";" )
atlantic<-read.csv("Input/Preparation input/Ecosystem layers/Ecosystem_components_Atlantic_grid.csv", header=T, sep=";" )

## select only the columns needed (e.g drop duplicate columns and other unnecessary columns). Reorder to same format
baltic<-select(baltic, CellCode,EofOrigin,NofOrigin,Baleen_wha,breed_bird, Coastal, Offshore,deeptooth, FishSpec,Seal, 
               Smalltooth, Turtles, Saltmarsh,Seagrass, Seamounts,  Cold_coral, Abyssal, CircaCS, CircaMS, CircaMud, 
               CiOrOffMd, CircaRBR,CircaSand, InfraCS, InfraMS,InfraMud, InfraRBR, InfraSand, Na, OffCiCS, OffCiMS, 
               OffCiMud, OffCiRBR, OffCiSand,Bathy)

blacksea<-select(blacksea, CellCode,EofOrigin,NofOrigin,Baleen_wha,breed_bird, Coastal, Offshore,deeptooth, FishSpec,Seal, 
               Smalltooth, Turtles, Saltmarsh,Seagrass, Seamounts,  Cold_coral, Abyssal, CircaCS, CircaMS, CircaMud, 
               CiOrOffMd, CircaRBR,CircaSand, InfraCS, InfraMS,InfraMud, InfraRBR, InfraSand, Na, OffCiCS, OffCiMS, 
               OffCiMud, OffCiRBR, OffCiSand,Bathy)

mediterranean<-select(mediterranean, CellCode,EofOrigin,NofOrigin,Baleen_wha,breed_bird, Coastal, Offshore,deeptooth, FishSpec,Seal, 
               Smalltooth, Turtles, Saltmarsh,Seagrass, Seamounts,  Cold_coral, Abyssal, CircaCS, CircaMS, CircaMud, 
               CiOrOffMd, CircaRBR,CircaSand, InfraCS, InfraMS,InfraMud, InfraRBR, InfraSand, Na, OffCiCS, OffCiMS, 
               OffCiMud, OffCiRBR, OffCiSand,Bathy)

atlantic<-select(atlantic, CellCode,EofOrigin,NofOrigin,Baleen_wha,breed_bird, Coastal, Offshore,deeptooth, FishSpec,Seal, 
               Smalltooth, Turtles, Saltmarsh,Seagrass, Seamounts,  Cold_coral, Abyssal, CircaCS, CircaMS, CircaMud, 
               CiOrOffMd, CircaRBR,CircaSand, InfraCS, InfraMS,InfraMud, InfraRBR, InfraSand, Na, OffCiCS, OffCiMS, 
               OffCiMud, OffCiRBR, OffCiSand,Bathy)

# combine to one data and remove the originals
habitats<-rbind(baltic,blacksea,mediterranean,atlantic)
rm(baltic,blacksea,mediterranean,atlantic)


### EEA CELLCODES 

#BEAT cells vs. 10 km
cellcodesBEAT<-read.csv("Input/Preparation input/BEATgrid_in_10km.csv", header=T, sep=";" )
cellcodesBEAT<-cellcodesBEAT %>% select(-EEA_SubReg)

# 100 km vs 10km -> THESE are not needed 
#cellcodes100<-read.csv("Input/Preparation input/EEA10km_EEA100km.csv", header=T, sep=";" )
#cellcodes100<-cellcodes100 %>% select(-OBJECTID)
#colnames(cellcodes100)<-c("CellCode","EofOrigin","NofOrigin","GRIDCODE","EofOrigi_100","NofOrigi_100")


### BEAT RESULTS

baltic_b<-read.csv("Input/Preparation input/BEAT results from EUROPE (EIONET)/BEAT_PLUS_Export_Baltic & North Seas.csv", header=T, sep=";" )
blacksea_b<-read.csv("Input/Preparation input/BEAT results from EUROPE (EIONET)/BEAT_PLUS_Export_Black Sea.csv", header=T, sep=";" )
mediterranean_b<-read.csv("Input/Preparation input/BEAT results from EUROPE (EIONET)/BEAT_PLUS_Export_Mediterranean Sea.csv", header=T, sep=";" )
Natlantic_b<-read.csv("Input/Preparation input/BEAT results from EUROPE (EIONET)/BEAT_PLUS_Export_North Atlantic Ocean.csv", header=T, sep=";" )
Eatlantic_b<-read.csv("Input/Preparation input/BEAT results from EUROPE (EIONET)/BEAT_PLUS_Export_North-east Atlantic Ocean.csv", header=T, sep=";" )

# select needed columns
baltic_b<-select(baltic_b,SpatialAssessmentUnit,Indicator,BQR)
blacksea_b<-select(blacksea_b,SpatialAssessmentUnit,Indicator,BQR)
mediterranean_b<-select(mediterranean_b,SpatialAssessmentUnit,Indicator,BQR)
Natlantic_b<-select(Natlantic_b,SpatialAssessmentUnit,Indicator,BQR)
Eatlantic_b<-select(Eatlantic_b,SpatialAssessmentUnit,Indicator,BQR)

#Combine BEAT results to one dataframe and remove the original tables
beat_data<-rbind(baltic_b,blacksea_b,mediterranean_b,Natlantic_b,Eatlantic_b)
rm(baltic_b,blacksea_b,mediterranean_b,Natlantic_b,Eatlantic_b)


### GROUPING OF BEAT INDICATORS

# Indicator data from the service template
BEAT_groups<-read.csv("Input/Preparation input/BEAT_Indicators_in_groups.csv", header=T, sep=";" )
BEAT_groups<-select(BEAT_groups, Biotic_group, Name.of.Indicator)




# ***************************************************************************
# *********  Combine input files  ****************

#Combine BEAT grids with EEA 10 km cells with broad scale habitats
habs_in_grids<-merge(habitats,cellcodesBEAT, by= "CellCode",all.x=TRUE, all.y=TRUE) %>% 
  select(-EofOrigin.y,-NofOrigin.y)

# change the table so that the benthic habitats are in one column and the area percent in other
#Remove rows that are not needed (e.g. benthic percent is 0).
habs_in_grids_mod<-gather(habs_in_grids, key="benthic_hab",value="area_percent", Abyssal:Bathy, 
                     factor_key=TRUE) 

%>% dplyr::filter(area_percent >0) #we are only interested in these cells


# Combine BEAT indicator groups with beat data
beat_inds<-full_join(beat_data,BEAT_groups, by=c("Indicator"="Name.of.Indicator"))


# create function to calculate integrated (e.g. mean) BQR. Takes around 5 min.
integrate_beat=function(input){ 
  output<-input[1,c("SpatialAssessmentUnit", "Biotic_group")]
  
  output$integ_beat<-mean(input$BQR, na.rm=TRUE)
  output$number_indicators<-length(input$Indicator)-sum(is.na(input$BQR)) # calculate number of distinct indicators. 
                                                                 
  return(output)
}

#apply function
integ_beat<-ddply(beat_inds, .(SpatialAssessmentUnit,Biotic_group),integrate_beat)


#### write tables
write.table(beat_inds, sep=";", file="Input/Preparation output/BEAT_indicators_in_grids.csv", row.names=F)
write.table(habs_in_grids_mod, sep=";", file="Input/Preparation output/Broad_habitats_in_grids.csv", row.names=F)
write.table(integ_beat, sep=";", file="Input/Preparation output/Integrated_BEAT_results.csv", row.names=F)


