###   Calculating ES capacities for Europe ###

# Created by Susanna Jernberg 13.8.2020

# This file is for a single ecosystem services and its capacity

# empty the R history
rm(list=ls())

# Check and set working directory
setwd("U:/Documents/7. ETC/Data")

library(tidyverse)
library(plyr)
library(BBmisc)

###############

# define if the service class is coastal, offshore or both?

#service_class<-"coastal"
  
# ***************************************************************************
# *********   Read files in ***********

# Ecosystem services data 
ES_data<-read.csv("Input/Services/Flood protection/Flood_protection_template24.8.2020.csv", header=T, sep=";" ) # The filled main page of the tempalte, e.g. all the habitats and indicators listed relevant for a certain services. note: input in a format that columnnames are the first row.

# Integrated BEAT results
integ_beat<-read.csv("Input/Preparation output/Integrated_BEAT_results.csv", header=T, sep=";" )

# Cumulative effect assessment
CEA_data<-read.csv("Input/Cumulative effect assessment/EEA_CEA_Ecosystemcomponent_Sum_impact_per_cell_XY_grid.csv",header=T, sep=";" )

# Broad habitat percents per EEA cell
habs_in_grids_mod<-read.csv("Input/Preparation output/Broad_habitats_in_grids.csv", header=T, sep=";" )

# All BEAT indicators. These will be needed if indicator groups defined by experts will be used. Not yet done.
#beat_inds<-read.csv("Input/Preparation output/BEAT_indicators_in_grids.csv", header=T, sep=";" ) # BEAT indicator results for all marine areas

#own grouping of BEAT indicators. Not yet used so not needed.
#inds_own_groups<-read.csv("Input/Services/Flood protection_BEAT_indicators.csv", header=T, sep=";" ) # BEAT indicator results for all marine areas


# ***************************************************************************
# *********  Modify column names of data ***********

# change columnnames of ES_data
colnames(ES_data)<-c("broad","biotic","bcont","species","scont","speciesind","broadind","bioticind","response","numeric" )

### Change the names of broad habitats to same as in other datasets. The correct classification needs still to be checked.
# "CircaCS"   "CircaMS"   "CircaMud"  "CircaRBR"  "CircaSand" are not included now. (and what layer is "CiOrOffMd" in habitat layers..?)
levels(ES_data$broad)[levels(ES_data$broad) == "Variable salinity water"]  <- "Coastal"
levels(ES_data$broad)[levels(ES_data$broad) == "Coastal Waters"]  <- "Coastal"
levels(ES_data$broad)[levels(ES_data$broad) == "Shelf Waters"]  <- "Offshore"
levels(ES_data$broad)[levels(ES_data$broad) == "Oceanic Waters"]  <- "Offshore"
levels(ES_data$broad)[levels(ES_data$broad) == "Ice-associated Habitats"]  <- "Ice" #although there is no habitat layer..
levels(ES_data$broad)[levels(ES_data$broad) == "Littoral Rock and Biogenic Reef"]  <- "L" # check these later
levels(ES_data$broad)[levels(ES_data$broad) == "Littoral Sediment"]  <- "L" #check these  later
levels(ES_data$broad)[levels(ES_data$broad) == "Shallow Sublittoral Rock and Biogenic Reef"]  <- "InfraRBR"
levels(ES_data$broad)[levels(ES_data$broad) == "Shallow Sublittoral Sand"]  <- "InfraSand"
levels(ES_data$broad)[levels(ES_data$broad) == "Shallow Sublittoral Coarse sediment"]  <- "InfraCS"
levels(ES_data$broad)[levels(ES_data$broad) == "Shallow Sublittoral Mixed Sediment"]  <- "InfraMS"
levels(ES_data$broad)[levels(ES_data$broad) == "Shallow Sublittoral Mud"]  <- "InfraMud"
levels(ES_data$broad)[levels(ES_data$broad) == "Shelf Sublittoral Rock and Biogenic Reef"]  <- "OffCiRBR"
levels(ES_data$broad)[levels(ES_data$broad) == "Shelf Sublittoral Coarse Sediment"]  <- "OffCiCS"
levels(ES_data$broad)[levels(ES_data$broad) == "Shelf Sublittoral Sand"]  <- "OffCiSand"
levels(ES_data$broad)[levels(ES_data$broad) == "Shelf Sublittoral Mud"]  <- "OffCiMud"
levels(ES_data$broad)[levels(ES_data$broad) == "Shelf Sublittoral Mixed Sediment"]  <- "OffCiMS"
levels(ES_data$broad)[levels(ES_data$broad) == "Upper Bathyal Rock and Biogenic Reef"]  <- "Bathy"
levels(ES_data$broad)[levels(ES_data$broad) == "Upper Bathyal Sediment"]  <- "Bathy"
levels(ES_data$broad)[levels(ES_data$broad) == "Lower Bathyal Rock and Biogenic Reef"]  <- "Bathy"
levels(ES_data$broad)[levels(ES_data$broad) == "Lower Bathyal Sediment"]  <- "Bathy"
levels(ES_data$broad)[levels(ES_data$broad) == "Abyssal Rock"]  <- "Abyssal"
levels(ES_data$broad)[levels(ES_data$broad) == "Abyssal Sediment"]  <- "Abyssal"


#  remove empty or NA rows that are accidentally in the end of table and rename columns (ice habitats don't exist in other)
ES_data<-dplyr::filter(ES_data, broad!="", broad!="NA")

# ***************************************************************************
# *********  Calculate capacity using BEAT indicators ***********


###  Combine integrated BEAT values to the data with EEA cells, BEAT grids and broad habitat percents

data<-full_join(integ_beat,habs_in_grids_mod, by=c("SpatialAssessmentUnit"="GRIDCODE"))

### Combine created data with ES_data. Choose only the ecosystem component (broad habitat + biotic group that 
# are contributing to the service e.g. are in the ES_data file. 
service_data<-left_join(ES_data,data, by=c("broad"="benthic_hab", "bioticind"="Biotic_group"))


### calculate new column with weighed capacity of each component (broad habitat + biotic). Place the BEAT value to the response function and multiply with contribution.
service_data<-mutate(service_data, weighedcpct = ifelse(response=="linear", bcont*integ_beat,
                                    ifelse(response=="sigmoidal", bcont* (exp(integ_beat)/(exp(integrated_beat)+1)),"NA"))) # later, more response functions could be added using case_when


service_data$CellCode<-as.character(service_data$CellCode) # for some reason all unused factor levels were stored in the column and didn't know how to take them out.. 


## take only coastal cells
service_data_coastal<-dplyr::filter(service_data, Coastal==1)

### Calculate capacity per cellcode per habitat

# Create function to do this
calc_step1=function(input_file){
  out<-input_file[1,c("broad","CellCode","EofOrigin.x","NofOrigin.x","area_percent")]
  out$capacity<-sum(input_file$weighedcpct)/sum(input_file$bcont) # this takes the average BQR. The components with higher contribution have higher impact on the average
  out$sum_indicators<-sum(input_file$number_indicators)                
  
  return(out)
}

##apply function
BEATcalc1<-ddply(service_data, .(CellCode,broad),calc_step1)                 # all cells
BEATcalc1_coastal<-ddply(service_data_coastal, .(CellCode,broad),calc_step1) #only coastal cells


### Calculate capacity per cellcode

## OPTION 1: Calculate capacity using percent of each broad habitat in each EEA cell 
calc_step2_multiply=function(file1){ #input DataFrame-muotoinen
  final1<-file1[1,c("CellCode","EofOrigin.x","NofOrigin.x")]
 
   final1$final_capacity<-sum(file1$capacity*file1$area_percent)
   final1$total_indicators<-sum(file1$number_indicators) 
  
  return(final1)
}

#apply function
capacities_beat_area_multiplied<-ddply(BEATcalc1, .(CellCode),calc_step2_multiply)                 #all cells
capacities_beat_area_multiplied_coastal<-ddply(BEATcalc1_coastal, .(CellCode),calc_step2_multiply) # coastal cells


## OPTION 2: Calculate capacity without multipling with area and just taking mean of BQRs
calc_step2_mean=function(file2){ #input DataFrame-muotoinen
  final2<-file2[1,c("CellCode","EofOrigin.x","NofOrigin.x")]
  
  final2$final_capacity<-mean(file2$capacity)
  final2$area_covered<-sum(file2$area_percent) 
  final2$total_indicators<-sum(file2$sum_indicators) 
  
  return(final2)
}

#apply function
capacities_beat_area_as_weight<-ddply(BEATcalc1, .(CellCode),calc_step2_mean) #all cells
capacities_beat_area_as_weight_coastal<-ddply(BEATcalc1_coastal, .(CellCode),calc_step2_mean) # coastal cells


# write tables
write.table(capacities_beat_area_multiplied, sep=";", file="Output/FloodCapacity_BEAT_area_multiplied.csv", row.names=F)                  # OPTION 1 all cells
write.table(capacities_beat_area_multiplied_coastal, sep=";", file="Output/FloodCapacity_BEAT_area_multiplied_coastal.csv", row.names=F)  # OPTION 1 coastal cells
write.table(capacities_beat_area_as_weight, sep=";", file="Output/FloodCapacity_BEAT_area_as_weight.csv", row.names=F)                  # OPTION 2 all cells
write.table(capacities_beat_area_as_weight_coastal, sep=";", file="Output/FloodCapacity_BEAT_area_as_weight_coastal.csv", row.names=F)  # OPTION 2 coastal cells

rm(integ_beat,service_data,service_data_coastal,capacities_beat_area_as_weight,capacities_beat_area_as_weight_coastal,
   capacities_beat_area_multiplied,capacities_beat_area_multiplied_coastal,BEATcalc1,BEATcalc1_coastal,data)


# ***************************************************************************
# *********  Calculate capacity using CEA ***********

#reorder the columns
CEA_data<-select(CEA_data, CellCode,EofOrigin,NofOrigin,Coastal,Offshore,Baleen_wha,breed_bird, deeptooth, FishSpec,Seal, 
               Smalltooth, Turtles, Saltmarsh,Seagrass, Seamounts,  Cold_coral, Abyssal, CircaCS, CircaMS, CircaMud,
               CircaRBR,CircaSand, InfraCS, InfraMS,InfraMud, InfraRBR, InfraSand, OffCiCS, OffCiMS, 
               OffCiMud, OffCiRBR, OffCiSand,Bathy)

colnames(CEA_data)<-c("CellCode_CEA","EofOrigin_CEA","NofOrigin_CEA","Coastal_CEA", "Offshore_CEA","Baleen_wha_CEA","breed_bird_CEA", "deeptooth_CEA", 
                     "FishSpec_CEA","Seal_CEA","Smalltooth_CEA","Turtles_CEA", "Saltmarsh_CEA","Seagrass_CEA", "Seamounts_CEA", 
                     "Cold_coral_CEA", "Abyssal", "CircaCS", "CircaMS", "CircaMud","CircaRBR","CircaSand", "InfraCS", "InfraMS","InfraMud", "InfraRBR", "InfraSand", "OffCiCS", "OffCiMS", 
                     "OffCiMud", "OffCiRBR", "OffCiSand","Bathy")


# normalize columns to range 0-1 and chance to 1-value
CEA<-cbind(CEA_data[,1:3],1-normalize(CEA_data[,4:33], method="range", range=c(0,1), margin=2)) %>% 
  gather(key="benthic_hab_CEA",value="CEA_value", Abyssal:Bathy, 
                     factor_key=TRUE, na.rm=TRUE) 

## join with the habitat percent data
all_CEA<-full_join(habs_in_grids_mod, CEA, by=c("CellCode"="CellCode_CEA", "benthic_hab"="benthic_hab_CEA"))%>% 
  select(-EofOrigin_CEA,-NofOrigin_CEA,-GRIDCODE,-Centr_X,-Centr_Y)

# combine table with ES data
service_CEA<-left_join(ES_data,all_CEA, by=c("broad"="benthic_hab"))

write.table(CEA, sep=";", file="Output/CEA.csv", row.names=F)    # OPTION 2 coastal cells
write.table(habs_in_grids_mod, sep=";", file="Output/habs_in_grids_mod.csv", row.names=F)    # OPTION 2 coastal cells

### calculate new column with weighed capacity of each component (broad habitat + bioticind) using the contribution and response function
service_CEA<-mutate(service_CEA, weighedcpct = ifelse(response=="linear", bcont*CEA_value,
                                              ifelse(response=="sigmoidal", bcont* (exp(CEA_value)/(exp(CEA_value)+1)),"NA")))

# change the new column from character to numeric
service_CEA$weighedcpct<-as.numeric(service_CEA$weighedcpct)



## take only coastal cells
service_CEA_coastal<-dplyr::filter(service_CEA, Coastal==1)

### Calculate capacity per cell using same approach as with BEAT indicators

#apply functions
# step 1
CEAcalc1<-ddply(service_CEA, .(CellCode,broad),calc_step1)
CEAcalc1_coastal<-ddply(service_CEA_coastal, .(CellCode,broad),calc_step1)

# Step 2 Option 1
capacities_CEA_area_multiplied<-ddply(CEAcalc1, .(CellCode),calc_step2_multiply) %>% select(-total_indicators) #all cells
capacities_CEA_area_multiplied_coastal<-ddply(CEAcalc1_coastal, .(CellCode),calc_step2_multiply)%>% select(-total_indicators) # coastal cells

# Step 2 OPTION 2
capacities_CEA_area_as_weight<-ddply(CEAcalc1, .(CellCode),calc_step2_mean)%>% select(-total_indicators)                 #all cells
capacities_CEA_area_as_weight_coastal<-ddply(CEAcalc1_coastal, .(CellCode),calc_step2_mean)%>% select(-total_indicators) # coastal cells


# Write tables
write.table(capacities_CEA_area_multiplied, sep=";", file="Output/FloodCapacity_CEA_area_multiplied.csv", row.names=F)                  # OPTION 1 all cells
write.table(capacities_CEA_area_multiplied_coastal, sep=";", file="Output/FloodCapacity_CEA_area_multiplied_coastal.csv", row.names=F)  # OPTION 1 coastal cells
write.table(capacities_CEA_area_as_weight, sep=";", file="Output/FloodCapacity_CEA_area_as_weight.csv", row.names=F)                    # OPTION 2 all cells
write.table(capacities_CEA_area_as_weight_coastal, sep=";", file="Output/FloodCapacity_CEA_area_as_weight_coastal.csv", row.names=F)    # OPTION 2 coastal cells

