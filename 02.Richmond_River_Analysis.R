#LOAD Richmond River DATA:=====
library(tidyverse)
library(gridExtra)
library(sjPlot)
library(sjmisc)

aa <- read.csv("RR_Plant.csv") #Ex aboveground.csv
bb <- read.csv("RR_Soil.csv") #Ex belowground.csv
SiteTreat <- read.csv("RR_SiteTreatment_PC.csv")##list sites and corresponding treatments

#Plant biomass & carbon model:=========
Plant_CarbonStock <- select (aa, Site,Treatment2, Total_Aboveground_Biomass_kg_100m2) %>%
  mutate(Ecosystem = Treatment2,
         Total_Aboveground_Biomass_Mg_ha = Total_Aboveground_Biomass_kg_100m2 / 10 , #1 kg/100m2 = 0.1 tonnes/ha
         Plant_C = 0.464* Total_Aboveground_Biomass_Mg_ha) %>%
  na.omit() %>%
  filter(Ecosystem != "Disturbed") #there are no data for Disturbed sites.

Plant_CarbonStock$Ecosystem <- droplevels(Plant_CarbonStock$Ecosystem)#drop Disturbed level

#model Total_Aboveground_Biomass_Mg_ha~Ecosystem:=====
plant_biomass_model <- lm(Total_Aboveground_Biomass_Mg_ha ~ Ecosystem, data = Plant_CarbonStock)
summary(plant_biomass_model )
tab_model(plant_biomass_model)

#model Plant_C~Ecosystem:=====
plant_carbon_model <- lm(Plant_C ~ Ecosystem, data = Plant_CarbonStock)
summary(plant_carbon_model )
tab_model(plant_carbon_model)

#Run Total_Aboveground_Biomass_Mg_ha~Ecosystem ANOVA:
plant_aov <- aov(Total_Aboveground_Biomass_Mg_ha ~ Ecosystem, data = Plant_CarbonStock)
summary(plant_aov )
tab_model(plant_aov )
sjp.aov1 (Plant_CarbonStock$Total_Aboveground_Biomass_Mg_ha,Plant_CarbonStock$Ecosystem,
          show.summary=T, show.p = T)


#Model Plant_C ~ Stand_Age, with zero intercept:===========
Plant_CarbonStock2 <-left_join(Plant_CarbonStock, SiteTreat, by = "Site") %>%
  mutate(Stand_Age = 2017 - as.numeric(as.character(SiteYear))) %>%
  select(Ecosystem, Plant_C, Stand_Age) 

plantC_model <- lm(Plant_C~ 0+Stand_Age , data = Plant_CarbonStock2)
summary(plantC_model )
tab_model(plantC_model)

plot(Plant_C ~ Stand_Age, data = Plant_CarbonStock2, xlim=c(0,100),ylim=c(0,300))
abline(lm(Plant_C ~ 0+ Stand_Age, data = Plant_CarbonStock2),lwd=4,col='red')


#Model Soil Stock ~ Ecosystem till 1m deep:====
#Compute Mean corrected soil C-stock accounting for compaction using Compaction Correction Value.
NewDATA <- bb
NewDATA$C_percent <- ifelse(NewDATA$C_perc == 0, 0.001, NewDATA$C_perc)#convert 0 into 0.001 to run log-models if any
NewDATA$SliceLength.cm <- (NewDATA$Depth_to - NewDATA$Depth_from) #cm
NewDATA$PipeDiameter.cm <- 5 #Diameter of coring pipes was 5 cm
NewDATA$SampleVolume.cm3 <- (pi*(NewDATA$PipeDiameter.cm/2)^2)*NewDATA$SliceLength.cm  #slice volume
NewDATA$dry_bulk_density.gcm3_corrected <- NewDATA$Dry_bulk_density_gcm3 * NewDATA$Lab_Compaction_Correction_Value
NewDATA$CarbonDensity.gcm3 <- NewDATA$dry_bulk_density.gcm3_corrected * NewDATA$C_percent/100
NewDATA$CarbonStock.Mgha <- ((NewDATA$CarbonDensity.gcm3  * 100) * NewDATA$SliceLength.cm )

range(NewDATA$Lab_Compaction_Correction_Value, na.rm = T)# Check if all values are  below 1 = If value = 1 this core had no records of compaction
range(NewDATA$CarbonStock.Mgha, na.rm = T )# 0.0000 125.6347
range(NewDATA$CarbonDensity.gcm3, na.rm = T )#0.00000000 0.06281735

#Compute total C-stock per core:
soil <- select(NewDATA, Site, Site_short,Site_Core, Treatment,Depth_to, Depth_Range,
               Site_Core, Carbon_stock_in_section_Mg_Cha,Treatment2,CarbonStock.Mgha,
               Lab_Compaction_Correction_Value)%>%
  filter  (Treatment != "Saltmarsh_natural") %>%
  filter (Depth_to <= 100 ) %>% #keeping only top 100 cm of each core
  mutate (Stock = "Soil") %>% #Assigning additional category, indicatig it is soil stock
  group_by(Site_Core) %>% #grouping by core till 100 cm
  summarise(TotalCarbonStockPerCore = sum(CarbonStock.Mgha, na.rm = T))%>% #Add-up all slices 
  separate(Site_Core, into = c("Site","CoreNumber"), sep = "_", remove = F)#Separate Site from core

soil_carbon <- left_join(soil, SiteTreat, by = "Site" )
soil_carbon$Ecosystem <- soil_carbon$SiteRenamed

#Run Soil Stock model:
soil_model <- lm (TotalCarbonStockPerCore ~ Ecosystem, data = soil_carbon )
summary(soil_model)
tab_model(soil_model)

