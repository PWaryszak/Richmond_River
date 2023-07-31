#LOAD libraries:=====
library(tidyverse)
library(lmerTest)  #For  computing P-value in lmer
library(lme4)      #For running lmer
library(sjPlot)    #For producing figures of lmer outputs
library(sjmisc)    #For producing tables of lmer outputs
library("insight")
library(gridExtra)
library(apaTables) #For producing Anova-style tables in Word off lm and aov

#LOAD Richmond River DATA:
aa <- read.csv("RR_Plant.csv") #Ex aboveground.csv
bb <- read.csv("RR_Soil.csv") #Ex belowground.csv
SiteTreat <- read.csv("RR_SiteTreatment_PC.csv")##list sites and corresponding treatments
NewTreatNames <- select(SiteTreat, Site, SiteRenamed,SiteYearNumeric)


#Plant C=========
#Calculate Plant_C (plant carbon stock):
Plant_CarbonStock <- select (aa, Site,Treatment2, Total_Aboveground_Biomass_kg_100m2) %>%
  left_join(NewTreatNames, by = "Site") %>%
  mutate(Ecosystem = SiteRenamed,
         Total_Aboveground_Biomass_Mg_ha = Total_Aboveground_Biomass_kg_100m2 / 10 , #1 kg/100m2 = 0.1 tonnes/ha
         Plant_C = 0.464* Total_Aboveground_Biomass_Mg_ha) %>% #convert to organic carbon MG/ha
  na.omit() %>%
  filter(Treatment2 != "Disturbed") #there are no data for Disturbed sites.

Plant_CarbonStock$Ecosystem <- factor(Plant_CarbonStock$Ecosystem, levels = c("Rehabilitated","Established"))#Relevel to move Rehabilitated to intercept on plant_carbon_model
#write.csv(Plant_CarbonStock, file = "Plant_CarbonStock.csv",row.names = F)
View(Plant_CarbonStock)

#Run a Model on Plant C:
plant_site_model <- lm (Plant_C ~ Site, data = Plant_CarbonStock)
summary(plant_site_model)
#Alternatively:
p <- read.csv("Plant_CarbonStock.csv")#Data saved previously off script on Plant_CarbonStock above. 
plant_site_model <- lm (Plant_C ~ Site, data = p)
summary(plant_site_model)


#Soil_C ~ Site, till 1m deep:====
#Compute Mean soil C-stock accounting for compaction using Compaction Correction Value.
NewDATA <- bb
NewDATA$C_percent <- ifelse(NewDATA$C_perc == 0, 0.001, NewDATA$C_perc)#convert 0 into 0.001 to run log-models if any
NewDATA$SliceLength.cm <- (NewDATA$Depth_to - NewDATA$Depth_from) #cm
NewDATA$PipeDiameter.cm <- 5 #Diameter of coring pipes was 5 cm
NewDATA$SampleVolume.cm3 <- (pi*(NewDATA$PipeDiameter.cm/2)^2)*NewDATA$SliceLength.cm  #slice volume

#Correct Sample volume by compaction:
NewDATA$dry_bulk_density.gcm3_corrected <- NewDATA$Dry_bulk_density_gcm3 * NewDATA$Lab_Compaction_Correction_Value

NewDATA$CarbonDensity.gcm3 <- NewDATA$dry_bulk_density.gcm3_corrected * NewDATA$C_percent/100
NewDATA$CarbonStock.Mgha <- ((NewDATA$CarbonDensity.gcm3  * 100) * NewDATA$SliceLength.cm )

range(NewDATA$Lab_Compaction_Correction_Value, na.rm = T)# Check if all values are  below 1 = If value = 1 this core had no records of compaction
range(NewDATA$CarbonStock.Mgha, na.rm = T )# 0.0000 125.6347
range(NewDATA$CarbonDensity.gcm3, na.rm = T )#0.00000000 0.06281735


#Compute total C-stock per core ((Rehab + Converted):
Soil_CarbonStock <- select(NewDATA, Site, Site_short,Site_Core, Treatment,Depth_to, Depth_Range,
                           Site_Core, Carbon_stock_in_section_Mg_Cha,Treatment2,CarbonStock.Mgha,
                           Lab_Compaction_Correction_Value)%>%
  filter  (Treatment != "Saltmarsh_natural") %>%
  filter (Depth_to <= 100 ) %>% #keeping only top 100 cm of each core
  mutate (Stock = "Soil") %>% #Assigning additional category, indicatig it is soil stock
  group_by(Site_Core) %>% #grouping by core till 100 cm
  summarise(Soil_C = sum(CarbonStock.Mgha, na.rm = T))%>% #Add-up all slices 
  separate(Site_Core, into = c("Site","CoreNumber"), sep = "_", remove = F) #Separate Site from core

soil_carbon <- left_join(Soil_CarbonStock, SiteTreat, by = "Site" )
soil_carbon$Ecosystem <- soil_carbon$SiteRenamed
#write.csv(soil_carbon, file = "soil_C_sites.csv", row.names = F)

#Run Soil Stock model:
unique(soil_carbon$Site)#"MDBP"   "MDSD"   "MDTUCK" "MNDC"   "MNND"   "MNPL"   "MR1991" "MR1992" "MRAP"   "MRCH1"  "MRCH2"  "MRPL"  

Soil_C_model <- lm(Soil_C ~ Site, data = soil_carbon)
summary(Soil_C_model)
#                     Estimate Std. Error     df t value Pr(>|t|)
#(Inpt) ="Converted"     92.506     22.175  4.349   4.172   0.0118
#Ecosystem=Established   93.607     30.940  4.123   3.025   0.0375
tab_model(Soil_C_model,show.re.var=T)

#Table S3 (off 2 models) Site effects (Word Table)=======
tab_model(Soil_C_model,plant_site_model,show.re.var=T)


#Table S3 off summarise function (restructure Above Table to AV+SE)========
#Run all above /\:

#PLANT (Aboveground):======
Plant_CarbonStock$Ecosystem <- factor(Plant_CarbonStock$Ecosystem, levels = c("Rehabilitated","Established"))#Relevel to move Rehabilitated to intercept on plant_carbon_model
head(Plant_CarbonStock)

Aboveground <-   Plant_CarbonStock %>%
  mutate(Plant_C = Total_Aboveground_Biomass_Mg_ha * 0.464) %>% ##0.464 conversion factor after Kauffman & Donato 2012 and Howard et al 2014)
  
  #Rename the Rehab site based on their age:
  mutate(SiteRenamed_25y = ifelse (SiteYearNumeric < 1992 & SiteRenamed =="Rehabilitated", "Rehabilitated_old" ,SiteRenamed)) %>%
  mutate(SiteRenamed_25y = ifelse (SiteYearNumeric >= 1992 & SiteRenamed =="Rehabilitated", "Rehabilitated_young" ,SiteRenamed_25y)) %>%
  
  group_by(Site,SiteRenamed_25y) %>%
  summarise(AV = round(mean(Plant_C, nr.rm=T),2),
                           N = n(),
                           SD = sd(Plant_C),
                           SE = round(SD/sqrt(N),2))  # %>% mutate(CO2_equivalent = AV * 3.67 )
Aboveground

Aboveground_united <- Aboveground %>%
  select(AV,SE, Site, SiteRenamed_25y) %>%
  #unite("stock", c("AV", "SE"), sep = " ± " ) %>%
  rename(Site_Type = SiteRenamed_25y) %>%
  mutate(StockSource = "Aboveground")

  
Aboveground_united

#Soil (Belowground):========
View(soil_carbon)

Belowground <-   soil_carbon %>%

  group_by(Site,SiteRenamed_25y) %>%
  summarise(AV = round(mean(Soil_C, nr.rm=T),2),
            N = n(),
            SD = sd(Soil_C),
            SE = round(SD/sqrt(N),2))  # %>% mutate(CO2_equivalent = AV * 3.67 )
Belowground

Belowground_united <- Belowground %>%
  select(AV,SE, Site, SiteRenamed_25y) %>%
  #unite("stock", c("AV", "SE"), sep = "//± " ) %>%
  rename(Site_Type = SiteRenamed_25y) %>%
  mutate(StockSource = "Belowground")

Belowground_united

AB <- rbind(Belowground_united, Aboveground_united)
write.csv(AB, file = "Table3_Restructured.csv")
