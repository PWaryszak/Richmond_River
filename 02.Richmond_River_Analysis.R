#LOAD Richmond River DATA:=====
library(tidyverse)
library(gridExtra)
library(sjPlot)    #For producing figures of lmer outputs
library(sjmisc)    #For producing tables of lmer outputs
library(lmerTest)  #For  computing P-value in lmer
library(lme4)      #For running lmer
library(apaTables) #For producing Anova-style tables in Word off lm and aov


aa <- read.csv("RR_Plant.csv") #Ex aboveground.csv
bb <- read.csv("RR_Soil.csv") #Ex belowground.csv
SiteTreat <- read.csv("RR_SiteTreatment_PC.csv")##list sites and corresponding treatments
NewTreatNames <- select(SiteTreat, Site, SiteRenamed)


#Plant biomass & carbon model:=========
Plant_CarbonStock <- select (aa, Site,Treatment2, Total_Aboveground_Biomass_kg_100m2) %>%
  left_join(NewTreatNames, by = "Site") %>%
  mutate(Ecosystem = SiteRenamed,
         Total_Aboveground_Biomass_Mg_ha = Total_Aboveground_Biomass_kg_100m2 / 10 , #1 kg/100m2 = 0.1 tonnes/ha
         Plant_C = 0.464* Total_Aboveground_Biomass_Mg_ha) %>% #convert to organic carbon MG/ha
  na.omit() %>%
  filter(Treatment2 != "Disturbed") #there are no data for Disturbed sites.

Plant_CarbonStock$Ecosystem <- droplevels(Plant_CarbonStock$Ecosystem)#drop Disturbed level
Plant_CarbonStock$Ecosystem <- factor(Plant_CarbonStock$Ecosystem, levels = c("Rehabilitated","Established"))#Relevel to move Rehabilitated to intercept on plant_carbon_model
levels(Plant_CarbonStock$Ecosystem)# "Rehabilitated" "Established" 

#Model Plant_C~Ecosystem+Site:=====
Plant_C_model <- lmer(Plant_C ~ Ecosystem + (1|Site), data = Plant_CarbonStock)
summary(Plant_C_model)
#                     Estimate Std. Error     df t value Pr(>|t|)
#(Intrct="Rehabilitated"49.413     16.621  7.048   2.973   0.0206
#Ecosystem= Established 96.444     28.740  7.002   3.356   0.0121

#Word Table: Site Effect on Plant_C :
plant_site_model <- lm (Plant_C ~ Site, data = Plant_CarbonStock)
#Produce ANOVA-style output table in Word:
options(contrasts = c("contr.sum", "contr.poly"))#in aov Compare to overall mean: https://www.dummies.com/programming/r/how-to-set-the-contrasts-for-your-data-with-r/
#apa.reg.table(plant_site_model, filename="PLANT_AOV_SiteEffects.doc")


#CAR: Model Plant_C ~ Stand_Age, with zero intercept:===========
Plant_CarbonStock2 <-left_join(Plant_CarbonStock, SiteTreat, by = "Site") %>%
  mutate(Stand_Age = 2017 - as.numeric(as.character(SiteYear))) %>%
  select(Ecosystem, Plant_C, Stand_Age) 

plant_CAR_model <- lm(Plant_C~ 0+Stand_Age , data = Plant_CarbonStock2)
summary(plant_CAR_model )
tab_model(plant_CAR_model)

plot(Plant_C ~ Stand_Age, data = Plant_CarbonStock2, xlim=c(0,100),ylim=c(0,300))
abline(lm(Plant_C ~ 0+ Stand_Age, data = Plant_CarbonStock2),lwd=4,col='red')


#Model Soil_C ~ Ecosystem till 1m deep:====
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

#Compute total C-stock per core ((Rehab + Converted):
Soil_CarbonStock <- select(NewDATA, Site, Site_short,Site_Core, Treatment,Depth_to, Depth_Range,
               Site_Core, Carbon_stock_in_section_Mg_Cha,Treatment2,CarbonStock.Mgha,
               Lab_Compaction_Correction_Value)%>%
  filter  (Treatment != "Saltmarsh_natural") %>%
  filter (Depth_to <= 100 ) %>% #keeping only top 100 cm of each core
  mutate (Stock = "Soil") %>% #Assigning additional category, indicatig it is soil stock
  group_by(Site_Core) %>% #grouping by core till 100 cm
  summarise(Soil_C = sum(CarbonStock.Mgha, na.rm = T))%>% #Add-up all slices 
  separate(Site_Core, into = c("Site","CoreNumber"), sep = "_", remove = F)#Separate Site from core

soil_carbon <- left_join(Soil_CarbonStock, SiteTreat, by = "Site" )
soil_carbon$Ecosystem <- soil_carbon$SiteRenamed
soil_carbon2 <- filter(soil_carbon, SiteRenamed != "Rehabilitated") #Compare 2 sites only: Converted to Established.
levels(droplevels(soil_carbon2$Ecosystem))#"Converted"   "Established"

#Run Soil Stock model:
Soil_C_model <- lmer (Soil_C ~ Ecosystem +(1|Site), data = soil_carbon2)
summary(Soil_C_model)
#                     Estimate Std. Error     df t value Pr(>|t|)
#(Inpt) ="Converted"     92.506     22.175  4.349   4.172   0.0118
#Ecosystem=Established   93.607     30.940  4.123   3.025   0.0375
anova(Soil_C_model)
mean((residuals(Soil_C_model))^2)#Mean Square Residuals


#CAR: Model Soil_C ~ Stand_Age, with zero intercept (Rehab + Established):===========
#Compute total C-stock per core:
soil <- select(NewDATA, Site, Site_short,Site_Core, Treatment,Depth_to, Depth_Range,
               Site_Core, Carbon_stock_in_section_Mg_Cha,Treatment2,CarbonStock.Mgha,
               Lab_Compaction_Correction_Value)%>%
  filter  (Treatment != "Saltmarsh_natural") %>%
  filter (Depth_to <= 100 ) %>% #keeping only top 100 cm of each core
  mutate (Stock = "Soil") %>% #Assigning additional category, indicatig it is soil stock
  group_by(Site_Core) %>% #grouping by core till 100 cm
  summarise(Soil_C = sum(CarbonStock.Mgha, na.rm = T))%>% #Add-up all slices 
  separate(Site_Core, into = c("Site","CoreNumber"), sep = "_", remove = F)#Separate Site from core

soil_carbon <- left_join(soil, SiteTreat, by = "Site" )
soil_carbon$Ecosystem <- soil_carbon$SiteRenamed
soil_carbon2 <- filter(soil_carbon, SiteRenamed != "Converted") #Run on Rehab and Established sites only.

#Join Treatment Descriptors (as names got updated):
Soil_CarbonStock2 <-left_join(soil_carbon2, NewTreatNames, by = "Site")%>%
  mutate(Stand_Age = 2017 - as.numeric(as.character(SiteYear))) %>%
  select(Ecosystem, Soil_C, Stand_Age) 

#Estimate CAR (Carbon Acrretion Rate) in soil:
Soil_CAR_model <- lm(Soil_C~ 0+Stand_Age , data = Soil_CarbonStock2)
summary(Soil_CAR_model )
tab_model(Soil_CAR_model)

plot(Soil_C ~ Stand_Age, data = Soil_CarbonStock2, xlim=c(0,100),ylim=c(0,300))
abline(lm(Soil_C ~ 0+ Stand_Age, data = Soil_CarbonStock2),lwd=4,col='red')


#Draw Table for Plan_C and Soil_C models:========

tab_model(Plant_C_model,Soil_C_model, show.icc = F,show.df = T) #Draw a output Table
#Plant_C_model = Established + Rehab
#Soil_C_model = Established + Converted