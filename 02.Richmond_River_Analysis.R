#LOAD libraries:=====
library(tidyverse)
library(lmerTest)  #For  computing P-value in lmer
library(lme4)      #For running lmer
library(sjPlot)    #For producing figures of lmer outputs
library(sjmisc)    #For producing tables of lmer outputs

library(gridExtra)
library(apaTables) #For producing Anova-style tables in Word off lm and aov

#LOAD Richmond River DATA:
aa <- read.csv("RR_Plant.csv") #Ex aboveground.csv
bb <- read.csv("RR_Soil.csv") #Ex belowground.csv
SiteTreat <- read.csv("RR_SiteTreatment_PC.csv")##list sites and corresponding treatments
NewTreatNames <- select(SiteTreat, Site, SiteRenamed,SiteYearNumeric)


#Plant biomass & carbon model:=========
#Calculate Plant_C (plant carbon stock):
Plant_CarbonStock <- select (aa, Site,Treatment2, Total_Aboveground_Biomass_kg_100m2) %>%
  left_join(NewTreatNames, by = "Site") %>%
  mutate(Ecosystem = SiteRenamed,
         Total_Aboveground_Biomass_Mg_ha = Total_Aboveground_Biomass_kg_100m2 / 10 , #1 kg/100m2 = 0.1 tonnes/ha
         Plant_C = 0.464* Total_Aboveground_Biomass_Mg_ha) %>% #convert to organic carbon MG/ha
  na.omit() %>%
  filter(Treatment2 != "Disturbed") #there are no data for Disturbed sites.

Plant_CarbonStock$Ecosystem <- droplevels(Plant_CarbonStock$Ecosystem)#drop Disturbed level
Plant_CarbonStock$Ecosystem <- factor(Plant_CarbonStock$Ecosystem, levels = c("Rehabilitated","Established"))#Relevel to move Rehabilitated to intercept on plant_carbon_model
#write.csv(Plant_CarbonStock, file = "Plant_CarbonStock.csv",row.names = F)

#Model Plant_C~Ecosystem + (1|Site):
levels(Plant_CarbonStock$Ecosystem)# Check levels. Should be two: "Rehabilitated" & "Established" 
Plant_C_model <- lmer(Plant_C ~ Ecosystem + (1|Site), data = Plant_CarbonStock)
summary(Plant_C_model)
#                     Estimate Std. Error     df t value Pr(>|t|)
#(Intrct="Rehabilitated"49.413     16.621  7.048   2.973   0.0206
#Ecosystem= Established 96.444     28.740  7.002   3.356   0.0121





#Model Soil_C ~ Ecosystem+ (1|Site), till 1m deep:====
#Compute Mean soil C-stock accounting for compaction using Compaction Correction Value.
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
  separate(Site_Core, into = c("Site","CoreNumber"), sep = "_", remove = F) #Separate Site from core

soil_carbon <- left_join(Soil_CarbonStock, SiteTreat, by = "Site" )
soil_carbon$Ecosystem <- soil_carbon$SiteRenamed
#write.csv(soil_carbon, file = "soil_C_sites.csv", row.names = F)

soil_carbon2 <- filter(soil_carbon, SiteRenamed != "Rehabilitated") #Compare 2 sites only: Converted to Established.
levels(droplevels(soil_carbon2$Ecosystem))#Check. Should be: "Converted"  & "Established"

#Run Soil Stock model:
Soil_C_model <- lmer (Soil_C ~ Ecosystem +(1|Site), data = soil_carbon2)
summary(Soil_C_model)
#                     Estimate Std. Error     df t value Pr(>|t|)
#(Inpt) ="Converted"     92.506     22.175  4.349   4.172   0.0118
#Ecosystem=Established   93.607     30.940  4.123   3.025   0.0375


#Site Effects Table=====
#Word Table: Site Effect on Plant_C :
p <- read.csv("Plant_CarbonStock.csv")#Data saved previosuly off script on Plant_CarbonStock above. 
plant_site_model <- lm (Plant_C ~ Site, data = p)
summary(plant_site_model)

#Produce ANOVA-style output table in Word:
#options(contrasts = c("contr.sum", "contr.poly"))#in aov Compare to overall mean: https://www.dummies.com/programming/r/how-to-set-the-contrasts-for-your-data-with-r/
#apa.reg.table(plant_site_model, filename="StatsTable_PLANT_SiteEffects.doc")#uncomment to run

#Word Table: Site Effect on Soil_C :
s <- read.csv( "soil_C_sites.csv")#There was a glitch,I had export and import these data to make lm work
Soil_site_model <- lm (Soil_C ~ Site, data = s)
summary(Soil_site_model)
#Produce ANOVA-style output table in Word:
library(apaTables) #For producing Anova-style tables in Word off lm and aov
#options(contrasts = c("contr.sum", "contr.poly"))#in aov Compare to overall mean: https://www.dummies.com/programming/r/how-to-set-the-contrasts-for-your-data-with-r/
#apa.reg.table(Soil_site_model, filename="StatsTable_Soil_SiteEffects.doc")#uncomment to run




#Model Plant_C ~ Stand_Age, with zero intercept:===========
Plant_CarbonStock2 <- Plant_CarbonStock %>%
  mutate(Stand_Age = 2017 - as.numeric(as.character(SiteYearNumeric))) %>%
  select(Ecosystem, Plant_C, Stand_Age) 

plant_CAR_model <- lm(Plant_C~ 0 + Stand_Age, data = Plant_CarbonStock2)
summary(plant_CAR_model )
tab_model(plant_CAR_model)

plot(Plant_C ~ Stand_Age, data = Plant_CarbonStock2, xlim=c(0,100),ylim=c(0,300))
abline(lm(Plant_C ~ 0 + Stand_Age, data = Plant_CarbonStock2),lwd=4,col='red')



# Model Soil_C ~ Stand_Age, with zero intercept (Rehab + Established, C 100 cm):===========
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

soil_carbon <- left_join(soil, NewTreatNames, by = "Site" )
soil_carbon$Ecosystem <- soil_carbon$SiteRenamed
soil_carbon2 <- filter(soil_carbon, SiteRenamed != "Converted") #Run on Rehab and Established sites only.

#Join Treatment Descriptors (as names got updated):
Soil_CarbonStock2 <- soil_carbon2  %>%
  mutate(Stand_Age = 2017 - as.numeric(as.character(SiteYearNumeric))) %>%
  select(Ecosystem, Soil_C, Stand_Age) 

#Estimate CAR (zero intercept) in soil:
Soil_CAR_model <- lm(Soil_C~ 0 + Stand_Age , data = Soil_CarbonStock2)
summary(Soil_CAR_model )
tab_model(Soil_CAR_model)

plot(Soil_C ~ Stand_Age, data = Soil_CarbonStock2, xlim=c(0,100),ylim=c(0,300))
abline(lm(Soil_C ~ 0+ Stand_Age, data = Soil_CarbonStock2),lwd=4,col='red')



# Model Soil_C ~ Stand_Age, with zero intercept (Rehab + Established, C TILL HORIZON):===========
#Compute total C-stock per core:
NewDATA2 <- left_join (NewDATA, SiteTreat, by = "Site") %>%  ##joining stock and site description data
  mutate(KeepThrow = ifelse(DepthTo_SinceRehabilitated_cm >= Depth_from, "keep", "throw")) %>% #keep slices data are > Depth_from
  filter(KeepThrow=="keep") %>% #keep the "keep"
  transform (DepthAtRehab_cm = ifelse(Depth_to <= DepthTo_SinceRehabilitated_cm,  #Cut the bottom slice to the length of DepthTo_SinceRehabilitated_cm
                                      Depth_to, DepthTo_SinceRehabilitated_cm)) %>%
  mutate (SliceAtRehab_cm = DepthAtRehab_cm - Depth_from) %>% #lenght of slice at cores up to DepthTo_SinceRehabilitated_cm
  mutate (CarbonStockTillRehab_Mgha = CarbonDensity.gcm3  * 100 * SliceAtRehab_cm) #Soil C stock in cores till DepthTo_SinceRehabilitated_cm



#stock till horizon and get AV+-SE:
soil_till_rehab <- select(NewDATA2, Site,SiteYear, Site_short,Site_Core, Treatment,Depth_to, Depth_Range,
                          Site_Core, Carbon_stock_in_section_Mg_Cha,Treatment2,CarbonStock.Mgha,
                          CarbonStockTillRehab_Mgha)%>%
  filter  (Treatment != "Saltmarsh_natural") %>% #not looking at saltmarhs in this study, mangroves only
  mutate (Stock = "Soil") %>% #Assigning additional category, indicatig it is soil stock
  group_by(Site_Core) %>% #grouping by core
  summarise(Soil_C = sum(CarbonStockTillRehab_Mgha))%>% #Add-up all slices 
  separate(Site_Core, into = c("Site","CoreNumber"), sep = "_") %>%
  mutate (Stock = "Soil_Till_Rehab")


soil_carbon <- left_join(soil_till_rehab, NewTreatNames, by = "Site" )
soil_carbon$Ecosystem <- soil_carbon$SiteRenamed
soil_carbon2 <- filter(soil_carbon, SiteRenamed != "Converted") #Run on Rehab and Established sites only.

#Join Treatment Descriptors (as names got updated):
Soil_CarbonStock2 <- soil_carbon2  %>%
  mutate(Stand_Age = 2017 - as.numeric(as.character(SiteYearNumeric))) %>%
  select(Ecosystem, Soil_C, Stand_Age) 

#Estimate CAR (zero intercept) in soil:
Soil_CAR_model <- lm(Soil_C~ 0+Stand_Age , data = Soil_CarbonStock2)
summary(Soil_CAR_model )
tab_model(Soil_CAR_model)

plot(Soil_C ~ Stand_Age, data = Soil_CarbonStock2, xlim=c(0,100),ylim=c(0,300))
abline(lm(Soil_C ~ 0+ Stand_Age, data = Soil_CarbonStock2),lwd=4,col='red')




# Soil CAR-Model (Carbon Accretion Rate)======
#Script below was used to estimate CarbonStockTillRehab_Mgha based on Stand_Age and soil C-stock till Depth of Rehab (DepthTo_SinceRehabilitated_cm)
#Some Sites, e.g., MRCH1 was too mixed to age-date hence "DepthTo_SinceRehabilitated_cm," is unavailable

#Compute Soil_Stock_Per_Year (Stock to Rehab Horizon / Stand_Age):
NewDATA2 <- left_join (NewDATA, SiteTreat, by = "Site") %>%
  mutate(KeepThrow = ifelse(DepthTo_SinceRehabilitated_cm >= Depth_from, "keep", "throw")) %>% #keep slices data are > Depth_from
  filter(KeepThrow=="keep") %>% #keep the "keep"
  transform (DepthAtRehab_cm = ifelse(Depth_to <= DepthTo_SinceRehabilitated_cm,  #Cut to the length of DepthTo_SinceRehabilitated_cm
                                      Depth_to, DepthTo_SinceRehabilitated_cm)) %>%
  mutate (SliceAtRehab_cm = DepthAtRehab_cm - Depth_from) %>% #lenght of slice at cores up to DepthTo_SinceRehabilitated_cm
  mutate (CarbonStockTillRehab_Mgha = CarbonDensity.gcm3  * 100 * SliceAtRehab_cm) #Soilc C stock in each cores till DepthTo_SinceRehabilitated_cm

CAR_soil <- NewDATA2 %>%
  select( Treatment,SiteYearNumeric, CarbonStockTillRehab_Mgha, Site_Core, Site, SiteRenamed)  %>%
  #filter (SiteRenamed == "Rehabilitated") %>%  #filter to look at rehab sites only
  filter  (Treatment != "Saltmarsh_natural") %>%
  group_by(Site, SiteYearNumeric, SiteRenamed, Site_Core) %>% #grouping by core
  summarise(SoilStockTillRehabHorizon = sum(CarbonStockTillRehab_Mgha,na.rm=T))%>% #Add-up all slices 
  mutate(Soil_Stand_Age = 2017 - as.numeric((as.character(SiteYearNumeric))),
         Soil_Stock_Per_Year = SoilStockTillRehabHorizon/Soil_Stand_Age )

#write.csv (CAR_soil, file = "CAR_soil_data.csv", row.names = F)
#Run Model (Stock ~ SiteType)
CAR_soil_model <- lmer(Soil_Stock_Per_Year~SiteRenamed + (1|Site), data = CAR_soil)#Soil_Stock_Per_Year = CAR based on stock to horizon / stand age
tab_model(CAR_soil_model,show.df = T,show.stat =T,
          show.icc = F, show.se = T) #Draw a output Table

#Run Model (Stock ~ SiteAGe) , zero-intercepth enforced
CAR_soil_age_model <- lm(Soil_Stock_Per_Year ~ 0 + Soil_Stand_Age, data = CAR_soil)#Soil_Stock_Per_Year = CAR based on stock to horizon / stand age
tab_model(CAR_soil_age_model,show.df = T,show.stat =T,
          show.icc = F, show.se = T) #Draw a output Table





#Plant CAR-Model (Carbon Accretion Rate)======
aa <- read.csv("RR_Plant.csv") #Previously file was named: "aboveground.csv"
SiteTreat <- read.csv("RR_SiteTreatment_PC.csv")##list sites and corresponding treatments
NewTreatNames <- select(SiteTreat, Site, SiteRenamed,SiteYearNumeric)

CAR_plant <- select (aa, Site,Subplot_Name, Treatment2, Total_Aboveground_Biomass_kg_100m2) %>%
  left_join(NewTreatNames, by = "Site") %>%
  filter(SiteRenamed != "Converted") %>% #Remove data of convereted sites (Zero veg cover there)
  mutate(Total_Aboveground_Biomass_Mg_ha = Total_Aboveground_Biomass_kg_100m2 / 10,  #1 kg/100m2 = 0.1 tonnes/ha
         Plant_C = 0.464* Total_Aboveground_Biomass_Mg_ha) %>% #%>%  #1 kg/100m2 = 0.1 tonnes/ha
  mutate(Plant_Stand_Age = 2017 - as.numeric((as.character(SiteYearNumeric))),
         Plant_Stock_Per_Year = Plant_C/Plant_Stand_Age,
         Site_Core = Subplot_Name) #Adding Site-Core column to match belowground data 

CAR_plant_model <- lmer(Plant_Stock_Per_Year~SiteRenamed + (1|Site), data = CAR_plant)
tab_model(CAR_plant_model,show.df = T,show.stat =T,
          show.icc = F) #Draw a output Table

#Draw Table for Plan_C and Soil_C models:
#Plant_C_model = Established + Rehab
#Soil_C_model = Established + Converted
tab_model(CAR_soil_model,CAR_plant_model,show.df = T,show.stat =T,show.icc = F)
