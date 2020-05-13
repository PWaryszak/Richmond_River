#LOAD Richmond River DATA:=====
library(tidyverse)
library(gridExtra)
library(flextable)

aa <- read.csv("RR_Plant.csv")  #Plant biomass data
bb <- read.csv("RR_Soil.csv")  #Soil stock data
SiteTreat <- read.csv("RR_SiteTreatment_PC.csv")#list sites and corresponding treatments 

#Correct Soil data for compaction
NewDATA <- bb
NewDATA$C_percent <- ifelse(NewDATA$C_perc == 0, 0.001, NewDATA$C_perc)#convert 0 into 0.001 to run log-models if any
NewDATA$SliceLength.cm <- (NewDATA$Depth_to - NewDATA$Depth_from) #cm
NewDATA$PipeDiameter.cm <- 5 #Diameter of coring pipes was 5 cm
NewDATA$SampleVolume.cm3 <- (pi*(NewDATA$PipeDiameter.cm/2)^2)*NewDATA$SliceLength.cm  #slice volume
NewDATA$dry_bulk_density.gcm3_corrected <- NewDATA$Dry_bulk_density_gcm3 * NewDATA$Lab_Compaction_Correction_Value
NewDATA$CarbonDensity.gcm3 <- NewDATA$dry_bulk_density.gcm3_corrected * NewDATA$C_percent/100
NewDATA$CarbonStock.Mgha <- ((NewDATA$CarbonDensity.gcm3  * 100) * NewDATA$SliceLength.cm )


####################################################################
################TABLE1: Plant Carbon per Stand age at rehab sites###
####################################################################

#TABLE1_Rehabilitated sites part=========
#Estimate CarbonStockTillRehab_Mgha based on Stand_Age and soil C-stock till Depth of Rehab (DepthTo_SinceRehabilitated_cm)
#Some Sites, e.g., MRCH1 was too mixed to age-date hence "DepthTo_SinceRehabilitated_cm," is unavailable
NewDATA2 <- left_join (NewDATA, SiteTreat, by = "Site") %>%
  mutate(KeepThrow = ifelse(DepthTo_SinceRehabilitated_cm >= Depth_from, "keep", "throw")) %>% #keep slices data are > Depth_from
  filter(KeepThrow=="keep") %>% #keep the "keep"
  transform (DepthAtRehab_cm = ifelse(Depth_to <= DepthTo_SinceRehabilitated_cm,  #Cut to the length of DepthTo_SinceRehabilitated_cm
                                      Depth_to, DepthTo_SinceRehabilitated_cm)) %>%
  mutate (SliceAtRehab_cm = DepthAtRehab_cm - Depth_from) %>% #lenght of slice at cores up to DepthTo_SinceRehabilitated_cm
  mutate (CarbonStockTillRehab_Mgha = CarbonDensity.gcm3  * 100 * SliceAtRehab_cm) #Soilc C stock in each cores till DepthTo_SinceRehabilitated_cm

#Soil Carbon Per Stand Age in Rehabilitated cores only:
Rehabilitated_soil <- NewDATA2 %>%
  select( Treatment,SiteYear, CarbonStockTillRehab_Mgha, Site_Core, Site, SiteRenamed)  %>%
  filter (SiteRenamed == "Rehabilitated") %>%  #filter to look at rehab sites only
  filter  (Treatment != "Saltmarsh_natural") %>%
  group_by(Site, SiteYear, SiteRenamed, Site_Core) %>% #grouping by core
  summarise(TotalCarbonStockPerCore = sum(CarbonStockTillRehab_Mgha,na.rm=T))%>% #Add-up all slices 
  mutate(Soil_Stand_Age = 2017 - as.numeric((as.character(SiteYear))),
         Soil_Stock_Per_Year = TotalCarbonStockPerCore/Soil_Stand_Age )

#Compute mean +-SE for each site from Rehabilitated_soil:
av_stand_age_soil <- select(Rehabilitated_soil, Site, Soil_Stock_Per_Year,TotalCarbonStockPerCore ) %>%
  group_by(Site) %>%
  summarise(AV_soil = round(mean(Soil_Stock_Per_Year, na.rm = T),1),
            N_soil = length(Soil_Stock_Per_Year),
            SD_soil = round(sd(Soil_Stock_Per_Year),1),
            SE_soil = round(SD_soil/sqrt(N_soil),1),
            
            AV_stock = round(mean(TotalCarbonStockPerCore, na.rm = T),1),
            N_stock = length(TotalCarbonStockPerCore),
            SD_stock = round(sd(TotalCarbonStockPerCore),1),
            SE_stock = round(SD_stock/sqrt(N_stock),1))


#Sites need merging with site descriptors such as SiteYear:
merge_year <- select(SiteTreat, Site,SiteYear)


############PLANTS:
#Soil Carbon Per Stand Age in Rehabilitated cores only:
Rehabilitated_plant <- select (aa, Site,Subplot_Name, Treatment2, Total_Aboveground_Biomass_kg_100m2) %>%
  filter(Treatment2 == "Rehabilitated") %>%
  left_join(merge_year,by = "Site") %>%
  mutate(Total_Aboveground_Biomass_Mg_ha = Total_Aboveground_Biomass_kg_100m2 / 10,  #1 kg/100m2 = 0.1 tonnes/ha
         Plant_C = 0.464* Total_Aboveground_Biomass_Mg_ha) %>% #%>%  #1 kg/100m2 = 0.1 tonnes/ha
  mutate(Plant_Stand_Age = 2017 - as.numeric((as.character(SiteYear))),
         Plant_Stock_Per_Year = Plant_C/Plant_Stand_Age,
         Site_Core = Subplot_Name) #Adding Site-Core column to match belowground data 



#Join Rehabilitated_plant &  Rehabilitated_soil:
soil_plant_stock <- left_join(Rehabilitated_plant,Rehabilitated_soil, by = "Site_Core")

#Compute mean +_SE of plant soil and combined stock + CO2 equivalents:
av_soil_plant_stock <- soil_plant_stock %>%
  mutate(stock2 = Soil_Stock_Per_Year+ Plant_Stock_Per_Year,
         CO2_eq_both = stock2 * 3.67) %>%
  group_by(Site.x) %>%
  summarise(AV_soil_stock = round(mean(Soil_Stock_Per_Year, na.rm = T),3),
            N_soil_stock = length(Soil_Stock_Per_Year),
            SD_soil_stock = sd(Soil_Stock_Per_Year),
            SE_soil_stock = round(SD_soil_stock/sqrt(N_soil_stock),3),
            
            AV_plant_stock = round(mean(Plant_Stock_Per_Year, na.rm = T),1),
            N_plant_stock = length(Plant_Stock_Per_Year),
            SD_plant_stock = sd(Plant_Stock_Per_Year),
            SE_plant_stock = round(SD_plant_stock/sqrt(N_plant_stock),1),
            
            
            AV_stock2 = round(mean(stock2, na.rm = T),1),
            N_stock2 = length(stock2),
            SD_stock2 = sd(stock2),
            SE_stock2 = round(SD_stock2/sqrt(N_stock2),1),
            
            AV2_CO2 = round(mean (CO2_eq_both, na.rm = T),1),
            N2_CO2 = length(CO2_eq_both),
            SD2_CO2 = sd(CO2_eq_both),
            SE2_CO2 = round(SD2_CO2/sqrt(N2_CO2),1))


#Select data for table and join AV & SE columns together with  " ± " sign
av_soil_plant_stock_table <- select(av_soil_plant_stock,
                                    Site.x,
                                    AV_soil_stock, SE_soil_stock,
                                    AV_plant_stock,SE_plant_stock,
                                    AV_stock2,     SE_stock2,
                                    AV2_CO2,       SE2_CO2) %>%
  unite("Soil_carbon_per_year_since_rehab", c("AV_soil_stock", "SE_soil_stock"), sep = " ± " ) %>%
  unite("Plant_carbon_per_year_since_rehab", c("AV_plant_stock", "SE_plant_stock"), sep = " ± " ) %>%
  unite("Combined_carbon_accumulation", c("AV_stock2", "SE_stock2"), sep = " ± " ) %>%
  unite("CO2_eq_per_ha_per_year", c("AV2_CO2", "SE2_CO2"), sep = " ± " ) %>%
  select(Site.x,Soil_carbon_per_year_since_rehab,Plant_carbon_per_year_since_rehab, Combined_carbon_accumulation,CO2_eq_per_ha_per_year )

flextable(av_soil_plant_stock_table) #Table to copy to word
write.csv(av_soil_plant_stock_table, file = "CarbonAccTable.csv", row.names = F)##Table in excel.

#Overall Average to add on to Table 1:
av_soil_plant_stock2 <- soil_plant_stock %>%
  mutate(stock2 = Soil_Stock_Per_Year+ Plant_Stock_Per_Year,
         CO2_eq_both = stock2 * 3.67) %>%
  group_by(SiteRenamed) %>%
  summarise(AV_soil_stock = round(mean(Soil_Stock_Per_Year, na.rm = T),3),
            N_soil_stock = length(Soil_Stock_Per_Year),
            SD_soil_stock = sd(Soil_Stock_Per_Year),
            SE_soil_stock = round(SD_soil_stock/sqrt(N_soil_stock),3),
            
            AV_plant_stock = round(mean(Plant_Stock_Per_Year, na.rm = T),1),
            N_plant_stock = length(Plant_Stock_Per_Year),
            SD_plant_stock = sd(Plant_Stock_Per_Year),
            SE_plant_stock = round(SD_plant_stock/sqrt(N_plant_stock),1),
            
            AV_stock2 = round(mean(stock2, na.rm = T),1),
            N_stock2 = length(stock2),
            SD_stock2 = sd(stock2),
            SE_stock2 = round(SD_stock2/sqrt(N_stock2),1),
            
            AV2_CO2 = round(mean (CO2_eq_both, na.rm = T),1),
            N2_CO2 = length(CO2_eq_both),
            SD2_CO2 = sd(CO2_eq_both),
            SE2_CO2 = round(SD2_CO2/sqrt(N2_CO2),1))
View(av_soil_plant_stock2)


#TABLE1_Established sites part:=====
merge_year <- select(SiteTreat, Site,SiteYear)

#Soil Carbon Per Stand Age in Established cores only:
soil_Established <- NewDATA2 %>%
  select( Treatment,SiteYear, CarbonStockTillRehab_Mgha, Site_Core, Site, SiteRenamed)  %>%
  filter (SiteRenamed == "Established") %>%  #filter to look at rehab sites only
  filter  (Treatment != "Saltmarsh_natural") %>%
  group_by(Site, SiteYear, SiteRenamed, Site_Core) %>% #grouping by core
  summarise(TotalCarbonStockPerCore = sum(CarbonStockTillRehab_Mgha,na.rm=T))%>% #Add-up all slices 
  mutate(Soil_Stand_Age = 2017 - as.numeric((as.character(SiteYear))),
         Soil_Stock_Per_Year = TotalCarbonStockPerCore/Soil_Stand_Age )

#Plant Carbon Per Stand Age in Established cores only:
plant_Established <- select (aa, Site,Subplot_Name, Treatment2, Total_Aboveground_Biomass_kg_100m2) %>%
  filter(Treatment2 == "Remnant") %>% #later changed to Established
  left_join(merge_year,by = "Site") %>%
  mutate(Total_Aboveground_Biomass_Mg_ha = Total_Aboveground_Biomass_kg_100m2 / 10,  #1 kg/100m2 = 0.1 tonnes/ha
         Plant_C = 0.464* Total_Aboveground_Biomass_Mg_ha) %>% #%>%  #1 kg/100m2 = 0.1 tonnes/ha
  mutate(Plant_Stand_Age = 2017 - as.numeric((as.character(SiteYear))),
         Plant_Stock_Per_Year = Plant_C/Plant_Stand_Age,
         Site_Core = Subplot_Name) #Adding Site-Core column to match belowground data 

#Join plant and soil stock per year and computer CO2 equivalents:
soil_plant_stock_Established <- left_join(soil_Established,plant_Established, by = "Site_Core")#

#Mean +- SE soil_stock,plant_stock, combined and combined CO2 equivalents:
av_soil_plant_stock3 <- soil_plant_stock_Established %>%
  mutate(stock2 = Soil_Stock_Per_Year + Plant_Stock_Per_Year,
         CO2_eq_both = stock2 * 3.67) %>%
  group_by(Site.x) %>%
  summarise(AV_soil_stock = round(mean(Soil_Stock_Per_Year, na.rm = T),1),
            N_soil_stock = length(Soil_Stock_Per_Year),
            SD_soil_stock = sd(Soil_Stock_Per_Year),
            SE_soil_stock = round(SD_soil_stock/sqrt(N_soil_stock),1),
            
            AV_plant_stock = round(mean(Plant_Stock_Per_Year, na.rm = T),1),
            N_plant_stock = length(Plant_Stock_Per_Year),
            SD_plant_stock = sd(Plant_Stock_Per_Year),
            SE_plant_stock = round(SD_plant_stock/sqrt(N_plant_stock),1),
            
            
            AV_stock2 = round(mean(stock2, na.rm = T),1),
            N_stock2 = length(stock2),
            SD_stock2 = sd(stock2),
            SE_stock2 = round(SD_stock2/sqrt(N_stock2),1),
            
            AV2_CO2 = round(mean (CO2_eq_both, na.rm = T),1),
            N2_CO2 = length(CO2_eq_both),
            SD2_CO2 = sd(CO2_eq_both),
            SE2_CO2 = round(SD2_CO2/sqrt(N2_CO2),1))

#Select data for table and join AV & SE columns together with  " ± " sign:
av_soil_plant_stock_table3 <- select(av_soil_plant_stock3,
                                     Site.x,
                                     AV_soil_stock, SE_soil_stock,
                                     AV_plant_stock,SE_plant_stock,
                                     AV_stock2,     SE_stock2,
                                     AV2_CO2,       SE2_CO2) %>%
  unite("Soil_carbon_per_year_since_rehab", c("AV_soil_stock", "SE_soil_stock"), sep = " ± " ) %>%
  unite("Plant_carbon_per_year_since_rehab", c("AV_plant_stock", "SE_plant_stock"), sep = " ± " ) %>%
  unite("Combined_carbon_accumulation", c("AV_stock2", "SE_stock2"), sep = " ± " ) %>%
  unite("CO2_eq_per_ha_per_year", c("AV2_CO2", "SE2_CO2"), sep = " ± " ) %>%
  select(Site.x,Soil_carbon_per_year_since_rehab,Plant_carbon_per_year_since_rehab, Combined_carbon_accumulation,CO2_eq_per_ha_per_year )

flextable(av_soil_plant_stock_table3) #Table to copy to word
#write.csv(av_soil_plant_stock_table3, file = "CarbonAccTableEstablished.csv", row.names = F)

#OVerall Average to add on to Table 1:
av_soil_plant_stock3 <- soil_plant_stock_Established  %>%
  mutate(stock2 = Soil_Stock_Per_Year+ Plant_Stock_Per_Year,
         CO2_eq_both = stock2 * 3.67) %>%
  group_by(SiteRenamed) %>%
  summarise(AV_soil_stock = round(mean(Soil_Stock_Per_Year, na.rm = T),3),
            N_soil_stock = length(Soil_Stock_Per_Year),
            SD_soil_stock = sd(Soil_Stock_Per_Year),
            SE_soil_stock = round(SD_soil_stock/sqrt(N_soil_stock),3),
            
            AV_plant_stock = round(mean(Plant_Stock_Per_Year, na.rm = T),1),
            N_plant_stock = length(Plant_Stock_Per_Year),
            SD_plant_stock = sd(Plant_Stock_Per_Year),
            SE_plant_stock = round(SD_plant_stock/sqrt(N_plant_stock),1),
            
            AV_stock2 = round(mean(stock2, na.rm = T),1),
            N_stock2 = length(stock2),
            SD_stock2 = sd(stock2),
            SE_stock2 = round(SD_stock2/sqrt(N_stock2),1),
            
            AV2_CO2 = round(mean (CO2_eq_both, na.rm = T),1),
            N2_CO2 = length(CO2_eq_both),
            SD2_CO2 = sd(CO2_eq_both),
            SE2_CO2 = round(SD2_CO2/sqrt(N2_CO2),1))

View(av_soil_plant_stock3)
