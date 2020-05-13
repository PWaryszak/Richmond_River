#LOAD Richmond River DATA:=====
library(tidyverse)
library(gridExtra)
library(flextable)

aa <- read.csv("RR_Plant.csv") #Ex aboveground.csv
bb <- read.csv("RR_Soil.csv") #All Data
SiteTreat <- read.csv("RR_SiteTreatment_PC.csv")# Treatments to join to All Data
bb <- bb %>%  filter(Treatment != "Saltmarsh_natural")#Remove this habitat, not assessed
bb$Site <- factor(bb$Site) #Remove the extra factor level of SNND

#Correct Soil data for compaction
#Compute corrected C-stock (Off Bulk Density, Correct Soil for compaction):
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

#Difference in means between all cores========

#Compute Mean corrected C-stock:
#Corrected C-stock is computed from percentage change in Depth_to after
#Accounting for compaction using Compaction Correction Value.
CorrectedDepthCarbon <- select(bb,Site, Site_short,Site_Core, Treatment,Depth_to, Depth_Range,
                               Site_Core, Carbon_stock_in_section_Mg_Cha,
                               Lab_Compaction_Correction_Value)%>%
  filter(Treatment != "Saltmarsh_natural") %>%
  filter(Depth_to <= 100 ) %>%
  group_by(Site_Core)%>%
  mutate(TotalPerCore = sum (Carbon_stock_in_section_Mg_Cha)) %>%
  mutate( DepthCorrected = Depth_to / Lab_Compaction_Correction_Value, #Depth_to = recorded depth of core in the lab
          DepthChange = DepthCorrected-Depth_to,
          DepthChangePerc = 100-(DepthChange/Depth_to *100), #Percent increase in depth
          CorrectedCarbonStock = TotalPerCore * DepthChangePerc/100) %>% #Percent decrease in Carbon Stock when cut at 50cm
  group_by(Site) %>% #to sum total Carbon Stock per core.
  summarise(AV = mean(CorrectedCarbonStock, na.rm = T),
            SD = sd(CorrectedCarbonStock, na.rm = T),
            N = length(CorrectedCarbonStock),
            SE = SD / sqrt(N)) %>%
  mutate (Stock = "Belowground")

CorrectedDepthCarbon

#Three Means=====
#Compute total C-stock till 100 cm deep fir each of three site types:
three_means <- select(NewDATA, Site, Site_short,Site_Core, Treatment,Depth_to, Depth_Range,
                              Site_Core, Carbon_stock_in_section_Mg_Cha,Treatment2,CarbonStock.Mgha,
                              Lab_Compaction_Correction_Value)%>%
  filter  (Treatment != "Saltmarsh_natural") %>%
  filter (Depth_to <= 100 ) %>% #keeping only top 100 cm of each core
  group_by(Site_Core) %>% #grouping by core till 100 cm
  summarise(TotalCarbonStockPerCore = sum(CarbonStock.Mgha)) %>%   #Add-up all slices 
  separate(Site_Core, into = c("Site","CoreNumber"), sep = "_", remove = F) %>%
  left_join(SiteTreat, by = "Site" ) %>%
  
  group_by(SiteRenamed)%>%
  summarise(AV = mean(TotalCarbonStockPerCore, na.rm = T),
            SD = sd(TotalCarbonStockPerCore, na.rm = T),
            N = length(TotalCarbonStockPerCore),
            SE = SD / sqrt(N))

View(three_means)
#SiteRenamed      AV    SD     N    SE
# Converted      92.5  33.6     9  11.2
# Established   186.   48.7    12  14.1
# Rehabilitated 169.   63.8    24  13.0


# % Difference of Converted to One Mean of Established===========
#Compute average soil stock for Established site (abline):
AV_soil_stock_Established <- left_join(NewDATA,SiteTreat, by = "Site")%>%
  select( Site,SiteYear, Site_Core, SiteRenamed,Depth_to,CarbonStock.Mgha)%>%
  filter  (SiteRenamed == "Established") %>% #Established only
  filter(Depth_to <= 100) %>% #down to 100 cm
  mutate (Stock = "Soil") %>% #Assigning additional category, indicatig it is soil stock
  group_by(Site_Core) %>% #grouping by core till 100 cm
  summarise(TotalCarbonStockPerCore = sum(CarbonStock.Mgha))%>% #Add-up all slices 
  summarise(AV = mean(TotalCarbonStockPerCore, na.rm = T),
            SD = sd(TotalCarbonStockPerCore, na.rm = T),
            N = length(TotalCarbonStockPerCore),
            SE = SD / sqrt(N)) %>%
  select(AV,SE) %>%
  mutate(SiteYear = "1970",
         SiteRenamed = "Established")

AV_soil_stock_Established #One value =  186.  14.1 1970     Established
AV_Established <- as.numeric(AV_soil_stock_Established[1,1])

#Soil stock percent in rehab in relation to one mean value from established site above:
percent_diff <- left_join(NewDATA, SiteTreat, by = "Site" ) %>%
  select( Treatment, CarbonStock.Mgha,Depth_to, Site_Core, Site, SiteRenamed)  %>%
  filter  (Treatment != "Saltmarsh_natural") %>%
  mutate (Stock = "Soil") %>% #Assigning additional category, indicatig it is soil stock
  group_by(Site_Core, SiteRenamed) %>% #grouping by core till 100 cm
  summarise(TotalCarbonStockPerCore = sum(CarbonStock.Mgha))%>% #Add-up all slices 
  filter(SiteRenamed == "Converted") %>%
  mutate(MeanStockEstablished = AV_Established )%>%
  mutate(PercentDiff = TotalCarbonStockPerCore/MeanStockEstablished*100)%>%
  group_by(SiteRenamed) %>%
  summarise(AV = mean(PercentDiff, na.rm = T),
            SD = sd(PercentDiff, na.rm = T),
            N = length(PercentDiff),
            SE = SD / sqrt(N))

percent_diff 
#SiteRenamed    AV    SD     N    SE
# Converted    44.9  11.1     9  3.71

# % Difference of Rehabilitated to One Mean of Established===========
#Compute average soil stock for Established site (abline):
AV_plant_stock_Established <- left_join(aa,SiteTreat, by = "Site")%>%
  select( Site,SiteYear, SiteRenamed,Total_Aboveground_Biomass_kg_100m2)%>%
  filter  (SiteRenamed == "Established") %>% #Established only
  mutate (Stock = "Plant",
          Plant_C = Total_Aboveground_Biomass_kg_100m2 * 0.464/10) %>% 
  group_by(SiteRenamed) %>% #grouping by core till 100 cm
  summarise(AV = mean(Plant_C, na.rm = T),
            SD = sd(Plant_C, na.rm = T),
            N = length(Plant_C),
            SE = SD / sqrt(N)) 

AV_plant_stock_Established  #146.  64.0    12  18.5

AV_Established_plant <- as.numeric(AV_plant_stock_Established [1,"AV"])
AV_Established_plant#145.8572

#Soil stock percent in rehab in relation to one mean value from established site above:
percent_diff <- left_join(aa, SiteTreat, by = "Site" )%>%
  select( Treatment2, Site, SiteRenamed,Total_Aboveground_Biomass_kg_100m2)  %>%
  mutate (Stock = "Plant",
          Plant_C = Total_Aboveground_Biomass_kg_100m2 * 0.464/10) %>% 
  filter(SiteRenamed == "Rehabilitated") %>%
  mutate(MeanStockEstablished = AV_Established_plant )%>%
  mutate(PercentDiff = Plant_C/MeanStockEstablished*100)%>%
  group_by(SiteRenamed) %>%
  summarise(AV = mean(PercentDiff, na.rm = T),
            SD = sd(PercentDiff, na.rm = T),
            N = length(PercentDiff),
            SE = SD / sqrt(N))

percent_diff 
#SiteRenamed    AV    SD     N    SE
#Rehabilitated  35.6  18.5    22  3.95

#No SE % Differences====
#Compute percentage difference between converted and established:
three_means[1,2]/three_means[3,2] *100 #54.61885%
three_means[3,2]/three_means[1,2] #1.8 times lower in converted

#Carbon Accumulation Rates======
#To check numbers of carbon accumulation in Plant and Soils go
#Github: https://github.com/PWaryszak/Richmond_River/blob/master/RR_Table1.R

