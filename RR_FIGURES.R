#LOAD Data (FIGURE 2):=========
#To compute CAR gC/m2/yr (Carbon Accretion Rate):
#chose cores where average value for MAR using the CF.CS model is produced,
#multiply this by the % C in each section to obtain the CAR
#MAR values (g/cm2/y) are based on file sent from Pere: "Pb210 dating Cores Mangrove Rehab Carnell 2019"

library(tidyverse)
library(gridExtra)

aa <- read.csv("RR_Plant.csv") #Ex aboveground.csv
bb <- read.csv("RR_Soil.csv") #All Data
SiteTreat <- read.csv("RR_SiteTreatment_PC.csv")# Treatments to join to All Data
#We decided to split Rehabilitated site into two categories >25 and <25 years old (colun SiteRenamed_25y):
SiteTreat

bb <- bb %>%  filter(Treatment != "Saltmarsh_natural")#Remove this habitat, not assessed
bb$Site <- factor(bb$Site) #Remove the extra factor level of SNND
SitesToMerge <- select(SiteTreat, Site, SiteYearNumeric,Site_short_old)#We need descriptors for sites from SiteTreat file.
SitesToMerge


#PLOT CAR (burial_boxplot) :=========
#Create dataset off Pere's file above. MAR's units = g/cm2/y
MAR <- data.frame( Site = c("MNND","SNND","MNDC","MDBP",
                            "MDSD","MR1991","MR1992","MRAP",
                            "MRCH1","MRCH2","MRPL","MDTUCK", "MNPL"),
                   MAR = c(0.095, 0.096, NA,     0, 
                           0,    0.0324 ,0.076,0.32,
                           NA,  0.16,NA, NA   ,0.045),
                   MAR_SE = c(0.008,0.016, NA, 0,
                              0, 0.0010,0.005,0.11,
                              NA,0.02,NA, NA, 0.002))
MAR

#Join Sites with MAR data:
SiteTreatMAR <- left_join(SiteTreat,MAR, by = "Site")
SiteTreatMAR#list sites and corresponding treatments

#CAR = C-stock to horizon / Stand_Age,
NewDATA <- bb
NewDATA$C_percent <- ifelse(NewDATA$C_perc == 0, 0.001, NewDATA$C_perc)#convert 0 into 0.001 to run log-models if any
NewDATA$SliceLength.cm <- (NewDATA$Depth_to - NewDATA$Depth_from) #cm
NewDATA$PipeDiameter.cm <- 5 #Diameter of coring pipes was 5 cm
NewDATA$SampleVolume.cm3 <- (pi*(NewDATA$PipeDiameter.cm/2)^2)*NewDATA$SliceLength.cm  #slice volume
NewDATA$dry_bulk_density.gcm3_corrected <- NewDATA$Dry_bulk_density_gcm3 * NewDATA$Lab_Compaction_Correction_Value ##Correct Soil data for compaction and #Compute corrected C-stock (Off Bulk Density):
NewDATA$CarbonDensity.gcm3 <- NewDATA$dry_bulk_density.gcm3_corrected * NewDATA$C_percent/100
NewDATA$CarbonStock.Mgha <- ((NewDATA$CarbonDensity.gcm3  * 100) * NewDATA$SliceLength.cm )


#Cut slices at the depth where rehab started (DepthAtRehab_cm):
NewDATA2 <- left_join (NewDATA, SiteTreat, by = "Site") %>%
  mutate(KeepThrow = ifelse(DepthTo_SinceRehabilitated_cm >= Depth_from, "keep", "throw")) %>% #keep slices data are > Depth_from
  filter(KeepThrow=="keep") %>% #keep the "keep"
  transform (DepthAtRehab_cm = ifelse(Depth_to <= DepthTo_SinceRehabilitated_cm,  #Cut to the length of DepthTo_SinceRehabilitated_cm
                                      Depth_to, DepthTo_SinceRehabilitated_cm)) %>% #Keep slice below horizon, if above horizon cut to horizon depth
  mutate (SliceAtRehab_cm = DepthAtRehab_cm - Depth_from) %>% #lenght of slice at cores up to DepthTo_SinceRehabilitated_cm
  mutate (CarbonStockTillRehab_Mgha = CarbonDensity.gcm3  * 100 * SliceAtRehab_cm) #Soilc C stock in each cores till DepthTo_SinceRehabilitated_cm


#Compute Soil_CAR (Stock to Rehab Horizon / Stand_Age):
CAR_Rehab_Established <- NewDATA2 %>%
  select (Treatment,SiteYearNumeric, CarbonStockTillRehab_Mgha, Site_Core, Site, SiteRenamed_25y)  %>%
  filter  (Treatment != "Saltmarsh_natural") %>%
  group_by(Site, SiteYearNumeric, SiteRenamed_25y, Site_Core) %>% #grouping by core
  summarise(SoilStockTillRehabHorizon = sum(CarbonStockTillRehab_Mgha,na.rm=T))%>% #Add-up all slices per core
  mutate(Soil_Stand_Age = 2017 - as.numeric((as.character(SiteYearNumeric))), #Compute Stand_Age
         CAR_MgHaYear = SoilStockTillRehabHorizon/Soil_Stand_Age )  #Compute CAR (Soil_CAR)

CAR_Rehab_Established

CAR_Rehab_Established_short <- CAR_Rehab_Established %>% 
  select(Site,SiteYearNumeric, SiteRenamed_25y,CAR_MgHaYear ) %>%
  filter(SiteRenamed_25y != "Converted")

#Relevel:
CAR_Rehab_Established_short $ SiteRenamed_25y <- factor(CAR_Rehab_Established_short$SiteRenamed_25y,
                                                        levels = c("Established","Rehabilitated_young", "Rehabilitated_old"))
levels(CAR_Rehab_Established_short2 $SiteRenamed_25y)

burial_boxplot <- ggplot(CAR_Rehab_Established_short, aes(x= SiteRenamed_25y,y= CAR_MgHaYear)) +
  labs(x = "",y=bquote("Soil burial rate " (Mg*~ha^-1 ~y^-1)))+
  geom_boxplot(outlier.shape = NULL) +
  geom_jitter()+  
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=12, color = "black",angle=45),
        axis.text.y=element_text(size=15, color = "black"),
        axis.title.y=element_text(size=20, color = "black"),
        legend.position = "none")

burial_boxplot


#CAR averages 4 Results=====
#Sum up for the Results section in the MAnuscript:
CAR_AV <- CAR_Rehab_Established_short %>%
  group_by(SiteRenamed_25y) %>%
  summarise(AV = mean(CAR_MgHaYear, nr.rm=T),
            N = n(),
            SD = sd(CAR_MgHaYear),
            SE = SD/sqrt(N))

CAR_AV
SiteRenamed_25y        AV     N    SD     SE
1 Established         1.22     12 0.169 0.0488
2 Rehabilitated_young 2.62      8 1.35  0.476 
3 Rehabilitated_old   0.666     8 0.291 0.103 

#PLOT Plant_C (Plant_Carbon_boxplot)======
#DATA:
Plant_Carbon<- select (aa, Site, Treatment2, Total_Aboveground_Biomass_kg_100m2) %>%
  
  mutate(Total_Aboveground_Biomass_Mg_ha = Total_Aboveground_Biomass_kg_100m2 / 10,  #1 kg/100m2 = 0.1 tonnes/ha
         Plant_C = 0.464* Total_Aboveground_Biomass_Mg_ha ) %>%  #1 kg/100m2 = 0.1 tonnes/ha
  
  left_join(SiteTreat,by = "Site") %>% # To get new site names (SiteRenamed)
  mutate (Stock = "Plant") %>%
  left_join(SitesToMerge, by = "Site")

Plant_Carbon


#Compute mean of plant stock in remnant to draw a dotted line:
Established_AV <- filter(Plant_Carbon, SiteRenamed == "Established") %>%
  select(Plant_C,SiteRenamed) %>% 
  group_by(SiteRenamed) %>%
  summarise(remnant_plant = mean(Plant_C, na.rm=T))

Established_AV #146

#PLOT:
#Re-level to show Converted last on the plot:
levels(as.factor((Plant_Carbon$SiteRenamed_25y)))
Plant_Carbon$SiteRenamed_25y <- factor(Plant_Carbon$SiteRenamed_25y,
                                       levels = c("Established", "Rehabilitated_young", "Rehabilitated_old","Converted"))

Plant_Carbon_boxplot <- ggplot(Plant_Carbon, aes(x=SiteRenamed_25y, y= Plant_C))+
  labs(x = "",y =bquote("Plant organic carbon stock " (Mg*~ha^-1 ~y^-1)))+
  geom_boxplot(outlier.shape = NULL) +
  geom_jitter()+  #aes(color=Site_short_old) add color/dots for sites
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=12, color = "black",angle=45),
        axis.text.y=element_text(size=15, color = "black"),
        axis.title.y=element_text(size=20, color = "black"))

Plant_Carbon_boxplot

#Plant_Carbon averages 4 Results=====
#Sum up for the Results section in the MAnuscript:
PLANT_AV <- Plant_Carbon %>%
  group_by(SiteRenamed_25y) %>%
  summarise(AV = mean(Plant_C, nr.rm=T),
            N = n(),
            SD = sd(Plant_C),
            SE = SD/sqrt(N))

PLANT_AV

#Towards Above ground Carbon stocks Results:
round(66.3 / 146 *100, 0)  #Plant Carbon was 45% young lower compared to established
round (35.7  / 146 *100, 0)#Plant Carbon was 45% young lower compared to established

PLANT_sites <- Plant_Carbon %>%
  group_by(Site) %>%
  summarise(AV = mean(Plant_C, nr.rm=T),
            N = n(),
            SD = sd(Plant_C),
            SE = SD/sqrt(N))

PLANT_sites
Site   SiteRenamed_25y        AV     N    SD    SE
1 MDBP   Converted             0       1 NA    NA   
2 MDSD   Converted             0       1 NA    NA   
3 MDTUCK Converted             0       1 NA    NA   
4 MNDC   Established         215.      4 57.8  28.9 
5 MNND   Established          93.5     4 32.8  16.4 
6 MNPL   Established         129.      4 12.7   6.37
7 MR1991 Rehabilitated_old    36.2     4 15.7   7.83
8 MR1992 Rehabilitated_young  84.9     4 13.1   6.56
9 MRAP   Rehabilitated_young  25.4     3  2.33  1.35
10MRCH1  Rehabilitated_old    20.0     4  8.31  4.16
11MRCH2  Rehabilitated_old    51.0     4 10.8   5.38
12 MRPL                       78.2     4 12.8   6.40

#Soil_C PLOT (soil_till_rehab_boxplot):========
#Get CarbonStock.Mgha corrected for soil compaction:
NewDATA <- bb
NewDATA$C_percent <- ifelse(NewDATA$C_perc == 0, 0.001, NewDATA$C_perc)#convert 0 into 0.001 to run log-models if any
NewDATA$SliceLength.cm <- (NewDATA$Depth_to - NewDATA$Depth_from) #cm
NewDATA$PipeDiameter.cm <- 5 #Diameter of coring pipes was 5 cm
NewDATA$SampleVolume.cm3 <- (pi*(NewDATA$PipeDiameter.cm/2)^2)*NewDATA$SliceLength.cm  #slice volume
NewDATA$dry_bulk_density.gcm3_corrected <- NewDATA$Dry_bulk_density_gcm3 * NewDATA$Lab_Compaction_Correction_Value
NewDATA$CarbonDensity.gcm3 <- NewDATA$dry_bulk_density.gcm3_corrected * NewDATA$C_percent/100
NewDATA$CarbonStock.Mgha <- ((NewDATA$CarbonDensity.gcm3  * 100) * NewDATA$SliceLength.cm )

#C-stock (Stand Age) till Depth of Rehab (DepthTo_SinceRehabilitated_cm)
NewDATA2 <- left_join (NewDATA, SiteTreat, by = "Site") %>%
  mutate(KeepThrow = ifelse(DepthTo_SinceRehabilitated_cm >= Depth_from, "keep", "throw")) %>% #keep slices data are > Depth_from
  filter(KeepThrow=="keep") %>% #keep the "keep"
  
  transform (DepthAtRehab_cm = ifelse(Depth_to <= DepthTo_SinceRehabilitated_cm,  #Cut to the length of DepthTo_SinceRehabilitated_cm
                                      Depth_to, DepthTo_SinceRehabilitated_cm)) %>%
  
  mutate (SliceAtRehab_cm = DepthAtRehab_cm - Depth_from) %>% #length of slice at cores up to DepthTo_SinceRehabilitated_cm, for established sites set to 50cm
  mutate (CarbonStockTillRehab_Mgha = CarbonDensity.gcm3  * 100 * SliceAtRehab_cm) #Soils C stock in core slices till DepthTo_SinceRehabilitated_cm




soil_stock_till_rehab <- select(NewDATA2, Site,SiteYearNumeric, Site_short,Site_Core, Treatment,Depth_to, Depth_Range,
                                Site_Core, Carbon_stock_in_section_Mg_Cha,Treatment2,CarbonStock.Mgha,
                                CarbonStockTillRehab_Mgha)%>%
  filter  (Treatment != "Saltmarsh_natural") %>% #not looking at saltmarsh in this study, mangroves only
  mutate (Stock = "Soil") %>% #Assigning additional category, indicating it is soil stock
  group_by(Site_Core) %>% #grouping by core till 100 cm
  summarise(TotalCarbonStockPerCore = sum(CarbonStockTillRehab_Mgha))%>% #Add-up all slices 
  separate(Site_Core, into = c("Site","CoreNumber"), sep = "_") %>%
  mutate (Stock = "Soil_Till_Rehab")%>%
  left_join(SiteTreat, by = "Site" ) %>% #Add columns from SiteTreat 
  mutate(MyColor = ifelse(Site=="MRCH1", "lightgrey","black"))#change Ageiceras-dominated site of MRCH1 to grey if present.

soil_stock_till_rehab
#flextable(soil_till_rehab) #For Abstract/Results section
#write.csv(soil_till_rehab, file = "soil_till_rehab.csv", row.names = F)


#Soil C Averages 4 Results======
NewDATA_Converted <- left_join (NewDATA, SiteTreat, by = "Site")%>%
  filter(SiteRenamed_25y =="Converted" | SiteRenamed_25y =="Established")

soil_stock_converted <- select(NewDATA_Converted, Site,Depth_to, Site_Core, Treatment,
                                Site_Core,Treatment2,CarbonStock.Mgha)%>%
  
  filter(Depth_to <51) %>% # established/converted stock set to below 50 cm to enable comparison

  group_by(Site_Core) %>% #grouping by core till 100 cm
  summarise(TotalCarbonStockPerCore = sum(CarbonStock.Mgha))%>% #Add-up all slices 
  separate(Site_Core, into = c("Site","CoreNumber"), sep = "_") %>%
  mutate (Stock = "Soil_Till_Rehab")%>%
  left_join(SiteTreat, by = "Site" ) #Add columns from SiteTreat 


soil_stock_converted_summary <- soil_stock_converted %>%
  group_by(SiteRenamed) %>%
  summarise(AV = mean(TotalCarbonStockPerCore, nr.rm=T),
            N = n(),
            SD = sd(TotalCarbonStockPerCore),
            SE = SD/sqrt(N))

soil_stock_converted_summary
SiteRenamed    AV     N    SD    SE
1 Converted    71.5     9  18.7  6.24
2 Established 122.     12  16.9  4.88
round(71.5/122.4 *100,1) # Converted C-stock is on average 58.6% lower than Established
122.4- 71.5

#PLOT Soil_C:
unique(soil_stock_till_rehab$SiteRenamed_25y)#"Established","Rehabilitated_old","Rehabilitated_young"
#RE-level:
soil_stock_till_rehab$SiteRenamed_25y <- factor(soil_stock_till_rehab$SiteRenamed_25y,levels = c("Established","Rehabilitated_young","Rehabilitated_old"))

soil_till_rehab_boxplot <- ggplot(soil_stock_till_rehab,
                                  aes(x= SiteRenamed_25y, y= TotalCarbonStockPerCore )) +
  labs(x = "",y =bquote("Soil organic carbon stock " (Mg*~ha^-1)))+
  geom_boxplot() +
  geom_jitter()+  #aes(color=Site_short_old) add color/dots for sites
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=12, color = "black",angle=45),
        axis.text.y=element_text(size=15, color = "black"),
        axis.title.y=element_text(size=20, color = "black"))

soil_till_rehab_boxplot





#Combine Plots (Plant_Carbon_boxplot, soil_till_rehab_boxplot, burial_boxplot):=======
grid.arrange(Plant_Carbon_boxplot, soil_till_rehab_boxplot, burial_boxplot,
             ncol = 3)

#To save in higher resolution use arrangeGrob:
plots <- arrangeGrob(Plant_Carbon_boxplot, soil_till_rehab_boxplot, burial_boxplot, nrow=1)
ggsave(plots, filename = "Fig2_600DPI_RehabHorizonBoxplot_25y_04july2023_v2.png",  width = 17, height = 8, dpi = 600)

