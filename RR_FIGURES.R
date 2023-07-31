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
#We decided to split Rehabilitated site into two categories >25 and <25 years old (column SiteRenamed_25y):
SiteTreat

bb <- bb %>%  filter(Treatment != "Saltmarsh_natural")#Remove this habitat, not assessed
bb$Site <- factor(bb$Site) #Remove the extra factor level of SNND
SitesToMerge <- select(SiteTreat, Site, SiteYearNumeric,Site_short_old,
                       DepthTo_SinceRehabilitated_cm, SiteRenamed_25y)#We need descriptors for sites from SiteTreat file.
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
NewDATA2 <- left_join (NewDATA, SitesToMerge, by = "Site") %>% #SitesToMerge -> DepthTo_SinceRehabilitated_cm was set too high to show CAR therein was close to zero.
  mutate(KeepThrow = ifelse(DepthTo_SinceRehabilitated_cm >= Depth_from, "keep", "throw"))%>% #keep slices data are > Depth_from
  filter(KeepThrow=="keep" ) %>% #keep the "keep"
  transform (DepthAtRehab_cm = ifelse(Depth_to <= DepthTo_SinceRehabilitated_cm,  #Cut to the length of DepthTo_SinceRehabilitated_cm
                                      Depth_to, DepthTo_SinceRehabilitated_cm)) %>% #Keep slice below horizon, if above horizon cut to horizon depth
  mutate (SliceAtRehab_cm = DepthAtRehab_cm - Depth_from) %>% #lenght of slice at cores up to DepthTo_SinceRehabilitated_cm
  mutate (CarbonStockTillRehab_Mgha = CarbonDensity.gcm3  * 100 * SliceAtRehab_cm) #Soil C stock in each cores till DepthTo_SinceRehabilitated_cm

names(NewDATA2)

#Compute Soil_CAR (Stock to Rehab Horizon / Stand_Age):
CAR_Rehab_Established <- NewDATA2 %>%
  select (Treatment,SiteYearNumeric, CarbonStockTillRehab_Mgha, Site_Core, Site, SiteRenamed_25y)  %>%
  filter  (Treatment != "Saltmarsh_natural") %>%
  group_by(Site, SiteYearNumeric, SiteRenamed_25y, Site_Core) %>% #grouping by core
  summarise(SoilStockTillRehabHorizon = sum(CarbonStockTillRehab_Mgha,na.rm=T))%>% #Add-up all slices per core
  mutate(Soil_Stand_Age = 2017 - as.numeric((as.character(SiteYearNumeric))), #Compute Stand_Age
         CAR_MgHaYear = SoilStockTillRehabHorizon/Soil_Stand_Age )  #Compute CAR (Soil_CAR)

View(CAR_Rehab_Established)

CAR_Rehab_Established_short <- CAR_Rehab_Established %>% 
  select(Site,SiteYearNumeric, SiteRenamed_25y,CAR_MgHaYear ) #%>%filter(SiteRenamed_25y != "Converted")

View(CAR_Rehab_Established_short )

#Converted sites had no CAR. Ser CAR in Converted to zero:
CAR_Rehab_Established_short$CAR2 <- ifelse(CAR_Rehab_Established_short$SiteRenamed_25y == "Converted", 0 ,CAR_Rehab_Established_short$CAR_MgHaYear)

#Relevel CAR_Rehab_Established_short:
CAR_Rehab_Established_short $ SiteRenamed_25y <- factor(CAR_Rehab_Established_short$SiteRenamed_25y,
                                              levels = c("Converted", "Established", "Rehabilitated_young", "Rehabilitated_old"))
levels(CAR_Rehab_Established_short $SiteRenamed_25y) ####"Converted" ,"Rehabilitated_young", "Rehabilitated_old", "Established"  

burial_boxplot <- ggplot(CAR_Rehab_Established_short, aes(x= SiteRenamed_25y, y= CAR2)) +
  labs(x = "",y=bquote("Soil burial rate " (Mg*~ha^-1 ~y^-1)))+
  geom_boxplot(outlier.shape = NULL) +
  geom_jitter()+  
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=12, color = "black",angle=45),
        axis.text.y=element_text(size=15, color = "black"),
        axis.title.y=element_text(size=20, color = "black"),
        legend.position = "none")

burial_boxplot





---------------------------------------------
#Inset of CAR from Rehab young sites only:
rehab_young <- CAR_Rehab_Established_short[CAR_Rehab_Established_short$SiteRenamed_25y == "Rehabilitated_young",]

burial_boxplot_young <- ggplot(rehab_young, aes(x= SiteRenamed_25y, y= CAR2)) +
  labs(x = "",y=bquote("Soil burial rate " (Mg*~ha^-1 ~y^-1)))+
  geom_boxplot(outlier.shape = NULL) +
  geom_jitter(aes(fill=Site,shape=Site),size=6)+  
  scale_shape_manual(values = c(24,24))+
  theme_classic()+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=18, colour = "black"),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20),
        legend.position =   "bottom" ,  # c(.85, .85),
        legend.text = element_text(size = 12),
        legend.title = element_text(face = "italic", size=14),
        plot.title = element_text(hjust = 0.5,lineheight=1.2, face="bold",size=22))


burial_boxplot_young

#CAR averages 4 Results=====
#Sum up for the Results section in the MAnuscript:
CAR_AV <- CAR_Rehab_Established_short %>%
  group_by(SiteRenamed_25y) %>%
  summarise(AV = mean(CAR_MgHaYear, nr.rm=T),
            N = n(),
            SD = sd(CAR_MgHaYear),
            SE = SD/sqrt(N)) %>%
  mutate(CO2_equivalent = AV * 3.67 )

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
  
  left_join(SitesToMerge,by = "Site") %>% # To get new site names (SiteRenamed)
  mutate (Stock = "Plant") #%>%  left_join(SitesToMerge, by = "Site")

Plant_Carbon


#Compute mean of plant stock in remnant to draw a dotted line:
Established_AV <- filter(Plant_Carbon, SiteRenamed_25y == "Established") %>%
  select(Plant_C,SiteRenamed_25y) %>% 
  group_by(SiteRenamed_25y) %>%
  summarise(remnant_plant = mean(Plant_C, na.rm=T))

Established_AV #146

#PLOT:
#Re-level to show Converted last on the plot:
unique(Plant_Carbon$SiteRenamed_25y)
Plant_Carbon$SiteRenamed_25y <- factor(Plant_Carbon$SiteRenamed_25y,
                                       levels = c("Converted","Established", "Rehabilitated_young", "Rehabilitated_old"))

Plant_Carbon_boxplot <- ggplot(Plant_Carbon, aes(x=SiteRenamed_25y, y= Plant_C))+
  labs(x = "",y =bquote("Plant organic carbon stock " (Mg*~ha^-1 ~y^-1)))+
  geom_boxplot(outlier.shape = NA) +
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
            SE = SD/sqrt(N)) %>%
  mutate(CO2_equivalent = AV * 3.67 )


PLANT_AV

#Towards Above ground Carbon stocks Results:
round(66.3 / 146 *100, 0)  #Plant Carbon was 45% young lower compared to established
round (35.7  / 146 *100, 0)#Plant Carbon was 24% old lower compared to established

PLANT_sites <- Plant_Carbon %>%
  group_by(Site,SiteRenamed_25y) %>%
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
12 MRPL Rehabilitated_young   78.2     4 12.8   6.40

#Soil_C PLOT (soil_till_rehab_boxplot):========
#Get CarbonStock.Mgha corrected for soil compaction:

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

View(soil_stock_till_rehab)
#flextable(soil_till_rehab) #For Abstract/Results section
#write.csv(soil_till_rehab, file = "soil_till_rehab.csv", row.names = F)


#Soil C Averages 4 Results (down to 50 cm deep)======
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
122.4- 71.5 #58.6% lower translates to 50.9 C-tonnes per hectare of difference.

#PLOT Soil_C:
unique(soil_stock_till_rehab$SiteRenamed_25y)#"Established","Rehabilitated_old","Rehabilitated_young"
#RE-level:
soil_stock_till_rehab$SiteRenamed_25y <- factor(soil_stock_till_rehab$SiteRenamed_25y,
                                                levels = c("Converted", "Established","Rehabilitated_young","Rehabilitated_old"))

soil_till_rehab_boxplot <- ggplot(soil_stock_till_rehab,
                                  aes(x= SiteRenamed_25y, y= TotalCarbonStockPerCore )) +
  labs(x = "",y =bquote("Soil organic carbon stock " (Mg*~ha^-1)))+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter()+  #aes(color=Site_short_old) add color/dots for sites
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=12, color = "black",angle=45),
        axis.text.y=element_text(size=15, color = "black"),
        axis.title.y=element_text(size=20, color = "black"))

soil_till_rehab_boxplot








# Fig2:Combine Plots (Plant_Carbon_boxplot, soil_till_rehab_boxplot, burial_boxplot):=======
grid.arrange(Plant_Carbon_boxplot, soil_till_rehab_boxplot, burial_boxplot,
             ncol = 3)

#To save in higher resolution use arrangeGrob:
plots <- arrangeGrob(Plant_Carbon_boxplot, soil_till_rehab_boxplot, burial_boxplot, nrow=1)
ggsave(plots, filename = "Fig2_600DPI_RehabHorizonBoxplot_31july2023_v1.png",  width = 17, height = 8, dpi = 600)



#SUBSET FIGURE (Established and Converted only to compare)==========
names(soil_stock_till_rehab)
names(CAR_Rehab_Established_short)

soil_till_rehab_boxplot2 <- ggplot(soil_stock_till_rehab[soil_stock_till_rehab$SiteRenamed_25y=="Converted" |soil_stock_till_rehab$SiteRenamed_25y=="Established",],
                                  aes(x= SiteRenamed_25y, y= TotalCarbonStockPerCore )) +
  labs(x = "",y =bquote("Soil organic carbon stock " (Mg*~ha^-1)))+
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous (limits= c(0,150))+
  geom_jitter()+  #aes(color=Site_short_old) add color/dots for sites
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=20, color = "black",angle=0),
        axis.text.y=element_text(size=20, color = "black"),
        axis.title.y=element_text(size=20, color = "black"))

soil_till_rehab_boxplot2


burial_boxplot2 <- ggplot(CAR_Rehab_Established_short[CAR_Rehab_Established_short$SiteRenamed_25y=="Converted" | CAR_Rehab_Established_short$SiteRenamed_25y=="Established",],
                          aes(x= SiteRenamed_25y,y= CAR2)) +
  labs(x = "",y=bquote("Soil burial rate " (Mg*~ha^-1 ~y^-1)))+
  geom_boxplot(outlier.shape = NULL) +
  geom_jitter()+  
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=20, color = "black",angle=0),
        axis.text.y=element_text(size=20, color = "black"),
        axis.title.y=element_text(size=20, color = "black"),
        legend.position = "none")

burial_boxplot2

#To save in higher resolution use arrangeGrob:
plots2 <- arrangeGrob(soil_till_rehab_boxplot2, burial_boxplot2, nrow=1)
ggsave(plots2, filename = "FigEXTRA_600DPI_EstablishedConverted_24July2023_v3.png",  width = 17, height = 8, dpi = 600)

----------------------------------------



#Fig3: BARPLOT of Established vs Converted 50cm deep:==========
#LOAD Richmond River DATA
library(tidyverse)
library(gridExtra)

aa <- read.csv("RR_Plant.csv") #Ex aboveground.csv
names(aa)
bb <- read.csv("RR_Soil.csv") #Ex belowground.csv
names(bb)

#Soil data (50cm deep):==========

#merge sites and corresponding treatments
SiteTreat <- read.csv("RR_SiteTreatment_PC.csv")#to join Treatments to 
names(SiteTreat)
SiteTreatShort <- select(SiteTreat,SiteRenamed, Site)

#Compute Mean Soil C-stock: Filter out sites down to 50cm deep:
NewDATA_Converted <- left_join (NewDATA2, SiteTreatShort, by = "Site")

soil_stock_50cm<- select(NewDATA_Converted, Site,Depth_to, Site_Core, Treatment,
                               Site_Core,CarbonStock.Mgha,Site_short, SiteRenamed) %>%
  
  filter(Depth_to <=50) %>% # established/converted stock set to below 50 cm to enable comparison
  
  group_by(Site_Core, Site, Site_short, SiteRenamed) %>% #grouping by core till 50 cm deep
  summarise(TotalCarbonStockPerCore = sum(CarbonStock.Mgha))%>% #Add-up all slices 
  
  separate(Site_Core, into = c("Site","CoreNumber"), sep = "_") 

soil_stock_50cm

soil_stock_50cm_summary <- soil_stock_50cm %>%
  group_by(SiteRenamed,Site,Site_short) %>%
  summarise(AV = mean(TotalCarbonStockPerCore, nr.rm=T),
            N = n(),
            SD = sd(TotalCarbonStockPerCore),
            SE = SD/sqrt(N)) %>%
  mutate (Stock = "Soil")

soil_stock_50cm_summary 

#Compute Plant C-Stock (mean +- SE):=========
#Summarise abovegorund carbon stock:
names(aa) #Shorten the aa file  and a missing columns as per below:
aa_a <- left_join(aa,SiteTreatShort, by = "Site") #aa dataset miss SiteRenamed column. Join with SiteTreat_a

plant_stock_summary<- select (aa_a, Site,  Total_Aboveground_Biomass_kg_100m2,Site_short,SiteRenamed) %>%
  mutate(Total_Aboveground_Biomass_Mg_ha = Total_Aboveground_Biomass_kg_100m2 / 10 *0.464 )%>% #1 kg/100m2 = 0.1 tonnes/ha, *0.464 conversion factor after Kauffman & Donato 2012 and Howard et al 2014)

  gather(key = treat, value = mass, Total_Aboveground_Biomass_Mg_ha) %>% 
  group_by(Site,Site_short,SiteRenamed) %>%
  summarise(AV=mean(mass, na.rm = T),
            SD=sd(mass),
            N = length(mass),
            SE= SD / sqrt(N)) %>%
  mutate (Stock = "Plant")

plant_stock_summary
plant_stock_summary[is.na(plant_stock_summary)] <- 0 #Replace NaN (4 Converted sites 4 plotting purpose) with zeros and join with site label (SiteTreat) dataset:

#Join/Plot above and below Stocks:========
ab <- rbind (plant_stock_summary,soil_stock_50cm_summary ) %>%
  mutate(AV= ifelse(Stock =="Plant",AV,AV * -1))%>% #Turn soil values into negative values
  mutate(project = "RR")

ab_Disturbed <- filter(ab, SiteRenamed == "Converted") #Use this for plotting Converted sites
ab_Disturbed
ab_Remnant <- filter(ab, SiteRenamed == "Established")#Use this for plotting Establishedsites
ab_Remnant

#Draw figure breaks to fit data in:
MyBreaks <- c(-150,-100, -50, 0, 100, 200,300 ,400,500,600)

#Plot Disturbed Plant biomass:
plot_Disturbed <- ggplot(ab_Disturbed, aes(x=Site_short, y=AV, fill = Stock))+
  geom_bar(position="identity", stat="identity")+
  geom_errorbar( aes(ymin= AV+SE, ymax = AV-SE), width=.4)+
  geom_hline(yintercept=0)+
  scale_y_continuous(breaks = MyBreaks,labels = abs(MyBreaks), limits = c(-150,300))+ #abs to remove negative values on y-axis below 0
  scale_fill_manual(values = c("darkgreen","lightblue"))+
  xlab("Site")+ ylab("")+
  ggtitle("Converted")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=12),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20),
        legend.position = c(.75, .85),
        legend.text = element_text(size = 12),
        legend.title = element_text(face = "italic", size=14),
        plot.title = element_text(hjust = 0.5,lineheight=1.2, face="bold",size=22))
plot_Disturbed

#Plot Remnant Plant biomass:
plot_Remnant <- ggplot(ab_Remnant, aes(x=Site_short, y=AV, fill = Stock))+
  geom_bar(position="identity", stat="identity")+
  geom_errorbar( aes(ymin= AV+SE, ymax = AV-SE), width=.4)+
  geom_hline(yintercept=0)+
  scale_y_continuous(breaks = MyBreaks,labels = abs(MyBreaks), limits = c(-150,300))+ #abs to remove negative values on y-axis below 0
  scale_fill_manual(values = c("darkgreen","lightblue"))+
  xlab("Site") + ylab(bquote("Organic carbon stock " (Mg*~ha^-1)))+
  ggtitle("Established")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=12),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20),
        
        legend.position = "none",
        legend.text = element_text(size = 12),
        legend.title = element_text(face = "italic",size=14),
        plot.title = element_text(hjust = 0.5,lineheight=1.2, face="bold",size=22))

plot_Remnant

grid.arrange(plot_Remnant,plot_Disturbed, ncol = 2) #Check the layout of multiple plots

plots2 <- arrangeGrob(plot_Remnant,plot_Disturbed, nrow=1)
ggsave(plots2, filename = "Fig3_600DPI_BARPLOT_AboveBelowStock_31july2023_ConvertedToCarbonOnly.png",  width = 17, height = 8, dpi = 600)




#INSET of Rehab young sites:==========
#Soil data (50cm deep):
#merge sites and corresponding treatments
SiteTreat <- read.csv("RR_SiteTreatment_PC.csv")#to join Treatments to 
names(SiteTreat)
SiteTreatShort_a <- select( SiteTreat, Site, SiteRenamed_25y,SiteYearNumeric)

#Compute Mean Soil C-stock: Filter out sites down to 50cm deep:
NewDATA_Converted <- left_join (NewDATA, SiteTreatShort_a, by = "Site")
names(NewDATA_Converted)

soil_stock_50cm<- select(NewDATA_Converted, Site,Depth_to, Site_Core, Treatment,
                         Site_Core,CarbonStock.Mgha,Site_short, SiteRenamed_25y) %>%
  
  filter(Depth_to <=50) %>% # established/converted stock set to below 50 cm to enable comparison
  
  group_by(Site_Core, Site, Site_short, SiteRenamed_25y) %>% #grouping by core till 50 cm deep
  summarise(TotalCarbonStockPerCore = sum(CarbonStock.Mgha))%>% #Add-up all slices 
  
  separate(Site_Core, into = c("Site","CoreNumber"), sep = "_") 

soil_stock_50cm

soil_stock_50cm_summary <- soil_stock_50cm %>%
  group_by(SiteRenamed_25y,Site,Site_short) %>%
  summarise(AV = mean(TotalCarbonStockPerCore, nr.rm=T),
            N = n(),
            SD = sd(TotalCarbonStockPerCore),
            SE = SD/sqrt(N)) %>%
  mutate (Stock = "Soil")

soil_stock_50cm_summary 

#Compute Plant C-Stock (mean +- SE):
#Summarise abovegorund carbon stock:
names(aa) #Shorten the aa file  and a missing columns as per below:
aa_a <- left_join(aa,SiteTreatShort_a, by = "Site") #aa dataset miss SiteRenamed_25y column. Join with SiteTreat_a
head(aa_a)

plant_stock_summary<- select (aa_a, Site, SiteYearNumeric , Total_Aboveground_Biomass_kg_100m2,Site_short,SiteRenamed_25y) %>%
  mutate(Total_Aboveground_Biomass_Mg_ha = Total_Aboveground_Biomass_kg_100m2 / 10 *0.464 )%>% #1 kg/100m2 = 0.1 tonnes/ha, *0.464 conversion factor after Kauffman & Donato 2012 and Howard et al 2014)
  
  gather(key = treat, value = mass, Total_Aboveground_Biomass_Mg_ha) %>% 
  
  group_by(Site,Site_short,SiteRenamed_25y) %>%
  summarise(AV=mean(mass, na.rm = T),
            SD=sd(mass),
            N = length(mass),
            SE= SD / sqrt(N)) %>%
  mutate (Stock = "Plant")

plant_stock_summary
plant_stock_summary[is.na(plant_stock_summary)] <- 0 #Replace NaN (4 Converted sites 4 plotting purpose) with zeros and join with site label (SiteTreat) dataset:


ab <- rbind (plant_stock_summary,soil_stock_50cm_summary ) %>%
  mutate(AV= ifelse(Stock =="Plant",AV,AV * -1))%>% #Turn soil values into negative values
  mutate(project = "RR")

ab_Young <- filter(ab, SiteRenamed_25y == "Rehabilitated_young") #Use this for plotting Converted sites
ab_Young

#Draw figure breaks to fit data in:
MyBreaks <- c(-150,-100, -50, 0, 100, 200,300 ,400,500,600)

#Plot Disturbed Plant biomass:
plot_Young <- ggplot(ab_Young, aes(x=Site, y=AV, fill = Stock))+
  geom_bar(position="identity", stat="identity")+
  geom_errorbar( aes(ymin= AV+SE, ymax = AV-SE), width=.4)+
  geom_hline(yintercept=0)+
  scale_y_continuous(breaks = MyBreaks,labels = abs(MyBreaks), limits = c(-200,150))+ #abs to remove negative values on y-axis below 0
  scale_fill_manual(values = c("darkgreen","lightblue"))+
  xlab("")+ ylab("")+
  ggtitle("Rehabilitated young sites (50 cm cores)")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=18, colour = "black"),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20),
        legend.position = c(.5, .85),
        legend.text = element_text(size = 12),
        legend.title = element_text(face = "italic", size=14),
        plot.title = element_text(hjust = 0.5,lineheight=1.2, face="bold",size=22))
plot_Young
