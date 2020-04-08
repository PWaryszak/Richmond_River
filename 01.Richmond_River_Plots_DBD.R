#LOAD Richmond River DATA:=====
library(tidyverse)
library(gridExtra)

aa <- read.csv("RR_Plant.csv") #Ex aboveground.csv
bb <- read.csv("RR_Soil.csv") #Ex belowground.csv

#Correct Soil data for compaction:=========
#list sites and corresponding treatments
SiteTreat <- read.csv("RR_SiteTreatment.csv")#to join Treatments to 

#Compute corrected C-stock (Off Bulk Density):
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

#Compute total C-stock till 100 cm deep:========
soil <- select(NewDATA, Site, Site_short,Site_Core, Treatment,Depth_to, Depth_Range,
                     Site_Core, Carbon_stock_in_section_Mg_Cha,Treatment2,CarbonStock.Mgha,
                     Lab_Compaction_Correction_Value)%>%
  filter  (Treatment != "Saltmarsh_natural") %>%
  filter (Depth_to <= 100 ) %>% #keeping only top 100 cm of each core
  mutate (Stock = "Soil") %>% #Assigning additional category, indicatig it is soil stock
  group_by(Site_Core) %>% #grouping by core till 100 cm
  summarise(TotalCarbonStockPerCore = sum(CarbonStock.Mgha))%>% #Add-up all slices 
  separate(Site_Core, into = c("Site","CoreNumber"), sep = "_") %>%
  group_by(Site) %>% #to sum total Carbon Stock per Site
  summarise(AV = mean(TotalCarbonStockPerCore, na.rm = T),
            SD = sd(TotalCarbonStockPerCore, na.rm = T),
            N = length(TotalCarbonStockPerCore),
            SE = SD / sqrt(N)) %>%
  mutate (Stock = "Soil")

soil 
#Add column "Site_Short"
soil_carbon <- left_join(soil, SiteTreat, by = "Site" )
soil_carbon


#Compute Plant C-Stock (mean +- SE):=========
plant <- select (aa, Site,  Total_Aboveground_Biomass_kg_100m2) %>%
  mutate(Total_Aboveground_Biomass_Mg_ha = Total_Aboveground_Biomass_kg_100m2 / 10 )%>% #1 kg/100m2 = 0.1 tonnes/ha
  gather(key = treat, value = mass, Total_Aboveground_Biomass_Mg_ha) %>% 
  group_by(Site) %>%
  summarise(AV=mean(mass, na.rm = T),
            SD=sd(mass),
            N = length(mass),
            SE= SD / sqrt(N)) %>%
  mutate (Stock = "Plant")


plant[is.na(plant)] <- 0 #Replace NaN with zeros:
plant_carbon<-left_join(plant, SiteTreat, by = "Site") #join with site descriptives file
plant_carbon


#Plot above and below C-stocks:========
ab <- rbind (plant_carbon, soil_carbon)%>%
  mutate(AV= ifelse(Stock =="Plant",AV, AV*-1))

ab_Rehabilitated <- filter(ab, Treatment2 == "Rehabilitated")
ab_Disturbed <- filter(ab, Treatment2 == "Disturbed")
ab_Remnant <- filter(ab, Treatment2 == "Remnant")

#Draw a figure:
MyBreaks <- c(-300, -200,-100, -50, 0, 50, 100, 200,300 ,400,500)

#Plot Rehabilitated:
plot_Rehabilitated <- ggplot(ab_Rehabilitated, aes(x=as.factor(SiteNew), y=AV, fill = Stock))+
  geom_bar(position="identity", stat="identity")+
  geom_errorbar( aes(ymin= AV+SE, ymax = AV-SE), width=.4)+
  geom_hline(yintercept=0)+
  scale_y_continuous(breaks = MyBreaks,labels = abs(MyBreaks), limits = c(-300,600))+ #abs to remove negative values on y-axis below 0
  scale_fill_manual(values = c("darkgreen","lightblue"))+
  xlab("Site")+ylab("")+
  ggtitle("Rehabilitated")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=12),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=20),
        legend.position = "none",
        legend.text = element_text(size = 9),
        legend.title = element_text(face = "italic", size=10),
        plot.title = element_text(hjust = 0.9,lineheight=1.2, face="bold",size=20))

plot_Rehabilitated
#Plot Disturbed Plant biomass:
plot_Disturbed <- ggplot(ab_Disturbed, aes(x=Site_short, y=AV, fill = Stock))+
  geom_bar(position="identity", stat="identity")+
  geom_errorbar( aes(ymin= AV+SE, ymax = AV-SE), width=.4)+
  geom_hline(yintercept=0)+
  scale_y_continuous(breaks = MyBreaks,labels = abs(MyBreaks), limits = c(-300,600))+ #abs to remove negative values on y-axis below 0
  scale_fill_manual(values = c("darkgreen","lightblue"))+
  xlab("Site")+ ylab("")+
  ggtitle("Disturbed")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=12),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=20),
        legend.position = c(.75, .85),
        legend.text = element_text(size = 9),
        legend.title = element_text(face = "italic", size=10),
        plot.title = element_text(hjust = 0.5,lineheight=1.2, face="bold",size=20))

#Plot Remnant Plant biomass:
plot_Remnant <- ggplot(ab_Remnant, aes(x=Site_short, y=AV, fill = Stock))+
  geom_bar(position="identity", stat="identity")+
  geom_errorbar( aes(ymin= AV+SE, ymax = AV-SE), width=.4)+
  geom_hline(yintercept=0)+
  scale_y_continuous(breaks = MyBreaks,labels = abs(MyBreaks), limits = c(-300,600))+ #abs to remove negative values on y-axis below 0
  scale_fill_manual(values = c("darkgreen","lightblue"))+
  xlab("Site") + ylab(bquote("Organic carbon stock " (Mg*~ha^-1)))+
  ggtitle("Remnant")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=12),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=20),
        legend.position = "none",
        legend.text = element_text(size = 8),
        legend.title = element_text(face = "italic",size=10),
        plot.title = element_text(hjust = 0.5,lineheight=1.2, face="bold",size=20))

grid.arrange(plot_Remnant,plot_Rehabilitated,plot_Disturbed, ncol = 3)


#ABSTRACT Numbers======
aboveSum_abstract <- select (aa, Site, Total_Aboveground_Biomass_kg_100m2) %>%
  mutate(Total_Aboveground_Biomass_Mg_ha = Total_Aboveground_Biomass_kg_100m2 / 10 )%>% #1 kg/100m2 = 0.1 tonnes/ha
  gather(key = treat, value = mass, Total_Aboveground_Biomass_Mg_ha) %>% 
  group_by(Site) %>%
  summarise(AV=mean(mass, na.rm = T),
            SD=sd(mass),
            N = length(mass),
            SE= SD / sqrt(N)) %>%
  mutate (Stock = "Plant")

#Soil C-stock down to 50cm:=====
#For bottom part of Figure 2 in MS:
DownTo50cm <- select(bb,  Site_Core, Treatment,Treatment2,Depth_to, 
                     Carbon_stock_in_section_Mg_Cha,
                     Lab_Compaction_Correction_Value)%>%
  filter(Treatment != "Saltmarsh_natural") %>%
  filter(Depth_to <= 50 ) %>% #DownTo50cm
  group_by(Site_Core,Treatment2)%>%
  mutate( TotalPerCore = sum (Carbon_stock_in_section_Mg_Cha, na.rm = T),
          DepthCorrected = Depth_to / Lab_Compaction_Correction_Value, #Depth_to = recorded depth of core in the lab
          DepthChange = DepthCorrected-Depth_to,
          DepthChangePerc = 100-(DepthChange/Depth_to *100), #Percent increase in depth
          CorrectedCarbonStock = TotalPerCore * DepthChangePerc/100,#Percent decrease in Carbon Stock when cut at 50cm
          Stock = "Soil")%>% 
  group_by(Site_Core,Treatment2)%>%
  summarise(CoreTotal_Till50cm = sum (CorrectedCarbonStock, na.rm = T)) %>%

View(DownTo50cm)

#Boxplot:
#Compute total C-stock till 100 cm deep:
soil_50cm <- select(NewDATA, Site, Site_short,Site_Core, Treatment,Depth_to, Depth_Range,
               Site_Core, Carbon_stock_in_section_Mg_Cha,Treatment2,CarbonStock.Mgha,
               Lab_Compaction_Correction_Value)%>%
  filter  (Treatment != "Saltmarsh_natural") %>%
  filter (Depth_to <= 50 ) %>% #keeping only top 100 cm of each core
  mutate (Stock = "Soil") %>% #Assigning additional category, indicatig it is soil stock
  group_by(Site_Core) %>% #grouping by core till 100 cm
  summarise(TotalCarbonStockPerCore = sum(CarbonStock.Mgha))%>% #Add-up all slices 
  separate(Site_Core, into = c("Site","CoreNumber"), sep = "_") %>%
  group_by(Site) %>% #to sum total Carbon Stock per Site
  summarise(AV = mean(TotalCarbonStockPerCore, na.rm = T),
            SD = sd(TotalCarbonStockPerCore, na.rm = T),
            N = length(TotalCarbonStockPerCore),
            SE = SD / sqrt(N)) %>%
  mutate (Stock = "Soil")

soil_50cm <-  left_join(soil_50cm, SiteTreat, by = "Site" )
soil_50cm

soil_50cm$Treatment2 <- factor(soil_50cm$Treatment2)
soil_50cm$Treatment2 <- factor(soil_50cm$Treatment2, levels = c("Remnant","Rehabilitated" ,"Disturbed" ))


ggplot(soil_50cm, aes(x=Treatment2, y= AV))+
  labs(fill = "Treatments")+
  labs(x = "",y =bquote("Organic carbon stock " (Mg*~ha^-1 ~y^-1)))+
  geom_boxplot() +
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=15, color = "black"),
        axis.text.y=element_text(size=15, color = "black"),
        axis.title.y=element_text(size=20, color = "black"))

