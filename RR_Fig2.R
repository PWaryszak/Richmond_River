#LOAD Richmond River DATA:=====
library(tidyverse)
library(gridExtra)
library(ggpmisc)

aa <- read.csv("RR_Plant.csv") #Plant Biomass
bb <- read.csv("RR_Soil.csv") #Soil Carbon stock
SiteTreat <- read.csv("RR_SiteTreatment_PC.csv")#lists sites with corresponding treatments

#Create NewDATA corrected for soil compaction:
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

soil_carbon <- left_join(soil, SiteTreat, by = "Site" )#Add corresponding Site and treatments:
soil_carbon

#Compute Plant C-Stock (mean +- SE):=========
plant <- select (aa, Site,  Total_Aboveground_Biomass_kg_100m2) %>%
  mutate(Plant_C_Mgha = 0.464*Total_Aboveground_Biomass_kg_100m2 / 10 )%>% #1 kg/100m2 = 0.1 tonnes/ha
  gather(key = treat, value = mass, Plant_C_Mgha) %>% 
  group_by(Site) %>%
  summarise(AV=mean(mass, na.rm = T),
            SD=sd(mass),
            N = length(mass),
            SE= SD / sqrt(N)) %>%
  mutate (Stock = "Plant")


plant[is.na(plant)] <- 0 #Replace NaN with zeros:
plant_carbon<-left_join(plant, SiteTreat, by = "Site") #join with site descriptives file
plant_carbon

#Merge plant and soil data together:
ab <- rbind (plant_carbon, soil_carbon)%>%
  mutate(AV= ifelse(Stock =="Plant",AV, AV*-1)) #Turning soil stock into negative values for plotting.

#Split ab data by Treatment:
ab_Rehabilitated <- filter(ab, SiteOldName == "Rehabilitated")
ab_Disturbed <- filter(ab, SiteOldName == "Disturbed") #aka Converted
ab_Remnant <- filter(ab, SiteOldName == "Remnant") #aka Established

#Draw a figure:
MyBreaks <- c(-300, -200,-100, -50, 0, 50, 100, 200,300 ,400,500)
#Plot Converted Plant/Soil stock:========
plot_Converted <- ggplot(ab_Disturbed, aes(x=as.factor(SiteYear_old), y=AV, fill = Stock))+
  geom_bar(position="identity", stat="identity")+
  geom_errorbar( aes(ymin= AV+SE, ymax = AV-SE), width=.4)+
  geom_hline(yintercept=0)+
  scale_y_continuous(breaks = MyBreaks,labels = abs(MyBreaks), limits = c(-300,600))+ #abs to remove negative values on y-axis below 0
  scale_fill_manual(values = c("darkgreen","lightblue"))+
  xlab("Site")+ ylab(bquote("Organic carbon stock " (Mg*~ha^-1)))+
  ggtitle("Converted")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=12),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=20),
        legend.position = c(.2, .85),
        legend.text = element_text(size = 9),
        legend.title = element_text(face = "italic", size=10),
        plot.title = element_text(hjust = 0.5,lineheight=1.2, face="bold",size=20))

plot_Converted 

#Plot Rehabilitated Plant/Soil stock:=======
ab_Rehabilitated$SiteYear_old<- factor(ab_Rehabilitated$SiteYear_old, levels = c(2003, 2001,1992,1991,1981,1977))
plot_Rehabilitated <- ggplot(ab_Rehabilitated, aes(x=as.factor(SiteYear_old), y=AV, fill = Stock))+
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

#Plot Established Plant/Soil stock:=======
ab_Remnant$Site_short_old <- factor(ab_Remnant$Site_short_old, levels = c("ND","PL","DC"))

plot_Established <- ggplot(ab_Remnant, aes(x=Site_short_old, y=AV, fill = Stock))+
  geom_bar(position="identity", stat="identity")+
  geom_errorbar( aes(ymin= AV+SE, ymax = AV-SE), width=.4)+
  geom_hline(yintercept=0)+
  scale_y_continuous(breaks = MyBreaks,labels = abs(MyBreaks), limits = c(-300,600))+ #abs to remove negative values on y-axis below 0
  scale_fill_manual(values = c("darkgreen","lightblue"))+
  xlab("Site") + ylab("")+
  ggtitle("Established")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=12),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=20),
        legend.position = "none",
        legend.text = element_text(size = 8),
        legend.title = element_text(face = "italic",size=10),
        plot.title = element_text(hjust = 0.5,lineheight=1.2, face="bold",size=20))

plot_Established

grid.arrange(plot_Converted, plot_Rehabilitated, plot_Established,
             ncol = 3)


#Plot BOXPLOT Soil C-stock down to 50cm (partB)=======
soil_50cm <- select(NewDATA, Site, Site_short,Site_Core, Treatment,Depth_to, Depth_Range,
                    Site_Core,Treatment2,CarbonStock.Mgha,Lab_Compaction_Correction_Value)%>%
  filter  (Treatment != "Saltmarsh_natural") %>%
  filter (Depth_to <= 50 ) %>% #keeping only top 100 cm of each core
  mutate (Stock = "Soil")%>%
  mutate (Stock = "Soil") %>% #Assigning additional category, indicatig it is soil stock
  group_by(Site_Core) %>% #grouping by core till 100 cm
  summarise(TotalCarbonStockPerCore = sum(CarbonStock.Mgha))%>% #Add-up all slices 
  separate(Site_Core, into = c("Site","CoreNumber"), sep = "_")# 


soil_50cm <-  left_join(soil_50cm, SiteTreat, by = "Site" )
soil_50cm$SiteRenamed <- factor(soil_50cm$SiteRenamed, levels = c("Converted" ,"Rehabilitated" , "Established"))
MyBreaks2 <- c(-300, -200, -100,  -50  ,  0 )

ggplot(soil_50cm, aes(x="", y= TotalCarbonStockPerCore,fill = SiteRenamed))+
  labs(x = "",y =bquote("Soil carbon stock " (Mg*~ha^-1)))+
  geom_boxplot(outlier.shape = NULL) + geom_jitter()+
  scale_y_continuous( limits = c(0,300))+ #abs to remove negative values on y-axis below 0
  scale_fill_manual(values = c("lightblue","lightblue","lightblue"))+
  facet_grid(.~SiteRenamed)+
  theme_bw()+
  theme(axis.text.x=element_text(vjust=0.5,size=15, color = "black"),
        axis.text.y=element_text(size=15, color = "black"),
        axis.title.y=element_text(size=20, color = "black"),
        strip.text = element_text(size = 18),
        strip.background = element_rect("white"),
        legend.position = "none")
