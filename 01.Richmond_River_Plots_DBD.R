#LOAD Richmond River DATA:=====
library(tidyverse)
library(gridExtra)
library(ggpmisc)

aa <- read.csv("RR_Plant.csv") #Ex aboveground.csv
bb <- read.csv("RR_Soil.csv") #Ex belowground.csv

#Correct Soil data for compaction:=========
#list sites and corresponding treatments
SiteTreat <- read.csv("RR_SiteTreatment_PC.csv")#to join Treatments to 

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
  mutate(AV= ifelse(Stock =="Plant",AV, AV*-1)) #Turning soil stock into negative values for plotting.

ab_Rehabilitated <- filter(ab, Treatment2 == "Rehabilitated")
ab_Disturbed <- filter(ab, Treatment2 == "Disturbed")
ab_Remnant <- filter(ab, Treatment2 == "Remnant")

#Draw a figure:
MyBreaks <- c(-300, -200,-100, -50, 0, 50, 100, 200,300 ,400,500)
#Plot Converted Plant/Soil stock:========
plot_Converted <- ggplot(ab_Disturbed, aes(x=Site_short, y=AV, fill = Stock))+
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


#Plot Rehabilitated Plant/Soil stock:=======
ab_Rehabilitated$SiteNew <- factor(ab_Rehabilitated$SiteNew, levels = c(2003, 2001,1992,1991,1981,1977))
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

#Plot Established Plant/Soil stock:=======
ab_Remnant$Site_short <- factor(ab_Remnant$Site_short, levels = c("ND","PL","DC"))
levels(ab_Remnant$Site_short)

plot_Established <- ggplot(ab_Remnant, aes(x=Site_short, y=AV, fill = Stock))+
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

grid.arrange(plot_Converted, plot_Rehabilitated, plot_Established,
             ncol = 3)



#Boxplot Soil C-stock down to 50cm:=====
##Boxplot:For bottom part of Figure 2 in MS:
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
soil_50cm$SiteRenamed <- factor(soil_50cm$SiteRenamed, levels = c("Established","Rehabilitated" ,"Converted" ))

ggplot(soil_50cm, aes(x="", y= TotalCarbonStockPerCore,fill = SiteRenamed))+
  labs(x = "",y =bquote("Organic carbon stock " (Mg*~ha^-1)))+
  geom_boxplot(outlier.shape = NULL) + geom_jitter()+
  scale_fill_manual(values = c("lightblue","lightblue","lightblue"))+
  facet_grid(.~SiteRenamed)+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=15, color = "black"),
        axis.text.y=element_text(size=15, color = "black"),
        axis.title.y=element_text(size=20, color = "black"),
        strip.text = element_text(size = 18),
        legend.position = "none")

#NEW BOXPLOT
MyBreaks2 <- c(-300, -200, -100,  -50  ,  0 )
ggplot(soil_50cm, aes(x="", y= TotalCarbonStockPerCore*-1,fill = SiteRenamed))+
  labs(x = "",y =bquote("Organic carbon stock " (Mg*~ha^-1)))+
  geom_boxplot(outlier.shape = NULL) + geom_jitter()+
  geom_hline(yintercept=0)+
  scale_y_continuous(breaks = MyBreaks2,labels = abs(MyBreaks2), limits = c(-300,0))+ #abs to remove negative values on y-axis below 0
    scale_fill_manual(values = c("lightblue","lightblue","lightblue"))+
  facet_grid(.~SiteRenamed)+
  theme_bw()+
  theme(axis.text.x=element_text(vjust=0.5,size=15, color = "black"),
        axis.text.y=element_text(size=15, color = "black"),
        axis.title.y=element_text(size=20, color = "black"),
        strip.text = element_text(size = 18),
        strip.background = element_rect("white"),
        legend.position = "none")


#PLOT Aboveground_Biomass against time planted:=====
#Aboveground biomass carbon (Mg ha-1) was then calculated
#from the summed tree biomass estimates for each plot via
#application of a standard conversion factor of 0.464 (Kauffman & Donato 2012; Howard et al. 2014).

Aboveground_Biomass <- select (aa, Site, Treatment2, Total_Aboveground_Biomass_kg_100m2) %>%
   filter(Treatment2 == "Rehabilitated") %>%
  mutate(Total_Aboveground_Biomass_Mg_ha = Total_Aboveground_Biomass_kg_100m2 / 10)%>%  #1 kg/100m2 = 0.1 tonnes/ha
  gather(key = treat, value = mass, Total_Aboveground_Biomass_Mg_ha) %>% 
  group_by(Site) %>%
  summarise(AV=mean(mass, na.rm = T),
            SD=sd(mass),
            N = length(mass),
            SE= SD / sqrt(N)) %>%
  left_join(SiteTreat,by = "Site") %>%
  mutate(Time_Since_Planting = 2017 - as.numeric(as.character(SiteYear))) %>%
  mutate (Stock = "Plant") 

Aboveground_Biomass

#Plotting plant biomass regression that is of negative slope:
plant_stock_plot <- ggplot(Aboveground_Biomass, aes(x= Time_Since_Planting , y= AV))  +
  geom_point(aes(color = Site,shape=SiteRenamed),size=4) +
  scale_shape_manual(values = 17)+
  geom_errorbar( aes(ymin = AV + SE,
                     ymax = AV - SE), width=.2)+
  geom_smooth(method = "lm", formula = y~x)+
  labs(x= "Time since planting (years)", shape = "Site Type: ",
       y = bquote('Plant biomass  ' (Mg*~ha^-1)))+
  scale_x_continuous(limits = c(14,40))+
  theme_bw() +
  stat_fit_glance(method = "lm",
                  label.x = c(0.9,0),
                  method.args = list(formula = y ~ x),
                  mapping = aes(label = sprintf('R^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                stat(r.squared), stat(p.value))),
                  parse = TRUE)+
  stat_poly_eq(formula =   y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)+
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "bottom",
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))

plant_stock_plot


#Propose new plot for C_Plant without including a regression:
Aboveground_Biomass2 <- select (aa, Site, Treatment2, Total_Aboveground_Biomass_kg_100m2) %>%
  # filter(Treatment2 == "Rehabilitated") %>%
  mutate(Total_Aboveground_Biomass_Mg_ha = Total_Aboveground_Biomass_kg_100m2 / 10,  #1 kg/100m2 = 0.1 tonnes/ha
         Plant_C = 0.464* Total_Aboveground_Biomass_Mg_ha ) %>%  #1 kg/100m2 = 0.1 tonnes/ha
  gather(key = treat, value = mass, Plant_C) %>% 
  left_join(SiteTreat,by = "Site") %>%
  group_by(Site, SiteRenamed) %>%
  summarise(AV=mean(mass, na.rm = T),
            SD=sd(mass),
            N = length(mass),
            SE= SD / sqrt(N)) %>%
  mutate (Stock = "Plant") %>%
  filter(SiteRenamed != "Converted") 

Aboveground_Biomass2
Sites <- select(SiteTreat, Site, SiteYear)
AB3 <- left_join(Aboveground_Biomass2, Sites, by = "Site")

MyMin3 <- as.numeric(Aboveground_Biomass2[2,"AV"])
MyMax3 <- as.numeric(Aboveground_Biomass2[1,"AV"])

plant_stock_plot2 <- ggplot(AB3,aes(x= 2017-as.numeric(as.character(SiteYear)), y= AV )) +
  geom_point(aes(shape = SiteRenamed),size = 3) +
  geom_errorbar( aes(ymin = AV + SE,
                     ymax = AV - SE), width=.2)+
  scale_x_continuous(limits = c(0, 100))+
  labs(x= "Year planted", shape = "Site Type: ",
       y = bquote('Plant C-stock since rehabiliatation  ' (Mg*~ha^-1)))+
  geom_hline(yintercept= Remnant_intercept , linetype = 2)+
  theme_bw() +
  theme(axis.text.x = element_text(size = 14, color = "black",angle = 90),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.position = "top",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text=element_text(size=14),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))+
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = MyMin3, ymax = MyMax3, fill = "lightgrey", alpha = .4)


plant_stock_plot2








#Soil C-stock till Depth of Rehab (DepthTo_SinceRehabilitated_cm):========
NewDATA2 <- left_join (NewDATA, SiteTreat, by = "Site") %>%
    mutate(KeepThrow = ifelse(DepthTo_SinceRehabilitated_cm >= Depth_from, "keep", "throw")) %>% #keep slices data are > Depth_from
    filter(KeepThrow=="keep") %>% #keep the "keep"
    transform (DepthAtRehab_cm = ifelse(Depth_to <= DepthTo_SinceRehabilitated_cm,  #Cut to the length of DepthTo_SinceRehabilitated_cm
                                     Depth_to, DepthTo_SinceRehabilitated_cm)) %>%
  mutate (SliceAtRehab_cm = DepthAtRehab_cm - Depth_from) %>% #lenght of slice at cores up to DepthTo_SinceRehabilitated_cm
  mutate (CarbonStockTillRehab_Mgha = CarbonDensity.gcm3  * 100 * SliceAtRehab_cm) #Soilc C stock in cores till DepthTo_SinceRehabilitated_cm




soil_till_rehab <- select(NewDATA2, Site,SiteYear, Site_short,Site_Core, Treatment,Depth_to, Depth_Range,
                          Site_Core, Carbon_stock_in_section_Mg_Cha,Treatment2,CarbonStock.Mgha,
                          CarbonStockTillRehab_Mgha)%>%
  filter  (Treatment != "Saltmarsh_natural") %>% #not looking at saltmarhs in this study, mangroves only
  mutate (Stock = "Soil") %>% #Assigning additional category, indicatig it is soil stock
  group_by(Site_Core) %>% #grouping by core till 100 cm
  summarise(TotalCarbonStockPerCore = sum(CarbonStockTillRehab_Mgha))%>% #Add-up all slices 
  separate(Site_Core, into = c("Site","CoreNumber"), sep = "_") %>%
  group_by(Site) %>% #to sum total Carbon Stock per Site
  summarise(AV = mean(TotalCarbonStockPerCore, na.rm = T),
            SD = sd(TotalCarbonStockPerCore, na.rm = T),
            N = length(TotalCarbonStockPerCore),
            SE = SD / sqrt(N)) %>%
  mutate (Stock = "Soil")

soil_till_rehab 
#Add columns from SiteTreat again:
soil_stock_till_rehab <- left_join(soil_till_rehab , SiteTreat, by = "Site" )
soil_stock_till_rehab


#Compute Mean stock for Established site (abline):
stock_Established <- select(NewDATA2, Site,SiteYear, Site_short,Site_Core, SiteRenamed,Depth_to, Depth_Range,
                            Site_Core, Treatment2,CarbonStockTillRehab_Mgh,
                            stock_till_rehab)%>%
  filter  (SiteRenamed == "Established") %>% #Established only
  mutate (Stock = "Soil") %>% #Assigning additional category, indicatig it is soil stock
  group_by(Site_Core) %>% #grouping by core till 100 cm
  summarise(TotalCarbonStockPerCore = sum(CarbonStockTillRehab_Mgh))%>% #Add-up all slices 
  summarise(AV = mean(TotalCarbonStockPerCore, na.rm = T),
            SD = sd(TotalCarbonStockPerCore, na.rm = T),
            N = length(TotalCarbonStockPerCore),
            SE = SD / sqrt(N)) %>%
  select(AV,SE) %>%
  mutate(SiteYear = "1970",
         SiteRenamed = "Established")

stock_Established #122.+-  34.6
Remnant_intercept = as.numeric(stock_Established[1,"AV"])


#PLOT rehab with ribon of SE-se around Mean CAR for Established site (abline):====
MyMax <- as.numeric(stock_Established[1,"AV"] + stock_Established[1,"SE"])
MyMin <- as.numeric(stock_Established[1,"AV"] - stock_Established[1,"SE"])
MyMin2 <- as.numeric(soil_stock_till_rehab[1,"AV"])
MyMax2 <- as.numeric(soil_stock_till_rehab[2,"AV"])

soil_till_rehab_plot <- ggplot(soil_stock_till_rehab,
                               aes(x= 2017-as.numeric(as.character(SiteYear)), y= AV )) +
  geom_point(aes(shape = SiteRenamed),size = 3) +
  geom_errorbar( aes(ymin = AV + SE,
                     ymax = AV - SE), width=.2)+
  scale_x_continuous(limits = c(0, 100))+
  labs(x= "Year planted", shape = "Site Type: ",
       y = bquote('Soil C-stock since rehabiliatation  ' (Mg*~ha^-1)))+
  geom_hline(yintercept= Remnant_intercept , linetype = 2)+
  theme_bw() +
  theme(axis.text.x = element_text(size = 14, color = "black",angle = 90),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.position = "top",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text=element_text(size=14),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))+
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = MyMin2, ymax = MyMax2, fill = "lightgrey", alpha = .4)

soil_till_rehab_plot

#PLOT rehab with MIN/MAX of Established site (abline):====
stock_Established2 <- select(soil_stock_till_rehab, SiteRenamed, AV) %>%
  filter  (SiteRenamed == "Established") #Established only

MyMax <- as.numeric(  max  (stock_Established2$AV))
MyMin <- as.numeric(  min (stock_Established2$AV))

soil_till_rehab_plot2 <- ggplot(stock_Rehabiliated, aes(x= as.numeric(as.character(SiteYear)), y= AV )) +
  geom_point(aes(size = 1.1, shape = SiteRenamed)) +
  geom_errorbar( aes(ymin = AV + SE,
                     ymax = AV - SE), width=.2)+
  scale_x_continuous(limits = c(1970, 1997), breaks = c(1980, 1991,1992,1997))+
  labs(x= "Year planted", y = bquote('Soil C-stock since rehabiliatation  ' (Mg*~ha^-1)))+
  geom_hline(yintercept= Remnant_intercept , linetype = 2)+
  theme_bw() +
  theme(axis.text.x = element_text(size = 14, color = "black",angle = 90),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.position = "none",
        legend.text = element_text(size = 14),
        strip.text=element_text(size=14),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))+
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = MyMin, ymax = MyMax, fill = "lightgrey", alpha = .4)
soil_till_rehab_plot2


