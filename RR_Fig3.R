#LOAD Data:=========
#To compute CAR gC/m2/yr (Carbon Accretion Rate):
#chose cores where average value for MAR using the CF.CS model is produced,
#multiply this by the % C in each section to obtain the CAR
#MAR values (g/cm2/y) are based on file sent from Pere: "Pb210 dating Cores Mangrove Rehab Carnell 2019"

library(tidyverse)
library(gridExtra)

aa <- read.csv("RR_Plant.csv") #Ex aboveground.csv
bb <- read.csv("RR_Soil.csv") #All Data
SiteTreat <- read.csv("RR_SiteTreatment_PC.csv")# Treatments to join to All Data
bb <- bb %>%  filter(Treatment != "Saltmarsh_natural")#Remove this habitat, not assessed
bb$Site <- factor(bb$Site) #Remove the extra factor level of SNND

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

#Join Sites with MAR data:
SiteTreatMAR <- left_join(SiteTreat,MAR, by = "Site")
SiteTreatMAR#list sites and corresponding treatments

#Correct Soil data for compaction and #Compute corrected C-stock (Off Bulk Density):
NewDATA <- bb
NewDATA$C_percent <- ifelse(NewDATA$C_perc == 0, 0.001, NewDATA$C_perc)#convert 0 into 0.001 to run log-models if any
NewDATA$SliceLength.cm <- (NewDATA$Depth_to - NewDATA$Depth_from) #cm
NewDATA$PipeDiameter.cm <- 5 #Diameter of coring pipes was 5 cm
NewDATA$SampleVolume.cm3 <- (pi*(NewDATA$PipeDiameter.cm/2)^2)*NewDATA$SliceLength.cm  #slice volume
NewDATA$dry_bulk_density.gcm3_corrected <- NewDATA$Dry_bulk_density_gcm3 * NewDATA$Lab_Compaction_Correction_Value
NewDATA$CarbonDensity.gcm3 <- NewDATA$dry_bulk_density.gcm3_corrected * NewDATA$C_percent/100
NewDATA$CarbonStock.Mgha <- ((NewDATA$CarbonDensity.gcm3  * 100) * NewDATA$SliceLength.cm )

#Compute Carbon Accretion Rate at Rehabilitated and Established sites:
CAR_Rehabiliated <-
  left_join(bb,SiteTreatMAR, by = "Site") %>%
  select(C_perc, Site, Site_Core, MAR, MAR_SE, SiteRenamed,SiteYearNumeric) %>%
  mutate(CAR_gcm2y = (C_perc/100) * MAR *100) %>%# *100 to conver to tonnes per ha
  mutate (Stock = "Belowground") %>%
  filter(SiteRenamed != "Converted") %>%
  group_by(Site,SiteYearNumeric,SiteRenamed,) %>%
  summarise(Mean_CAR = weighted.mean (CAR_gcm2y, na.rm=T),
            N_CAR = length (CAR_gcm2y),
            SD_CAR =sd (CAR_gcm2y, na.rm=T),
            SE_CAR =sd (CAR_gcm2y, na.rm=T)/sqrt(N_CAR)) %>%
  mutate(MyColor = ifelse(Site=="MRCH1", "lightgrey","black" )) #change MRCH1 Aegiceras-dominated site to grey

#PLOT CAR:======
burial_plot <- ggplot(CAR_Rehabiliated, aes(x= 2017-as.numeric(as.character(SiteYearNumeric)),y= Mean_CAR, color = MyColor)) +
  geom_point(aes(shape = SiteRenamed, color=MyColor),size = 3) +
  scale_colour_manual(values = CAR_Rehabiliated[["MyColor"]])+ #change MRCH1 Aegiceras-dominated site to grey
  geom_errorbar( aes(ymin = Mean_CAR + SE_CAR,
                     ymax = Mean_CAR - SE_CAR), width=.2)+
  scale_x_continuous(limits = c(0, 100))+
  labs(x= "Stand age (years)", y = bquote('Soil C burial rate  ' (Mg*~ha^-1~year^-1)))+
  geom_hline(yintercept=0.3958711, linetype = 2)+
  theme_bw() +
  theme(axis.text.x = element_text(size = 14, color = "black",angle = 90),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.position = "none")

#Find MIN/MAX CAR for Established site grey shade if needed:
Remnant_CarMax <- as.numeric(CAR_Rehabiliated[3,"Mean_CAR"])
Remnant_CarMin <- as.numeric(CAR_Rehabiliated[2,"Mean_CAR"])
#+ Add grey area with annotate below if needed again:
# annotate("rect", xmin = -Inf, xmax = Inf, ymin = Remnant_CarMax , ymax = Remnant_CarMin , fill = "lightgrey", alpha = .4)

burial_plot


#PLOT Plant ======
SitesToMerge <- select(SiteTreat, Site, SiteYearNumeric)#We need descriptors for sites from SiteTreat file.

Plant_Carbon<- select (aa, Site, Treatment2, Total_Aboveground_Biomass_kg_100m2) %>%
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
  filter(SiteRenamed != "Converted") %>%
  left_join(SitesToMerge, by = "Site") %>%
  mutate(MyColor = ifelse(Site== "MRCH1", "lightgrey","black" ))#change two Aegiceras-dominated site to grey


#Compute mean of plant stock in remnant to draw a dotted line:
Established_AV <- filter(Plant_Carbon, SiteRenamed == "Established") %>%
  select(AV,SiteRenamed) %>% 
  group_by(SiteRenamed) %>%
  summarise(remnant_plant = mean(AV, na.rm=T))

Remnant_plant_intercept <-  as.numeric(Established_AV[1,"remnant_plant"]) #Dotted line should be numeric value:

#PLOT:
Plant_Carbon_plot <- ggplot(Plant_Carbon,aes(x= 2017-as.numeric(as.character(SiteYearNumeric)), y= AV )) +
  geom_point(aes(shape = SiteRenamed, color = Site),size = 3) +
  scale_colour_manual(values = Plant_Carbon[["MyColor"]])+ #change Aegiceras-dominated site to grey
  geom_errorbar( aes(ymin = AV + SE,
                     ymax = AV - SE), width=.2)+
  scale_x_continuous(limits = c(0, 100))+
  scale_y_continuous(limits = c(0, 250))+
  labs(x= "Stand age (years)", shape = "Site Type: ",
       y = bquote('Plant C-stock since rehabiliatation'~~(Mg*~ha^-1)))+
  geom_hline(yintercept= Remnant_plant_intercept , linetype = 2)+
  guides(color = FALSE) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 14, color = "black",angle = 90),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.position = c(0.25,0.85),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.key = element_rect( fill = "white", color = "black"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        strip.text=element_text(size=14),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))

#Optional: Find min and max carbon value for Established sites:
MyMin3 <- as.numeric(Plant_Carbon[2,"AV"])
MyMax3 <- as.numeric(Plant_Carbon[1,"AV"])
#+ Add grey error area with annotate below if needed again:
#annotate("rect", xmin = -Inf, xmax = Inf, ymin = MyMin3, ymax = MyMax3, fill = "lightgrey", alpha = .4)


#PLOT Soil:========
#C-stock (Stand Age) till Depth of Rehab (DepthTo_SinceRehabilitated_cm)
NewDATA2 <- left_join (NewDATA, SiteTreat, by = "Site") %>%
  mutate(KeepThrow = ifelse(DepthTo_SinceRehabilitated_cm >= Depth_from, "keep", "throw")) %>% #keep slices data are > Depth_from
  filter(KeepThrow=="keep") %>% #keep the "keep"
  transform (DepthAtRehab_cm = ifelse(Depth_to <= DepthTo_SinceRehabilitated_cm,  #Cut to the length of DepthTo_SinceRehabilitated_cm
                                      Depth_to, DepthTo_SinceRehabilitated_cm)) %>%
  mutate (SliceAtRehab_cm = DepthAtRehab_cm - Depth_from) %>% #lenght of slice at cores up to DepthTo_SinceRehabilitated_cm
  mutate (CarbonStockTillRehab_Mgha = CarbonDensity.gcm3  * 100 * SliceAtRehab_cm) #Soilc C stock in cores till DepthTo_SinceRehabilitated_cm




soil_stock_till_rehab <- select(NewDATA2, Site,SiteYearNumeric, Site_short,Site_Core, Treatment,Depth_to, Depth_Range,
                          Site_Core, Carbon_stock_in_section_Mg_Cha,Treatment2,CarbonStock.Mgha,
                          CarbonStockTillRehab_Mgha)%>%
  filter  (Treatment != "Saltmarsh_natural") %>% #not looking at saltmarhs in this study, mangroves only
  mutate (Stock = "Soil") %>% #Assigning additional category, indicatig it is soil stock
  group_by(Site_Core) %>% #grouping by core till 100 cm
  summarise(TotalCarbonStockPerCore = sum(CarbonStockTillRehab_Mgha))%>% #Add-up all slices 
  separate(Site_Core, into = c("Site","CoreNumber"), sep = "_") %>%
  group_by(Site) %>% #to sum total Carbon Stock per Site
  summarise(AV = round(mean(TotalCarbonStockPerCore, na.rm = T),1),
            SD = round(sd(TotalCarbonStockPerCore, na.rm = T),1),
            N = length(TotalCarbonStockPerCore),
            SE = round(SD / sqrt(N),1)) %>%
  mutate (Stock = "Soil_Till_Rehab")%>%
  left_join(SiteTreat, by = "Site" ) %>% #Add columns from SiteTreat 
  mutate(MyColor = ifelse(Site=="MRCH1", "lightgrey","black"))#change Ageiceras-dominated site of MRCH1 to grey if present.


#flextable(soil_till_rehab) #For Abstract/Results section
#write.csv(soil_till_rehab, file = "soil_till_rehab.csv", row.names = F)

#Compute Mean stock for Established site (abline):
stock_Established <- select(NewDATA2, Site, SiteYearNumeric, Site_short,Site_Core, SiteRenamed,Depth_to, Depth_Range,
                            Site_Core, Treatment2,CarbonStockTillRehab_Mgha)%>%
  filter  (SiteRenamed == "Established") %>% #Established only
  group_by(Site_Core) %>% #grouping by core till 100 cm
  summarise(TotalCarbonStockPerCore = sum(CarbonStockTillRehab_Mgha))%>% #Add-up all slices 
  summarise(AV = mean(TotalCarbonStockPerCore, na.rm = T),
            SD = sd(TotalCarbonStockPerCore, na.rm = T),
            N = length(TotalCarbonStockPerCore),
            SE = SD / sqrt(N)) %>%
  select(AV,SE) %>%
  mutate(SiteYearNumeric = "1970",
         SiteRenamed = "Established")

stock_Established #63.1 +- 14.6     Established
Remnant_intercept = as.numeric(stock_Established[1,"AV"])

MyMin2 <- as.numeric(soil_stock_till_rehab[1,"AV"])
MyMax2 <- as.numeric(soil_stock_till_rehab[2,"AV"])

#PLOT:
soil_till_rehab_plot <- ggplot(soil_stock_till_rehab,
                               aes(x= 2017-as.numeric(as.character(SiteYearNumeric)), y= AV )) +
  geom_point(aes(shape = SiteRenamed, color = Site),size = 3) +
  scale_colour_manual(values = soil_stock_till_rehab[["MyColor"]])+ #change two Aegiceras-dominated site to grey
  geom_errorbar( aes(ymin = AV + SE,
                     ymax = AV - SE), width=.2)+
  scale_x_continuous(limits = c(0, 100))+
  scale_y_continuous(limits = c(0, 150))+
  labs(x= "Stand age (years)", shape = "Site Type: ",
       y = bquote('Soil C-stock since rehabiliatation  ' (Mg*~ha^-1)))+
  geom_hline(yintercept= Remnant_intercept , linetype = 2)+
  theme_bw() +
  theme(axis.text.x = element_text(size = 14, color = "black",angle = 90),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        legend.position = "none",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text=element_text(size=14),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))

#Plot Combined Plant/Soil Stock and CAR:=======
grid.arrange(Plant_Carbon_plot,soil_till_rehab_plot,burial_plot,
             ncol = 3)

#To save in higher resolution use arrangeGrob:
plots <- arrangeGrob(Plant_Carbon_plot,soil_till_rehab_plot,burial_plot, nrow=1)
#ggsave(plots, filename = "Fig3_600DPI.png",  width = 15, height = 6, dpi = 600)

