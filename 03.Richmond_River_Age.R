#LOAD 210Pb Data:=========
#for cores where average value for MAR using the CF.CS model is produced,
# multiply this by the % C in each section to obtain the CAR in gC/m2/yr (Carbon Accretion Rate):
#MAR values (g/cm2/y) are based on file sent from Pere: "Pb210 dating Cores Mangrove Rehab Carnell 2019"

library(tidyverse)
library(gridExtra)

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

#Compute Carbon Accretion Rate at Rehabilitated sites:
CAR_Rehabiliated <-
  left_join(bb,SiteTreatMAR, by = "Site") %>%
  select(C_perc, Site, Site_Core, MAR, MAR_SE, SiteRenamed,SiteNew) %>%
  mutate(CAR_gcm2y = (C_perc/100) * MAR *100) %>%# *100 to conver to tonnes per ha
  mutate (Stock = "Belowground") %>%
  filter(SiteRenamed == "Rehabilitated") %>%
  group_by(Site,SiteNew) %>%
  summarise(Mean_CAR = weighted.mean (CAR_gcm2y, na.rm=T),
            N_CAR = length (CAR_gcm2y),
            SD_CAR =sd (CAR_gcm2y, na.rm=T),
            SE_CAR =sd (CAR_gcm2y, na.rm=T)/sqrt(N_CAR))

CAR_Rehabiliated

#Compute Mean CAR for Established site (abline):
CAR_Established <-   left_join(bb, SiteTreatMAR, by = "Site") %>%
  select(C_perc, Site, Site_Core, MAR, MAR_SE, SiteRenamed) %>%
  mutate(CAR_gcm2y = (C_perc/100) * MAR  *100) %>% # *100 to conver to tonnes per ha
  filter(SiteRenamed == "Established") %>%
  mutate (Stock = "Belowground",
          MaxCar = max (CAR_gcm2y, na.rm = T),
          MinCar = min (CAR_gcm2y, na.rm = T)) 
  

#To create a Mean =- SE riboon add this:
#%>%  summarise(Mean_Remnant_Burial = weighted.mean(CAR_gcm2y,na.rm=T),
            N_CAR = length (CAR_gcm2y),
            SD_CAR =sd (CAR_gcm2y, na.rm=T),
            SE_CAR =sd (CAR_gcm2y, na.rm=T)/sqrt(N_CAR)) 


#BURIAL PLOT:======
#Create a ribon of SE-se around MIN/MAX CAR for Established site (abline):
myMax <- mean (CAR_Established$MaxCar)
myMin <- mean (CAR_Established$MinCar)

burial_plot <- ggplot(CAR_Rehabiliated, aes(x= as.numeric(as.character(SiteNew)), y= Mean_CAR )) +
  geom_point(aes(color = SiteNew, size = 1.1)) +
  geom_errorbar( aes(ymin = Mean_CAR + SE_CAR,
                     ymax = Mean_CAR - SE_CAR), width=.2)+
  scale_x_continuous(limits = c(1977, 2005), breaks = c(1977, 1991,1992,2003))+
  labs(x= "Year planted", y = bquote('Organic carbon burial rate  ' (Mg*~ha^-1~year^-1)))+
  geom_hline(yintercept=0.3958711, linetype = 2)+
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black",angle = 90),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "none",
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))+
  annotate("rect", xmin = -Inf, xmax = Inf,
           ymin = myMin, ymax = myMax, fill = "lightgrey", alpha = .4)

burial_plot



#Plot C-stoc through time against remnant site:======
#RELOAD Richmond River DATA
#Compute total soil C-stock till 100 cm deep:========
soil_100cm <- select(NewDATA, Site, Site_short,Site_Core, Treatment,Depth_to, Depth_Range,
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

soil_100cm
#Add columns:-0
soil_carbon_100cm <- left_join(soil, SiteTreat, by = "Site" )
soil_carbon_100cm

#Compute soil stock at Rehabilitated sites:
stock_Rehabiliated <-
  select(soil_carbon_100cm, SiteNew,SiteRenamed, AV,SE, Treatment2) %>%
  filter(SiteRenamed == "Rehabilitated")

stock_Rehabiliated

#Compute Mean stock for Established site (abline):
stock_Established <- 
  select(soil_carbon, SiteNew, AV,SE, Treatment2,SiteRenamed) %>%
  filter(SiteRenamed == "Established") %>%
  summarise(Mean_Remnant_Stock = mean(AV,na.rm=T),
            N_stock = length (AV),
            SD_stock =sd (AV, na.rm=T),
            SE_stock =sd (AV, na.rm=T)/sqrt(N_stock)) 

stock_Established #186.  -+   26.5 SE

soil_stock_plot <- ggplot(stock_Rehabiliated, aes(x= as.numeric(as.character(SiteNew)), y= AV )) +
  geom_point(aes(color = SiteNew, size = 1.1)) +
  geom_errorbar( aes(ymin = AV + SE,
                     ymax = AV - SE), width=.2)+
  scale_x_continuous(limits = c(1977, 2005), breaks = c(1977, 1991,1992,2003))+
  labs(x= "Year planted", y = bquote('Organic carbon soil stock  ' (Mg*~ha^-1)))+
  geom_hline(yintercept= 186.1131, linetype = 2)+
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black",angle = 90),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "none",
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))
soil_stock_plot

#Create a ribon of SE-se around Mean CAR for Established site (abline):
#  186.1131 +- 26.5
myMax <- 186.1131 + 26.5
myMin <- 186.1131 -26.5

stock_plot_shade <- stock_plot +
  geom_ribbon(aes(ymin=myMin, ymax=myMax, alpha=0.2 ), fill = "lightgrey")+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))

stock_plot_shade


#Compute soil C-stock till Depth of Rehab (DepthTo_SinceRehabilitated_cm):========
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
View(soil_stock_till_rehab)

#Compute soil stock at Rehabilitated sites:
stock_Rehabiliated <-
  select(soil_stock_till_rehab, SiteYear,SiteRenamed, AV,SE) %>%
  filter(SiteRenamed == "Rehabilitated") %>%
  select(SiteYear,SiteRenamed,AV,SE)

stock_Rehabiliated

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

#Soil C-stock all:
stock_both <- rbind (stock_Established,stock_Rehabiliated)

#PLOT rehab with ribon of SE-se around Mean CAR for Established site (abline):====
MyMax <- as.numeric(stock_Established[1,"AV"] + stock_Established[1,"SE"])
MyMin <- as.numeric(stock_Established[1,"AV"] - stock_Established[1,"SE"])

soil_till_rehab_plot <- ggplot(stock_both, aes(x= as.numeric(as.character(SiteYear)), y= AV )) +
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

#Compute CAR (Carbon Acrretion Rate)=====
bb <- bb %>%  filter(Treatment != "Saltmarsh_natural") #Remove this habitat, not assessed
bb$Site <- factor(bb$Site) #Remove the extra factor level of SNND

cars <-  left_join(bb, MAR, by = "Site") %>%
  mutate(CAR_gcm2y = (C_perc/100) * MAR)%>%
  mutate (Stock = "Belowground")

cars2 <-   left_join(cars,SiteTreat, by = "Site")

cars30<- filter(cars2, Depth_to <= 30 ) %>%  #till 30 because that is how deep the Age-dating core was
  group_by(Site_Core) %>%
  mutate(Mean_CAR_30cm = mean(CAR_gcm2y)) %>%   #from 0 cm to 15 cm
  group_by(SiteRenamed) %>%
  summarise(AV = mean(Mean_CAR_30cm*100, na.rm = T),
            SD = sd(Mean_CAR_30cm*100, na.rm = T),
            N  = length(Mean_CAR_30cm),
            SE = SD / sqrt(N))  %>% 
  mutate (Depth = "00to30")

cars30

cars30$SiteRenamed <- factor(cars30$SiteRenamed, levels = c("Converted","Rehabilitated","Established"))
ggplot(cars30, aes(SiteRenamed, AV))  +
  geom_point(aes(shape = SiteRenamed, size = 3)) +
  geom_errorbar( aes(ymin= AV+SE, ymax = AV-SE), width=.3)+
  labs(x= "", y = bquote('Carbon Accretion Rate  ' (Mg*~ha^-1 ~y^-1)))+
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "none",
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))


#Compute CO2 equivalents:
#Multiple C by 3.67:
#WEB: https://thinkprogress.org/the-biggest-source-of-mistakes-c-vs-co2-c0b077313b/
cars30$AV_CO2_Eq <- cars30$AV * 3.67 #CO2_Eq = CO2 equivalents
cars30$SE_CO2_Eq <- cars30$SE * 3.67 #CO2_Eq = CO2 equivalents

cars30

#ggsave(filename = "RR_Age_600DPI.png", 
       width = 8, 
       height = 5,
       dpi = 600)
