#Join 210Pb Data:
#for cores where average value for MAR using the CF.CS model is produced,
# multiply this by the % C in each section to obtain the CAR in gC/m2/yr (Carbon Accretion Rate):
#MAR values are based on file sent from Pere: "Pb210 dating Cores Mangrove Rehab Carnell 2019"

library(tidyverse)
library(gridExtra)
library(broom)

bb <- read.csv("RR_belowground.csv") #All Data
SiteTreat <- read.csv("RR_SiteTreatment.csv")# Treatments to join to All Data

#Create dataset off Pere's file above. MAR's units = g/cm2/y
MAR <- data.frame( Site = c("MNND","SNND","MNDC","MDBP","MDSD","MR1991","MR1992","MRAP",
                            "MRCH1","MRCH2","MRPL","MDTUCK"),
                   MAR = c(0.095, 0.096, NA, NA, NA, 0.0324,0.076,0.32,NA,0.16,NA,NA))

SiteTreatMAR <- left_join(SiteTreat,MAR, by = "Site")
MAR_short <- select(MAR, Site, MAR)
#COmpute CAR (Carbon Acrretion Rate)=====

bb <- bb %>%  filter(Treatment != "Saltmarsh_natural") #Remove this habitat, not assessed
bb$Site <- factor(bb$Site) #Remove the extra factor level of SNND

cars <-  left_join(bb, MAR_short, by = "Site") %>%
  mutate(CAR_gcm2y = C_perc * MAR)%>%
  mutate (Stock = "Belowground")

cars30<- filter(cars, Depth_to <= 30 ) %>%  #till 30 because that is how deep the Age-dating core was
  group_by(Site_Core) %>%
  mutate(Total_CAR_30cm = sum(CAR_gcm2y[Depth_to <=15])) %>%   #from 0 cm to 15 cm
  group_by(Treatment2) %>%
  summarise(AV = mean(Total_CAR_15cm, na.rm = T),
            SD = sd(Total_CAR_15cm, na.rm = T),
            N  = length(Total_CAR_15cm),
            SE = SD / sqrt(N))  %>% 
  mutate (Depth = "00to15")

cars30<- filter(cars, Depth_to <= 30 ) %>%  #till 30 because that is how deep the Age-dating core was
  group_by(Site_Core)%>%
  mutate(Total_CAR_30cm = mean(CAR_gcm2y[Depth_to >=30])) %>% #From 15 cm to 30 cm because CAR was estimated per 1cm slice
  group_by(Treatment2) %>%
  summarise( AV = mean(Total_CAR_30cm, na.rm = T),
             SD= sd(Total_CAR_30cm, na.rm = T),
             N  = length(Total_CAR_30cm),
             SE = SD / sqrt(N)) %>%
  mutate(Depth = "00to30")

ggplot(cars30, aes(Treatment2, AV))  +
  geom_point(aes(color = Treatment2, size = 3)) +
  geom_errorbar( aes(ymin= AV+SD, ymax = AV-SD), width=.3)+
  labs(x= "", y = bquote('Carbon Accretion Rate ' (g*~cm^-2 ~y^-1)))+
 ggtitle("Richmond River")+
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "none",
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))








#Plots split between two depths 00to15 and 15t030:==========
cars15<- filter(cars, Depth_to <= 15 ) %>%  #till 30 because that is how deep the Age-dating core was
  group_by(Site_Core) %>%
  mutate(Total_CAR_30cm = sum(CAR_gcm2y[Depth_to <=15])) %>%   #from 0 cm to 15 cm
  group_by(Treatment2) %>%
  summarise(AV = mean(Total_CAR_15cm, na.rm = T),
            SD = sd(Total_CAR_15cm, na.rm = T),
            N  = length(Total_CAR_15cm),
            SE = SD / sqrt(N))  %>% 
  mutate (Depth = "00to15")

cars30<- filter(cars, Depth_to <= 30 ) %>%  #till 30 because that is how deep the Age-dating core was
  group_by(Site_Core)%>%
  mutate(Total_CAR_30cm = sum(CAR_gcm2y[Depth_to >=15])) %>% #From 15 cm to 30 cm because CAR was estimated per 1cm slice
  group_by(Treatment2) %>%
  summarise( AV = mean(Total_CAR_30cm, na.rm = T),
            SD= sd(Total_CAR_30cm, na.rm = T),
            N  = length(Total_CAR_30cm),
            SE = SD / sqrt(N)) %>%
  mutate(Depth = "15to30")


#Merge cars15 and cars30 together for a plot:
cars00to30 <- rbind(cars15, cars30)  

ggplot(cars00to30, aes(Treatment2, AV))  +
  geom_point(aes(color = Treatment2, size = 3)) +
  geom_errorbar( aes(ymin= AV+SE, ymax = AV-SE), width=.3)+
  labs(x= "", y = bquote('Carbon Accretion Rate ' (g*~cm^-2 ~y^-1)))
  ggtitle("Richmond River")+
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "none",
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))
  

