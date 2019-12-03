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
  mutate(CAR_gcm2y = C_perc * MAR) %>%
  mutate (Stock = "Belowground") %>%
  filter(Depth_to < 31 ) %>% #till 30 because that is how deep the Age-dating core was
  group_by(Site_Core)%>%
  mutate(Total_CAR_30cm = sum (CAR_gcm2y)) %>%
  group_by(Treatment2) %>%
  summarise(AV = mean(Total_CAR_30cm, na.rm = T),
            SD = sd(Total_CAR_30cm, na.rm = T),
            N = length(Total_CAR_30cm),
            SE = SD / sqrt(N))

ggplot(cars, aes(Treatment2, AV))  +
  geom_point() +
  geom_errorbar( aes(ymin= AV+SE, ymax = AV-SE), width=.4)+
  xlab("")+ ylab("Mean CAR in top 30cm")+
  ggtitle("Carbon Accretion Rates \n (CAR, g/cm2/y)")+
  theme_minimal()
  
  

