#Join 210Pb Data:
#for cores where average value for MAR using the CF.CS model is produced,
# multiply this by the % C in each section to obtain the CAR in gC/m2/yr (Carbon Accretion Rate):
#MAR values are based on file sent from Pere: "Pb210 dating Cores Mangrove Rehab Carnell 2019"

library(tidyverse)
library(gridExtra)

bb <- read.csv("RR_belowground.csv") #All Data
SiteTreat <- read.csv("RR_SiteTreatment.csv")# Treatments to join to All Data

#Create dataset off Pere's file above. MAR's units = g/cm2/y
MAR <- data.frame( Site = c("MNND","SNND","MNDC","MDBP",
                            "MDSD","MR1991","MR1992","MRAP",
                            "MRCH1","MRCH2","MRPL","MDTUCK", "MNPL"),
                   MAR = c(0.095, 0.096, NA,     0, 
                           0,    0.0324 ,0.076,0.32,
                           NA,  0.16,NA, NA   ,0.045))

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
  mutate(Mean_CAR_30cm = mean(CAR_gcm2y[Depth_to <=30])) %>%   #from 0 cm to 15 cm
  group_by(Treatment2) %>%
  summarise(AV = mean(Mean_CAR_30cm*100, na.rm = T),
            SD = sd(Mean_CAR_30cm*100, na.rm = T),
            N  = length(Mean_CAR_30cm),
            SE = SD / sqrt(N))  %>% 
  mutate (Depth = "00to30")

cars30$Treatment2 <- factor(cars30$Treatment2, levels = c("Active","Passive","Remnant", "Disturbed"))
ggplot(cars30, aes(Treatment2, AV))  +
  geom_point(aes(shape = Treatment2, size = 3)) +
  geom_errorbar( aes(ymin= AV+SE, ymax = AV-SE), width=.3)+
  labs(x= "", y = bquote('Carbon Accretion Rate ' (Mg*~ha^-1 ~y^-1)))+
  theme_bw() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "none",
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))

