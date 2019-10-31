#LOAD Richmond River DATA:=====
library(tidyverse)
library(gridExtra)
library(broom)

aa <- read.csv("RR_aboveground.csv")
bb <- read.csv("RR_belowground.csv")

#Correct Belowground data for compaction:=========
#list sites and corresponding treatments
SiteTreat <- read.csv("RR_SiteTreatment.csv")#to join Treatments to 

#Compute Mean corrected C-stock:
#Corrected C-stock is computed from percentage change in Depth_to after
#Accounting for compaction usinc Compaction Correction Value.
CorrectedDepthCarbon <- select(bb, Site_short,Site_Core, Treatment,Depth_to, Depth_Range,
                        Site_Core, Carbon_stock_in_section_Mg_Cha,
                        Lab_Compaction_Correction_Value)%>%
  filter(Treatment != "Saltmarsh_natural") %>%
  filter(Depth_to <= 100 ) %>%
  group_by(Site_Core)%>%
  mutate(TotalPerCore = sum (Carbon_stock_in_section_Mg_Cha)) %>%
  mutate( DepthCorrected = Depth_to / Lab_Compaction_Correction_Value, #Depth_to = recorded depth of core in the lab
          DepthChange = DepthCorrected-Depth_to,
          DepthChangePerc = 100-(DepthChange/Depth_to *100), #Percent increase in depth
          CorrectedCarbonStock = TotalPerCore * DepthChangePerc/100) %>% #Percent decrease in Carbon Stock when cut at 50cm
  group_by(Site_short) %>% #to sum total Carbon Stock per core.
  summarise(AV = mean(CorrectedCarbonStock, na.rm = T),
            SD = sd(CorrectedCarbonStock, na.rm = T),
            N = length(CorrectedCarbonStock),
            SE = SD / sqrt(N)) %>%
  mutate (Stock = "Belowground")

CorrectedDepthCarbon


#Add "Treatment2" collumn:
a <- left_join(CorrectedDepthCarbon, SiteTreat, by = "Site_short" )

#Compute aboveground C-Stock (mean +- SE):=========
aboveSum <- select (aa, Site_short, Total_Aboveground_Biomass_kg_100m2) %>%
  mutate(Total_Aboveground_Biomass_Mg_ha = Total_Aboveground_Biomass_kg_100m2 / 10 )%>% #1 kg/100m2 = 0.1 tonnes/ha
  gather(key = treat, value = mass, Total_Aboveground_Biomass_Mg_ha) %>% 
  group_by(Site_short) %>%
  summarise(AV=mean(mass, na.rm = T),
            SD=sd(mass),
            N = length(mass),
            SE= SD / sqrt(N)) %>%
  mutate (Stock = "Aboveground")

#Replace NaN with zeros:
aboveSum[is.na(aboveSum)] <- 0
b <-left_join(aboveSum, SiteTreat, by = "Site_short")
b


#Join/Plot above and below Stocks:=====
ab <- rbind (a,b) %>%
  mutate(AV= ifelse(Stock =="Aboveground",AV,AV*-1))%>%
  mutate(project = "RR")

ab_active <- filter(ab, Treatment2 == "Active")
ab_Passive  <- filter(ab, Treatment2 == "Passive")
ab_Disturbed <- filter(ab, Treatment2 == "Disturbed")
ab_Remnant <- filter(ab, Treatment2 == "Remnant")

#Draw a figure:
MyBreaks <- c(-300, -200,-100, -50, 0, 50, 100, 200,300 ,400,500)

ab1 <- ggplot(ab_active, aes(x=Site_short, y=AV, fill = Stock))+
  geom_bar(position="identity", stat="identity")+
  geom_errorbar( aes(ymin= AV+SE, ymax = AV-SE), width=.4)+
  geom_hline(yintercept=0)+
  scale_y_continuous(breaks = MyBreaks,labels = abs(MyBreaks), limits = c(-300,600))+ #abs to remove negative values on y-axis below 0
  scale_fill_manual(values = c("darkgreen","lightblue"))+
  xlab("Site")+ylab("")+
  ggtitle("Active")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=12),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=20),
        legend.position = "none",
        legend.text = element_text(size = 9),
        legend.title = element_text(face = "italic", size=10),
        plot.title = element_text(hjust = 0.9,lineheight=1.2, face="bold",size=20))
ab1

ab2 <- ggplot(ab_Passive, aes(x=Site_short, y=AV, fill = Stock))+
  geom_bar(position="identity", stat="identity")+
  geom_errorbar( aes(ymin= AV+SE, ymax = AV-SE), width=.4)+
  geom_hline(yintercept=0)+
  scale_y_continuous(breaks = MyBreaks,labels = abs(MyBreaks), limits = c(-300,600))+ #abs to remove negative values on y-axis below 0
  scale_fill_manual(values = c("darkgreen","lightblue"))+
  xlab("Site")+ ylab("")+
  ggtitle("Passive")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=12),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=20),
        legend.position = "none",
        legend.text = element_text(size = 6),
        legend.title = element_text(face = "italic",size=7),
        plot.title = element_text(hjust = 0.5,lineheight=1.2, face="bold",size=20))

ab3 <- ggplot(ab_Disturbed, aes(x=Site_short, y=AV, fill = Stock))+
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

ab4 <- ggplot(ab_Remnant, aes(x=Site_short, y=AV, fill = Stock))+
  geom_bar(position="identity", stat="identity")+
  geom_errorbar( aes(ymin= AV+SE, ymax = AV-SE), width=.4)+
  geom_hline(yintercept=0)+
  scale_y_continuous(breaks = MyBreaks,labels = abs(MyBreaks), limits = c(-300,600))+ #abs to remove negative values on y-axis below 0
  scale_fill_manual(values = c("darkgreen","lightblue"))+
  xlab("Site") + ylab("Organic Carbon Stock (Mg/ha)")+
  ggtitle("Remnant")+
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=12),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=20),
        legend.position = "none",
        legend.text = element_text(size = 6),
        legend.title = element_text(face = "italic",size=7),
        plot.title = element_text(hjust = 0.5,lineheight=1.2, face="bold",size=20))

grid.arrange(ab4,ab1,ab2,ab3, ncol = 4)


#Boxplot Mean corrected C-stock down to 50cm:=====
#For bottom part of Figure 3 in MS:
Paul <- select(bb, Site_short,Site_Core, Treatment,Treatment2,Depth_to, Depth_Range,
               Site_Core, Carbon_stock_in_section_Mg_Cha,
               Lab_Compaction_Correction_Value)%>%
  filter(Treatment != "Saltmarsh_natural") %>%
  filter(Depth_to <= 50 ) %>%
  group_by(Site_Core)%>%
  mutate(TotalPerCore = sum (Carbon_stock_in_section_Mg_Cha)) %>%
  mutate( DepthCorrected = Depth_to / Lab_Compaction_Correction_Value, #Depth_to = recorded depth of core in the lab
          DepthChange = DepthCorrected-Depth_to,
          DepthChangePerc = 100-(DepthChange/Depth_to *100), #Percent increase in depth
          CorrectedCarbonStock = TotalPerCore * DepthChangePerc/100) %>% #Percent decrease in Carbon Stock when cut at 50cm
  group_by(Treatment2,Site_Core) %>% #to sum total Carbon Stock per core.
  summarise(AV = mean(CorrectedCarbonStock, na.rm = T),
            SD = sd(CorrectedCarbonStock),
            N = length(CorrectedCarbonStock),
            SE = SD / sqrt(N)) %>%
  mutate (Stock = "Belowground")

Paul
#write.csv(Paul, file= "PaulsMeans.csv")#OLD FILE to 15 cm
#Paul$Treatment2 <- factor(Paul$Treatment2, levels = c("Active" ,"Passive",   "Disturbed",   "Remnant" ))
Paul$Treatment2 <- factor(Paul$Treatment2, levels = c(  "Remnant","Active" ,"Passive","Disturbed" ))

ggplot(Paul, aes(x=Treatment2, y=AV))+
  labs(fill = "Treatments")+
  labs(x = "",y ="Organic Carbon Stock (Mg/ha)" )+
  geom_boxplot() +
  theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=15, color = "black"),
        axis.text.y=element_text(size=15, color = "black"),
        axis.title.y=element_text(size=20, color = "black"))

