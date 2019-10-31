#LOAD Richmond River DATA:=====
library(tidyverse)
library(gridExtra)
library(sjPlot)
library(sjmisc)


aa <- read.csv("RR_aboveground.csv")
bb <- read.csv("RR_belowground.csv")

#RR_Model:=========
#list sites and corresponding treatments
SiteTreat <- read.csv("RR_SiteTreatment.csv")#to join Treatments to 

#Compute Mean corrected C-stock:
#Corrected C-stock is computed from percentage change in Depth_to after
#Accounting for compaction usinc Compaction Correction Value.
CorrectedCarbon <- select(bb, Site_short,Site_Core, Treatment,Depth_to, Depth_Range,
                               Site_Core, Carbon_stock_in_section_Mg_Cha,
                               Lab_Compaction_Correction_Value)%>%
  filter(Treatment != "Saltmarsh_natural") %>%
  filter(Depth_to <= 100 ) %>%
  group_by(Site_Core)%>%
  mutate(TotalPerCore = sum (Carbon_stock_in_section_Mg_Cha)) %>%
  mutate( DepthCorrected = Depth_to / Lab_Compaction_Correction_Value, #Depth_to = recorded depth of core in the lab
          DepthChange = DepthCorrected-Depth_to,
          DepthChangePerc = 100-(DepthChange/Depth_to *100), #Percent increase in depth
          CorrectedCarbonStock = TotalPerCore * DepthChangePerc/100) %>%#Percent decrease in Carbon Stock when cut at 50cm
  group_by(Site_Core) %>%  #to sum total Carbon Stock per core.
  summarise(OC = sum(CorrectedCarbonStock, na.rm = T)) %>%
  separate(Site_Core,c("Site","Core_Num"), sep = "_") #Separate to have factor for lm
            
CorrectedCarbon

m <- left_join(CorrectedCarbon,SiteTreat, by = "Site") #join to get Site_Type info for lm model
m$Treatment2 <- factor(m$Treatment2, levels = c("Disturbed", "Remnant" ,"Active", "Passive" ))
m$Site_Type <- m$Treatment2

model <- lm (OC ~ Site_Type, data = m )
summary(rr_model)
tab_model(model)
