#Question: Do the non-native species act differently than the native species?
#could see that peak isn't much different but that invasives satrt to bloom earlier. (pretty sure thats what the literature review in general was finding)
library(circular)
library(knitr)
library(ggplot2)
library(lubridate)
library(dplyr)
library(patchwork)
Phenostages<-read.csv("Species with Phenostage.csv")
Phenostages$day<-as.Date(Phenostages$Date, "%m/%d/%y")
Phenostages$doy<- yday(Phenostages$day)
Phenostages$doy.a <- (Phenostages$doy*360)/365

#Q1: Do invasive have a different peak bloom?

peak<-Phenostages%>%filter(Phenostage=="4")

nativeP<-peak%>%filter(Species=="Lemonade Berry"|Species=="California Brittlebush"|
                        Species=="California Poppy")

invasiveP<-peak%>%filter(Species=="Indian Hawthorn"|Species=="Trailing African Daisy "|
                          Species=="Pride of Madeira")


nativeP_circ<-circular(nativeP$doy.a, units="degrees", template="geographics")
invasiveP_circ<-circular(invasiveP$doy.a, units = "degrees", template = "geographics")

mean.circular(nativeP_circ)
mean.circular(invasiveP_circ)

#Q2: Do Invasive bloom earlier? Look at stage 3: leading blooms 

leading<-Phenostages%>%filter(Phenostage=="3")

berry_lead<-leading%>%filter(Species=="Lemonade Berry")
bush_lead<-leading%>%filter(Species=="California Brittlebush")
daisy_lead<-leading%>%filter(Species=="Trailing African Daisy ")
hawthron_lead<-leading%>%filter(Species=="Indian Hawthorn")
poppy_lead<-leading%>%filter(Species=="California Poppy")
pride_lead<-leading%>%filter(Species=="Pride of Madeira")

berry_lead_circ<-circular(berry_lead$doy.a, units="degrees", template="geographics")
bush_lead_circ<-circular(bush_lead$doy.a, units = "degrees", template = "geographics")
daisy_lead_circ<-circular(daisy_lead$doy.a, units = "degrees", template="geographics")
hawthron_lead_circ<-circular(hawthron_lead$doy.a, units = "degrees", template = "geographics")
poppy_lead_circ<-circular(poppy_lead$doy.a, units = "degrees", template = "geographics")
pride_lead_circ<-circular(pride_lead$doy.a, units = "degrees", template = "geographics")

mean.circular(berry_lead_circ)
mean.circular(bush_lead_circ)
mean.circular(daisy_lead_circ)
mean.circular(hawthron_lead_circ)
mean.circular(poppy_lead_circ)
mean.circular(pride_lead_circ)

nativeL<-leading%>%filter(Species=="Lemonade Berry"|Species=="California Brittlebush"|
                         Species=="California Poppy")

invasiveL<-leading%>%filter(Species=="Indian Hawthorn"|Species=="Trailing African Daisy "|
                           Species=="Pride of Madeira")


nativeL_circ<-circular(nativeL$doy.a, units="degrees", template="geographics")
invasiveL_circ<-circular(invasiveL$doy.a, units = "degrees", template = "geographics")

mean.circular(nativeL_circ)
mean.circular(invasiveL_circ)




