library(circular)
library(knitr)
library(ggplot2)
library(lubridate)
library(dplyr)


Phenostages<-read.csv("Species with Phenostage.csv")
Phenostages$day<-as.Date(Phenostages$Date, "%m/%d/%y")
Phenostages$doy<- yday(Phenostages$day)
Phenostages$doy.a <- (Phenostages$doy*360)/365

YN_Flower <- Phenostages %>%
  mutate(flower=case_when(Phenostage=="2" | Phenostage=="1" |Phenostage=="7" ~ "0",
    Phenostage=="3" | Phenostage=="4"| Phenostage=="5"| Phenostage=="6" ~ "1"))

YN_Flower %>% ggplot(aes(x=doy,fill=flower))+geom_histogram()+
  labs(title = "Observations in Flower", y="Total Observations",x="Day of Year" )

Flowering<-YN_Flower%>%filter(flower=="1")
Not_Flowering<-YN_Flower%>%filter(flower=="0")

flowering_circ<-circular(Flowering$doy.a, units="degrees", template="geographics")
not_flowering_circ<-circular(Not_Flowering$doy.a, units = "degrees",template = "geographics")

plot.circular(flowering_circ, col="green", main ="Flowering", stack=TRUE, axes=FALSE)
arrows.circular(mean(flowering_circ), col= "green")
points(not_flowering_circ, col = "red", stack=TRUE)
arrows.circular(mean(not_flowering_circ), col = "red")

watson.two.test(flowering_circ, not_flowering_circ)
# when looking at summed focal species and bloom and not bloom categories there is a significant difference in groups



