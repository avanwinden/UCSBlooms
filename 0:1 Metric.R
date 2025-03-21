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

Flowering<-YN_Flower%>%filter(flower=="1")
Not_Flowering<-YN_Flower%>%filter(flower=="0")



