library(circular)
library(knitr)
library(ggplot2)
library(lubridate)
library(dplyr)


Phenostages<-read.csv("Species with Phenostage.csv")
Phenostages$day<-as.Date(Phenostages$Date, "%m/%d/%y")
Phenostages$doy<- yday(Phenostages$day)
Phenostages$doy.a <- (Phenostages$doy*360)/365

#okay so i think now i seperate by species but then also by stage?
#I need to look at how other papers have done it 

Poppy<-Phenostages%>%filter(Species=="California Poppy")
Daisy<-Phenostages%>%filter(Species=="Trailing African Daisy ")
Pride<- Phenostages%>%filter(Species=="Pride of Madeira")
Berry<- Phenostages%>%filter(Species=="Lemonade Berry")
Bush<- Phenostages%>%filter(Species=="California Brittlebush")
Hawthorn<- Phenostages%>%filter(Species=="Indian Hawthorn")

## WHEN OBSERVATIONS ARE MADE
# this is just going to look at when observations are made 
Poppy_Observations<-circular(Poppy$doy.a, units="degrees", template="geographics")
mean.circular(Poppy_Observations)
plot.circular(Poppy_Observations, main= "Poppy Observations")
arrows.circular(mean(Poppy_Observations))

#oh i could do it for all. That would just tell me tho if observation date is non random 
all_observations<-circular(Phenostages$doy.a, units = "degrees", template = "geographics")
mean.circular(all_observations)
plot.circular(all_observations, main = "All Observations", stack = TRUE, shrink = 10)
arrows.circular(mean(all_observations))
rayleigh.test(all_observations)
#p-value is 0: date of observation is not random, there is a time more observations made

#back to individual species observations
Daisy_Observations<-circular(Daisy$doy.a, units="degrees", template="geographics")
mean.circular(Daisy_Observations)
plot.circular(Daisy_Observations, main= "Daisy Observations")
arrows.circular(mean(Daisy_Observations))

Pride_Observations<-circular(Pride$doy.a, units="degrees", template="geographics")
mean.circular(Pride_Observations)
plot.circular(Pride_Observations, main= "Pride Observations")
arrows.circular(mean(Pride_Observations))

Berry_Observations<-circular(Berry$doy.a, units="degrees", template="geographics")
mean.circular(Berry_Observations)
plot.circular(Berry_Observations, main= "Berry Observations")
arrows.circular(mean(Berry_Observations))

Bush_Observations<-circular(Bush$doy.a, units="degrees", template="geographics")
mean.circular(Bush_Observations)
plot.circular(Bush_Observations, main= "Bush Observations")
arrows.circular(mean(Bush_Observations))

Hawthorn_Observations<-circular(Hawthorn$doy.a, units="degrees", template="geographics")
mean.circular(Hawthorn_Observations)
plot.circular(Hawthorn_Observations, main= "Hawthorn Observations")
arrows.circular(mean(Hawthorn_Observations))

##okay just looking at the means, the species all are more likely to be observed around the same dates






