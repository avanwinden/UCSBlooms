library(circular)
library(knitr)
library(ggplot2)
library(lubridate)
library(dplyr)


Phenostages<-read.csv("Species with Phenostage.csv")
Phenostages$day<-as.Date(Phenostages$Date, "%m/%d/%y")
Phenostages$doy<- yday(Phenostages$day)
Phenostages$doy.a <- (Phenostages$doy*360)/365

Poppy<-Phenostages%>%filter(Species=="California Poppy")
Daisy<-Phenostages%>%filter(Species=="Trailing African Daisy ")
Pride<- Phenostages%>%filter(Species=="Pride of Madeira")
Berry<- Phenostages%>%filter(Species=="Lemonade Berry")
Bush<- Phenostages%>%filter(Species=="California Brittlebush")
Hawthorn<- Phenostages%>%filter(Species=="Indian Hawthorn")

## WHEN OBSERVATIONS ARE MADE
#That would just tell me tho if observation date is non random 
all_observations<-circular(Phenostages$doy.a, units = "degrees", template = "geographics")
mean.circular(all_observations)
plot.circular(all_observations, main = "All Observations", stack = TRUE, shrink = 10)
arrows.circular(mean(all_observations))
rayleigh.test(all_observations)
#p-value is 0: date of observation is not random, there is a time more observations made

Poppy_Observations<-circular(Poppy$doy.a, units="degrees", template="geographics")
mean.circular(Poppy_Observations)
plot.circular(Poppy_Observations, main= "Poppy Observations")
arrows.circular(mean(Poppy_Observations))

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
#do want to see if I can get them all on one 
#now the histogram is probably better for this but kinda intersting to see it plotted this way 

plot.circular(Poppy_Observations, col="yellow", main ="When Are Observations Made?", stack=TRUE, axes=FALSE)
arrows.circular(mean(Poppy_Observations), col= "yellow")
points(Pride_Observations, col = "red", stack=TRUE)
arrows.circular(mean(Pride_Observations), col = "red")
points(Hawthorn_Observations, col = "blue", stack=TRUE)
arrows.circular(mean(Hawthorn_Observations), col ="blue")
points(Daisy_Observations, col="green", stack = TRUE)
arrows.circular(mean(Daisy_Observations), col="green")
points(Bush_Observations, col = "orange", stack = TRUE)
arrows.circular(mean(Bush_Observations), col="orange")
points(Berry_Observations, col="purple", stack=TRUE)
arrows.circular(mean(Berry_Observations), col="purple")
axis.circular(at=circular(seq(0, 2*pi-pi/2, pi/2)), 
              labels=c("doy 91", "doy 1 and 365", "doy 274", "doy 183"))

watson.two.test(Poppy_Observations, Pride_Observations)
watson.two.test(Poppy_Observations,Bush_Observations)
watson.two.test(Daisy_Observations,Berry_Observations)

#looks like the species mean day of observation was signifcantly different, 
#Even the two with the closest mean observed date is significant between the species








