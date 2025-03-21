library(circular)
library(knitr)
library(ggplot2)
library(lubridate)
library(dplyr)


Phenostages<-read.csv("Species with Phenostage.csv")
Phenostages$day<-as.Date(Phenostages$Date, "%m/%d/%y")
Phenostages$doy<- yday(Phenostages$day)
Phenostages$doy.a <- (Phenostages$doy*360)/365


##PEAK PHENOSTAGE
peak<-Phenostages%>%filter(Phenostage=="4")

berry_peak<-peak%>%filter(Species=="Lemonade Berry")
bush_peak<-peak%>%filter(Species=="California Brittlebush")
daisy_peak<-peak%>%filter(Species=="Trailing African Daisy ")
hawthron_peak<-peak%>%filter(Species=="Indian Hawthorn")
poppy_peak<-peak%>%filter(Species=="California Poppy")
pride_peak<-peak%>%filter(Species=="Pride of Madeira")

berry_peak_circ<-circular(berry_peak$doy.a, units="degrees", template="geographics")
bush_peak_circ<-circular(bush_peak$doy.a, units = "degrees", template = "geographics")
daisy_peak_circ<-circular(daisy_peak$doy.a, units = "degrees", template="geographics")
hawthron_peak_circ<-circular(hawthron_peak$doy.a, units = "degrees", template = "geographics")
poppy_peak_circ<-circular(poppy_peak$doy.a, units = "degrees", template = "geographics")
pride_peak_circ<-circular(pride_peak$doy.a, units = "degrees", template = "geographics")

mean.circular(berry_peak_circ)
rho.circular(berry_peak_circ)
mean.circular(bush_peak_circ)
rho.circular(bush_peak_circ)
mean.circular(daisy_peak_circ)
rho.circular(daisy_peak_circ)
mean.circular(hawthron_peak_circ)
rho.circular(hawthron_peak_circ)
mean.circular(poppy_peak_circ)
rho.circular(poppy_peak_circ)
mean.circular(pride_peak_circ)
rho.circular(pride_peak_circ)
# r vector lengths indicate that the poppy has the widest spread of peak flowering days 
#at r=.70 and next lowest r is 0.87 for the daisy 

plot.circular(berry_peak_circ, col="yellow", main ="Peak Bloom", stack=TRUE, axes=FALSE)
arrows.circular(mean(berry_peak_circ), col= "yellow")
points(bush_peak_circ, col = "red", stack=TRUE)
arrows.circular(mean(bush_peak_circ), col = "red")
points(daisy_peak_circ, col = "blue", stack=TRUE)
arrows.circular(mean(daisy_peak_circ), col ="blue")
points(hawthron_peak_circ, col="green", stack = TRUE)
arrows.circular(mean(hawthron_peak_circ), col="green")
points(poppy_peak_circ, col = "orange", stack = TRUE)
arrows.circular(mean(poppy_peak_circ), col="orange")
points(pride_peak_circ, col="purple", stack=TRUE)
arrows.circular(mean(pride_peak_circ), col="purple")
axis.circular(at=circular(seq(0, 2*pi-pi/2, pi/2)), 
              labels=c("doy 91", "doy 1 and 365", "doy 274", "doy 183"))

# now need to test significant differences between the spread (r) and the mean angles 