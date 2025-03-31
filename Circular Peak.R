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
rayleigh.test(berry_peak_circ)
mean.circular(bush_peak_circ)
rho.circular(bush_peak_circ)
rayleigh.test(bush_peak_circ)
mean.circular(daisy_peak_circ)
rho.circular(daisy_peak_circ)
rayleigh.test(daisy_peak_circ)
mean.circular(hawthron_peak_circ)
rho.circular(hawthron_peak_circ)
rayleigh.test(hawthron_peak_circ)
mean.circular(poppy_peak_circ)
rho.circular(poppy_peak_circ)
rayleigh.test(poppy_peak_circ)
mean.circular(pride_peak_circ)
rho.circular(pride_peak_circ)
rayleigh.test(pride_peak_circ)
e# r vector lengths indicate that the poppy has the widest spread of peak flowering days 
#at r=.70 and next lowest r is 0.87 for the daisy 

#This needs a legend I think
{plot.circular(berry_peak_circ, col="yellow", main ="Peak Bloom", stack=TRUE, axes=FALSE,)
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
              labels=c("91", "365", "274", "183"))}




#Rose Diagrams, bins of 75 would be about every 5 days #then do for all and print on one screen
#would be nice to report the n and theta number here maybe? 
{layout(matrix(c(1,2,3,4,5,6),3,2))
  par(mar = c(1, .2, 1, .2))
rose.diag(berry_peak_circ, bins = 75, main = "Berry Peak n=6", axes = FALSE, prop = 1)
axis.circular(at=circular(seq(0, 2*pi-pi/2, pi/2)), 
              labels=c("91", "365", "274", "183"), cex = 0.5, tcl.text = -0.1 )
arrows.circular(mean(berry_peak_circ), col= "yellow")
rose.diag(bush_peak_circ, bins = 75, main = "Bush Peak n=21", axes = FALSE, prop = 1) 
axis.circular(at=circular(seq(0, 2*pi-pi/2, pi/2)), 
              labels=c("91", "365", "274", "183"), cex = 0.5, tcl.text = -0.1 )
arrows.circular(mean(bush_peak_circ), col = "red")
rose.diag(daisy_peak_circ, bins = 75, main = "Daisy Peak n=46", axes = FALSE, prop = 1) 
axis.circular(at=circular(seq(0, 2*pi-pi/2, pi/2)), 
              labels=c("91", "365", "274", "183"), cex = 0.5, tcl.text = -0.1 )
arrows.circular(mean(daisy_peak_circ), col ="blue")
rose.diag(hawthron_peak_circ, bins = 75, main = "Thorn Peak n=24", axes = FALSE, prop = 1) 
axis.circular(at=circular(seq(0, 2*pi-pi/2, pi/2)), 
              labels=c("91", "365", "274", "183"), cex = 0.5, tcl.text = -0.1 )
arrows.circular(mean(hawthron_peak_circ), col="green")
rose.diag(poppy_peak_circ, bins = 75, main = "Poppy Peak n=60", axes = FALSE, prop = 1) 
axis.circular(at=circular(seq(0, 2*pi-pi/2, pi/2)), 
              labels=c("91", "365", "274", "183"), cex = 0.5, tcl.text = -0.1 )
arrows.circular(mean(poppy_peak_circ), col="orange")
rose.diag(pride_peak_circ, bins = 75, main = "Pride Peak n=34", axes = FALSE, prop = 1) 
axis.circular(at=circular(seq(0, 2*pi-pi/2, pi/2)), 
              labels=c("91", "365", "274", "183"), cex = 0.5, tcl.text = -0.1 )
arrows.circular(mean(pride_peak_circ), col="purple")}


### now need to test significant differences between the spread (r) and the mean angles 

#Watson Williams Test, then pairsewsie posthoc comparisons. Testing the means

peak<-Phenostages%>%filter(Phenostage=="4")
all_circ_peak<-circular(peak$doy.a, units="degrees", template="geographics")
watson.williams.test(all_circ_peak,peak$Species)

bush.thorn1<-peak%>%filter(Species=="California Brittlebush"|Species=="Indian Hawthorn")
bush.thorn<-circular(bush.thorn1$doy.a, units = "degrees",template = "geographics")
watson.williams.test(bush.thorn,bush.thorn1$Species)

bush.berry1<-peak%>%filter(Species=="California Brittlebush"|Species=="Lemonade Berry")
bush.berry<-circular(bush.berry1$doy.a, units = "degrees",template="geographics")
watson.williams.test(bush.berry, bush.berry1$Species)

berry.thorn1<-peak%>%filter(Species=="Lemonade Berry"|Species=="Indian Hawthorn")
berry.thorn<-circular(berry.thorn1$doy.a, units = "degrees", template = "geographics")
watson.williams.test(berry.thorn, berry.thorn1$Species)

berry.poppy1<-peak%>%filter(Species=="Lemonade Berry"|Species=="California Poppy")
berry.poppy<-circular(berry.poppy1$doy.a, units = "degrees", template = "geographics")
watson.williams.test(berry.poppy,berry.poppy1$Species)

berry.daisy1<-peak%>%filter(Species=="Lemonade Berry"|Species=="Trailing African Daisy ")
berry.daisy<-circular(berry.daisy1$doy.a, units = "degrees", template = "geographics")
watson.williams.test(berry.daisy, berry.daisy1$Species)

berry.pride1<-peak%>%filter(Species=="Lemonade Berry"|Species=="Pride of Madeira")
berry.pride<-circular(berry.pride1$doy.a, units = "degrees", template = "geographics")
watson.williams.test(berry.pride, berry.pride1$Species)

thorn.poppy1<-peak%>%filter(Species=="Indian Hawthorn"|Species=="California Poppy")
thorn.poppy<-circular(thorn.poppy1$doy.a, units = "degrees", template = "geographics")
watson.williams.test(thorn.poppy, thorn.poppy1$Species)

thorn.daisy1<-peak%>%filter(Species=="Indian Hawthorn"|Species=="Trailing African Daisy ")
thorn.daisy<-circular(thorn.daisy1$doy.a, units = "degrees", template = "geographics")
watson.williams.test(thorn.daisy, thorn.daisy1$Species)

thorn.pride1<-peak%>%filter(Species=="Indian Hawthorn"|Species=="Pride of Madeira")
thorn.pride<-circular(thorn.pride1$doy.a, units = "degrees", template = "geographics")
watson.williams.test(thorn.pride, thorn.pride1$Species)

bush.poppy1<-peak%>%filter(Species=="California Brittlebush"|Species=="California Poppy")
bush.poppy<-circular(bush.poppy1$doy.a, units = "degrees", template = "geographics")
watson.williams.test(bush.poppy,bush.poppy1$Species)

bush.daisy1<-peak%>%filter(Species=="California Brittlebush"|Species=="Trailing African Daisy ")
bush.daisy<-circular(bush.daisy1$doy.a, units = "degrees", template = "geographics")
watson.williams.test(bush.daisy, bush.daisy1$Species)

bush.pride1<-peak%>%filter(Species=="California Brittlebush"|Species=="Pride of Madeira")
bush.pride<-circular(bush.pride1$doy.a, units = "degrees", template = "geographics")
watson.williams.test(bush.pride, bush.pride1$Species)

poppy.daisy1<-peak%>%filter(Species=="California Poppy"|Species=="Trailing African Daisy ")
poppy.daisy<-circular(poppy.daisy1$doy.a, units = "degrees", template = "geographics")
watson.williams.test(poppy.daisy, poppy.daisy1$Species)

poppy.pride1<-peak%>%filter(Species=="California Poppy"|Species=="Pride of Madeira")
poppy.pride<-circular(poppy.pride1$doy.a, units = "degrees", template="geographics")
watson.williams.test(poppy.pride, poppy.pride1$Species)

daisy.pride1<-peak%>%filter(Species=="Trailing African Daisy "|Species=="Pride of Madeira")
daisy.pride<-circular(daisy.pride1$doy.a, units = "degrees", template = "geographics")
watson.williams.test(daisy.pride, daisy.pride1$Species)



