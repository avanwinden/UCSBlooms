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

flowering_circ<-circular(Flowering$doy.a, units="degrees", template="geographics")
not_flowering_circ<-circular(Not_Flowering$doy.a, units = "degrees",template = "geographics")

plot.circular(flowering_circ, col="green", main ="Flowering", stack=TRUE, axes=FALSE)
arrows.circular(mean(flowering_circ), col= "green")
points(not_flowering_circ, col = "red", stack=TRUE)
arrows.circular(mean(not_flowering_circ), col = "red")

plot.circular(not_flowering_circ, col="red", main ="Not Flowering", stack=TRUE, axes=FALSE)
arrows.circular(mean(not_flowering_circ), col= "red")
# immediate looking just at the graph notice the mean is early in the year
mean.circular(not_flowering_circ)
rho.circular(not_flowering_circ)
rayleigh.test(not_flowering_circ)
# r value isn't extremely high so not all of the not flowering taken at same time but
# but the p vaule is 0 for the rayleiigh test. SO not random when not flowering has been observed. 
## well day 25 would actually be near the end of the project. before spring would be peak not flowering 


watson.two.test(flowering_circ, not_flowering_circ)
# when looking at summed focal species and bloom and not bloom categories there is a significant difference in groups

#now I want to seperate by species 

PoppyYN<-Flowering%>%filter(Species=="California Poppy")
DaisyYN<-Flowering%>%filter(Species=="Trailing African Daisy ")
PrideYN<- Flowering%>%filter(Species=="Pride of Madeira")
BerryYN<- Flowering%>%filter(Species=="Lemonade Berry")
BushYN<- Flowering%>%filter(Species=="California Brittlebush")
HawthornYN<- Flowering%>%filter(Species=="Indian Hawthorn")

berryYN_circ<-circular(BerryYN$doy.a, units="degrees", template="geographics")
bushYN_circ<-circular(BushYN$doy.a, units = "degrees", template = "geographics")
daisyYN_circ<-circular(DaisyYN$doy.a, units = "degrees", template="geographics")
hawthronYN_circ<-circular(HawthornYN$doy.a, units = "degrees", template = "geographics")
poppyYN_circ<-circular(PoppyYN$doy.a, units = "degrees", template = "geographics")
prideYN_circ<-circular(PrideYN$doy.a, units = "degrees", template = "geographics")

mean.circular(berryYN_circ)
rho.circular(berryYN_circ)
mean.circular(bushYN_circ)
rho.circular(bushYN_circ)
mean.circular(daisyYN_circ)
rho.circular(daisyYN_circ)
mean.circular(hawthronYN_circ)
rho.circular(hawthronYN_circ)
mean.circular(poppyYN_circ)
rho.circular(poppyYN_circ)
mean.circular(prideYN_circ)
rho.circular(prideYN_circ)

rayleigh.test(berryYN_circ)
rayleigh.test(bushYN_circ)
rayleigh.test(daisyYN_circ)
rayleigh.test(hawthronYN_circ)
rayleigh.test(poppyYN_circ)
rayleigh.test(prideYN_circ)

# We are seeing differences between the mean date (center of condition) for any flowering vs peak flowering
# really need to look into california brittlebush more. Big difference in mean day
# bush and poppy are the only two where the eman day is later. Look into that 

{layout(matrix(c(1,2,3,4,5,6),3,2))
  par(mar = c(1, .2, 1, .2))
  rose.diag(berryYN_circ, bins = 75, main = "Berry Flowering n=", axes = FALSE, prop = 1)
  axis.circular(at=circular(seq(0, 2*pi-pi/2, pi/2)), 
                labels=c("91", "365", "274", "183"), cex = 0.5, tcl.text = -0.1 )
  arrows.circular(mean(berryYN_circ), col= "yellow")
  rose.diag(bushYN_circ, bins = 75, main = "Bush Flowering n=", axes = FALSE, prop = 1) 
  axis.circular(at=circular(seq(0, 2*pi-pi/2, pi/2)), 
                labels=c("91", "365", "274", "183"), cex = 0.5, tcl.text = -0.1 )
  arrows.circular(mean(bushYN_circ), col = "red")
  rose.diag(daisyYN_circ, bins = 75, main = "Daisy Flowering n=", axes = FALSE, prop = 1) 
  axis.circular(at=circular(seq(0, 2*pi-pi/2, pi/2)), 
                labels=c("91", "365", "274", "183"), cex = 0.5, tcl.text = -0.1 )
  arrows.circular(mean(daisyYN_circ), col ="blue")
  rose.diag(hawthronYN_circ, bins = 75, main = "Thorn Flowering n=", axes = FALSE, prop = 1) 
  axis.circular(at=circular(seq(0, 2*pi-pi/2, pi/2)), 
                labels=c("91", "365", "274", "183"), cex = 0.5, tcl.text = -0.1 )
  arrows.circular(mean(hawthronYN_circ), col="green")
  rose.diag(poppyYN_circ, bins = 75, main = "Poppy Flowering n=", axes = FALSE, prop = 1) 
  axis.circular(at=circular(seq(0, 2*pi-pi/2, pi/2)), 
                labels=c("91", "365", "274", "183"), cex = 0.5, tcl.text = -0.1 )
  arrows.circular(mean(poppyYN_circ), col="orange")
  rose.diag(prideYN_circ, bins = 75, main = "Pride Flowering n=", axes = FALSE, prop = 1) 
  axis.circular(at=circular(seq(0, 2*pi-pi/2, pi/2)), 
                labels=c("91", "365", "274", "183"), cex = 0.5, tcl.text = -0.1 )
  arrows.circular(mean(prideYN_circ), col="purple")}


bush.thorn2<-Flowering%>%filter(Species=="California Brittlebush"|Species=="Indian Hawthorn")
bush.thorn<-circular(bush.thorn2$doy.a, units = "degrees",template = "geographics")
watson.williams.test(bush.thorn,bush.thorn2$Species)

bush.berry2<-Flowering%>%filter(Species=="California Brittlebush"|Species=="Lemonade Berry")
bush.berry<-circular(bush.berry2$doy.a, units = "degrees",template="geographics")
watson.williams.test(bush.berry, bush.berry2$Species)

berry.thorn2<-Flowering%>%filter(Species=="Lemonade Berry"|Species=="Indian Hawthorn")
berry.thorn<-circular(berry.thorn2$doy.a, units = "degrees", template = "geographics")
watson.williams.test(berry.thorn, berry.thorn2$Species)

berry.poppy2<-Flowering%>%filter(Species=="Lemonade Berry"|Species=="California Poppy")
berry.poppy<-circular(berry.poppy2$doy.a, units = "degrees", template = "geographics")
watson.williams.test(berry.poppy,berry.poppy2$Species)

berry.daisy2<-Flowering%>%filter(Species=="Lemonade Berry"|Species=="Trailing African Daisy ")
berry.daisy<-circular(berry.daisy2$doy.a, units = "degrees", template = "geographics")
watson.williams.test(berry.daisy, berry.daisy2$Species)

berry.pride2<-Flowering%>%filter(Species=="Lemonade Berry"|Species=="Pride of Madeira")
berry.pride<-circular(berry.pride2$doy.a, units = "degrees", template = "geographics")
watson.williams.test(berry.pride, berry.pride2$Species)

thorn.poppy2<-Flowering%>%filter(Species=="Indian Hawthorn"|Species=="California Poppy")
thorn.poppy<-circular(thorn.poppy2$doy.a, units = "degrees", template = "geographics")
watson.williams.test(thorn.poppy, thorn.poppy2$Species)

thorn.daisy2<-Flowering%>%filter(Species=="Indian Hawthorn"|Species=="Trailing African Daisy ")
thorn.daisy<-circular(thorn.daisy2$doy.a, units = "degrees", template = "geographics")
watson.williams.test(thorn.daisy, thorn.daisy2$Species)

thorn.pride2<-Flowering%>%filter(Species=="Indian Hawthorn"|Species=="Pride of Madeira")
thorn.pride<-circular(thorn.pride2$doy.a, units = "degrees", template = "geographics")
watson.williams.test(thorn.pride, thorn.pride2$Species)

bush.poppy2<-Flowering%>%filter(Species=="California Brittlebush"|Species=="California Poppy")
bush.poppy<-circular(bush.poppy2$doy.a, units = "degrees", template = "geographics")
watson.williams.test(bush.poppy,bush.poppy2$Species)

bush.daisy2<-Flowering%>%filter(Species=="California Brittlebush"|Species=="Trailing African Daisy ")
bush.daisy<-circular(bush.daisy2$doy.a, units = "degrees", template = "geographics")
watson.williams.test(bush.daisy, bush.daisy2$Species)

bush.pride2<-Flowering%>%filter(Species=="California Brittlebush"|Species=="Pride of Madeira")
bush.pride<-circular(bush.pride2$doy.a, units = "degrees", template = "geographics")
watson.williams.test(bush.pride, bush.pride2$Species)

poppy.daisy2<-Flowering%>%filter(Species=="California Poppy"|Species=="Trailing African Daisy ")
poppy.daisy<-circular(poppy.daisy2$doy.a, units = "degrees", template = "geographics")
watson.williams.test(poppy.daisy, poppy.daisy2$Species)

poppy.pride2<-Flowering%>%filter(Species=="California Poppy"|Species=="Pride of Madeira")
poppy.pride<-circular(poppy.pride2$doy.a, units = "degrees", template="geographics")
watson.williams.test(poppy.pride, poppy.pride2$Species)

daisy.pride2<-Flowering%>%filter(Species=="Trailing African Daisy "|Species=="Pride of Madeira")
daisy.pride<-circular(daisy.pride2$doy.a, units = "degrees", template = "geographics")
watson.williams.test(daisy.pride, daisy.pride2$Species)






