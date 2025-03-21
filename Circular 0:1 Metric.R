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

# We are seeing differences between the mean date (center of condition) for any flowering vs peak flowering
# really need to look into california brittlebush more. Big difference in mean day 
# and obviously data is going to be more spread when looking at all flowering vs peak in general
# however, poppy is now a lot closer to others in the group. Potentially peak looks 
#different in poppy observations to the phenostage assigners. Might have something to do with denisty of patches?


plot.circular(not_flowering_circ, col="red", main ="Not Flowering", stack=TRUE, axes=FALSE)
arrows.circular(mean(not_flowering_circ), col= "red")
# immediate looking just at the graph notice the mean is early in the year
mean.circular(not_flowering_circ)
rho.circular(not_flowering_circ)
rayleigh.test(not_flowering_circ)
# r value isn't extremely high so not all of the not flowering taken at same time but
# but the p vaule is 0 for the rayleiigh test. SO not random when not flowering has been observed. 
## well day 25 would actually be near the end of the project. before spring would be peak not flowering 

