library(ggplot2)
library(lubridate)
library(dplyr)
library(circular)
library(knitr)

#INAT/OBSERVATIONS
Complete_Project<-read.csv("observations-537390.csv")
Complete_Project$day <- as.Date(Complete_Project$observed_on)
UCSBlooms<-subset(Complete_Project,day <= "2020-03-18")
UCSBlooms$month <- month(UCSBlooms$day)
#UCSBlooms is the data included in the 03/11/2019 - 03/17/2020 run of the project
length(unique(UCSBlooms$user_id))

blitz<-subset(UCSBlooms, day =="2019-04-06"|day=="2019-05-05"|day=="2019-06-07"|
                day=="2019-10-19"|day=="2019-11-16"|day=="2020-01-11"|day=="2020-02-09"
              |day=="2020-03-07")
length(unique(blitz$user_id))
month<-subset(UCSBlooms, day !="2019-04-06"&day!="2019-05-05"&day!="2019-06-07"&
                day!="2019-10-19"&day!="2019-11-16"&day!="2020-01-11"&day!="2020-02-09"
              &day!="2020-03-07")
length(unique(month$user_id))

PublicObservations<-UCSBlooms%>% filter(user_login!="avanwinden",user_login!="taylorc",
                                             user_login!="charliethrift",
                                             user_login!="michellelee",user_login!="excarpobro")

combined<-UCSBlooms%>% mutate(blitz=case_when(day =="2019-04-06"|day=="2019-05-05"|day=="2019-06-07"|
                                                day=="2019-10-19"|day=="2019-11-16"|day=="2020-01-11"|day=="2020-02-09"
                                              |day=="2020-03-07"~"B"))
combined %>% ggplot(aes(x=day,fill=blitz))+ 
  geom_histogram(position = "stack" , binwidth = 30) + xlab("Date of Observation") +
  ylab("Number of Observations") +
  scale_fill_discrete(name = "BioBlitz", labels = c("Yes", "No"))



#PEAK BLOOM
Phenostages<-read.csv("Species with Phenostage.csv")
Phenostages$day<-as.Date(Phenostages$Date, "%m/%d/%y")
Phenostages$doy<- yday(Phenostages$day)
Phenostages$doy.a <- (Phenostages$doy*360)/365

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

{layout(matrix(c(1,2,3,4,5,6),3,2))
  par(mar = c(1, .2, 1, .2))
  
  plot.circular(berry_peak_circ, col="#60992D", main ="Rhus integrifolia", stack= TRUE, axes=FALSE,
                bins = 365, shrink = 1.5, tol = 0, sep = 0.15)
  axis.circular(at=circular(seq(0, 2*pi-pi/2, pi/2)), 
                labels=c("", "JAN 1", "", "JUL 2"), cex = 0.5, tcl.text = -0.1)
  arrows.circular(mean(berry_peak_circ), col= "#60992D")
  
  plot.circular(bush_peak_circ, col = "#F6BB2D", main = "Encelia californica", axes = FALSE, 
                stack = TRUE, bins = 365, shrink = 1.5, tol = 0, sep = 0.15) 
  axis.circular(at=circular(seq(0, 2*pi-pi/2, pi/2)), 
                labels=c("", "JAN 1", "", "JUL 2"), cex = 0.5, tcl.text = -0.1 )
  arrows.circular(mean(bush_peak_circ), col = "#F6BB2D")
  
  plot.circular(daisy_peak_circ, col = "#B68CB8", main = "Osteospermum fruticosum", axes = FALSE, 
                stack = TRUE, bins = 365, shrink = 1.5, tol = 0, sep = 0.15)  
  axis.circular(at=circular(seq(0, 2*pi-pi/2, pi/2)), 
                labels=c("", "JAN 1", "", "JUL 2"), cex = 0.5, tcl.text = -0.1 )
  arrows.circular(mean(daisy_peak_circ), col ="#B68CB8")
  
  plot.circular(hawthron_peak_circ, col = "#E660BA", main = "Rhaphiolepis indica", axes = FALSE, 
                stack = TRUE, bins = 365, shrink = 1.5, tol = 0, sep = 0.15)  
  axis.circular(at=circular(seq(0, 2*pi-pi/2, pi/2)), 
                labels=c("", "JAN 1", "", "JUL 2"), cex = 0.5, tcl.text = -0.1 )
  arrows.circular(mean(hawthron_peak_circ), col="#E660BA")
  
  plot.circular(poppy_peak_circ, col = "#F26D19", main = "Eschscholzia californica", axes = FALSE, 
                stack = TRUE, bins = 365, shrink = 1.5, tol = 0, sep = 0.15)  
  axis.circular(at=circular(seq(0, 2*pi-pi/2, pi/2)), 
                labels=c("", "JAN 1", "", "JUL 2"), cex = 0.5, tcl.text = -0.1 )
  arrows.circular(mean(poppy_peak_circ), col="#F26D19")
  
  plot.circular(pride_peak_circ, col = "#615096", main = "Echium candicans", axes = FALSE, 
                stack = TRUE, bins = 365, shrink = 1.5, tol = 0, sep = 0.15)  
  axis.circular(at=circular(seq(0, 2*pi-pi/2, pi/2)), 
                labels=c("", "JAN 1", "", "JUL 2"), cex = 0.5, tcl.text = -0.1 )
  arrows.circular(mean(pride_peak_circ), col="#615096") 
}


#ENTIRE BLOOM PERIOD 

YN_Flower <- Phenostages %>%
  mutate(flower=case_when(Phenostage=="2" | Phenostage=="1" |Phenostage=="7" ~ "0",
                          Phenostage=="3" | Phenostage=="4"| Phenostage=="5"| Phenostage=="6" ~ "1"))

Flowering<-YN_Flower%>%filter(flower=="1")
Not_Flowering<-YN_Flower%>%filter(flower=="0")
flowering_circ<-circular(Flowering$doy.a, units="degrees", template="geographics")
not_flowering_circ<-circular(Not_Flowering$doy.a, units = "degrees",template = "geographics")

PoppyYN<-Flowering%>%filter(Species=="California Poppy")
DaisyYN<-Flowering%>%filter(Species=="Trailing African Daisy ")
PrideYN<- Flowering%>%filter(Species=="Pride of Madeira")
BerryYN<- Flowering%>%filter(Species=="Lemonade Berry")
BushYN<- Flowering%>%filter(Species=="California Brittlebush")
HawthornYN<- Flowering%>%filter(Species=="Indian Hawthorn")
berryYN_circ<-circular(BerryYN$doy.a, units="degrees", template="geographics")
bushYN_circ<-circular(BushYN$doy.a, units = "degrees", template = "geographics")
daisyYN_circ<-circular(DaisyYN$doy.a, units = "degrees", template="geographics")
hawthornYN_circ<-circular(HawthornYN$doy.a, units = "degrees", template = "geographics")
poppyYN_circ<-circular(PoppyYN$doy.a, units = "degrees", template = "geographics")
prideYN_circ<-circular(PrideYN$doy.a, units = "degrees", template = "geographics")

mean.circular(berryYN_circ)
rho.circular(berryYN_circ)
rayleigh.test(berryYN_circ)
mean.circular(bushYN_circ)
rho.circular(bushYN_circ)
rayleigh.test(bushYN_circ)
mean.circular(daisyYN_circ)
rho.circular(daisyYN_circ)
rayleigh.test(daisyYN_circ)
mean.circular(hawthornYN_circ)
rho.circular(hawthornYN_circ)
rayleigh.test(hawthornYN_circ)
mean.circular(poppyYN_circ)
rho.circular(poppyYN_circ)
rayleigh.test(poppyYN_circ)
mean.circular(prideYN_circ)
rho.circular(prideYN_circ)
rayleigh.test(prideYN_circ)

plot.circular(flowering_circ, col="green", main ="All Flowering", 
              axes=FALSE,stack = TRUE, bins = 365, shrink = 2, tol = 0, sep = 0.015)
axis.circular(at=circular(seq(0, 2*pi-pi/2, pi/2)), 
              labels=c("", "JAN 1", "", "JUL 2"), cex = 0.5, tcl.text = -0.1 )
arrows.circular(mean(flowering_circ), col= "green")

{layout(matrix(c(1,2,3,4,5,6),3,2))
  par(mar = c(1, .2, 1, .2))
  plot.circular(berryYN_circ, col="#60992D", main ="Rhus integrifolia", stack= TRUE, axes=FALSE,
                bins = 365, shrink = 1.5, tol = 0, sep = 0.028)
  axis.circular(at=circular(seq(0, 2*pi-pi/2, pi/2)), 
                labels=c("", "JAN 1", "", "JUL 2"), cex = 0.5, tcl.text = -0.1)
  arrows.circular(mean(berryYN_circ), col= "#60992D")
  plot.circular(bushYN_circ, col = "#F6BB2D", main = "Encelia californica", axes = FALSE, 
                stack = TRUE, bins = 365, shrink = 1.5, tol = 0, sep = 0.028) 
  axis.circular(at=circular(seq(0, 2*pi-pi/2, pi/2)), 
                labels=c("", "JAN 1", "", "JUL 2"), cex = 0.5, tcl.text = -0.1 )
  arrows.circular(mean(bushYN_circ), col = "#F6BB2D")
  plot.circular(daisyYN_circ, col = "#B68CB8", main = "Osteospermum fruticosum", axes = FALSE, 
                stack = TRUE, bins = 365, shrink = 1.5, tol = 0, sep = 0.028)  
  axis.circular(at=circular(seq(0, 2*pi-pi/2, pi/2)), 
                labels=c("", "JAN 1", "", "JUL 2"), cex = 0.5, tcl.text = -0.1 )
  arrows.circular(mean(daisyYN_circ), col ="#B68CB8")
  plot.circular(hawthronYN_circ, col = "#E660BA", main = "Rhaphiolepis indica", axes = FALSE, 
                stack = TRUE, bins = 365, shrink = 1.5, tol = 0, sep = 0.028)  
  axis.circular(at=circular(seq(0, 2*pi-pi/2, pi/2)), 
                labels=c("", "JAN 1", "", "JUL 2"), cex = 0.5, tcl.text = -0.1 )
  arrows.circular(mean(hawthronYN_circ), col="#E660BA")
  plot.circular(poppyYN_circ, col = "#F26D19", main = "Eschscholzia californica", axes = FALSE, 
                stack = TRUE, bins = 365, shrink = 1.5, tol = 0, sep = 0.028)  
  axis.circular(at=circular(seq(0, 2*pi-pi/2, pi/2)), 
                labels=c("", "JAN 1", "", "JUL 2"), cex = 0.5, tcl.text = -0.1 )
  arrows.circular(mean(poppyYN_circ), col="#F26D19")
  plot.circular(prideYN_circ, col = "#615096", main = "Echium candicans", axes = FALSE, 
                stack = TRUE, bins = 365, shrink = 1.5, tol = 0, sep = 0.028)  
  axis.circular(at=circular(seq(0, 2*pi-pi/2, pi/2)), 
                labels=c("", "JAN 1", "", "JUL 2"), cex = 0.5, tcl.text = -0.1 )
  arrows.circular(mean(prideYN_circ), col="#615096")}





