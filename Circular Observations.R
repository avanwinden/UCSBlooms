library(circular)
library(knitr)
library(ggplot2)
library(lubridate)
library(dplyr)
library(writexl)


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
all_observations_focal<-circular(Phenostages$doy.a, units = "degrees", template = "geographics")
mean.circular(all_observations_focal)
plot.circular(all_observations_focal, main = "All Observations", stack = TRUE)
arrows.circular(mean(all_observations_focal))
rayleigh.test(all_observations_focal)
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





#DID KNOWING FOCAL SPECIES CHANGE OBSERVATIONS?
#Here I want to look at day and not day of the year, to see if towards the end of the project changed observations 
yearish_all_obs<-read.csv("observations-537390.csv")
yearish_all_obs$day<-as.Date(yearish_all_obs$observed_on)
yearish_all_obs<-subset(yearish_all_obs,day <= "2020-04-01")
write_xlsx(yearish_all_obs, "data_frame1.xlsx")
yearish_all_obs<-read.csv("data_frame1.csv")
yearish_focal_obs<-read.csv("Species with Phenostage.csv")
yearish_focal_obs$day<-as.Date(yearish_focal_obs$Date, "%m/%d/%y")
yearish_focal_obs<-subset(yearish_focal_obs,day <= "2020-04-01")
write_xlsx(yearish_focal_obs,"data_frame2.xlsx")
yearish_focal_obs<-read.csv("data_frame2.csv")
#Cycle here is the year.ish march 11 2019 is 1, and march 17 2020 is 373
yearish_focal_obs$day.a <- (yearish_focal_obs$X*360)/373
yearish_all_obs$day.a <- (yearish_all_obs$daycycle*360)/373

yearish_all_obs_c<-circular(yearish_all_obs$day.a, units = "degrees", template = "geographics")
yearish_focal_obs_c<-circular(yearish_focal_obs$day.a, units = "degrees", template = "geographics")


plot.circular(yearish_all_obs_c,col = "green", main = "Focal vs All Observations", stack = TRUE, axes=FALSE)
arrows.circular(mean(yearish_all_obs_c), col="green")
points(yearish_focal_obs_c, col = "black", stack = TRUE)
arrows.circular(mean(yearish_focal_obs_c), col="black")
axis.circular(at=circular(seq(0, 2*pi-pi/2, pi/2)), 
              labels=c("doy x", "doy 1 and 373", "doy x", "doy x"))
mean.circular(yearish_all_obs_c)
mean.circular(yearish_focal_obs_c)


watson.two.test(yearish_all_obs_c,yearish_focal_obs_c)
#oh yeah polt statistic p is less than 0.001, focal species observations is different than all observations


