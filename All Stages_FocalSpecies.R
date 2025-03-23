library(circular)
library(knitr)
library(ggplot2)
library(lubridate)
library(dplyr)
library(zoo)

Phenostages<-read.csv("Species with Phenostage.csv")
Phenostages$day<-as.Date(Phenostages$Date, "%m/%d/%y")
Phenostages$doy<- yday(Phenostages$day)
Phenostages$doy.a <- (Phenostages$doy*360)/365

Phenostages$parabola<- Phenostages$Phenostage
Phenostages$parabola<-as.character(Phenostages$parabola)
Phenostages$parabola[Phenostages$parabola=="7"] <- "1"
Phenostages$parabola[Phenostages$parabola=="6"]<-"2"
Phenostages$parabola[Phenostages$parabola=="5"]<-"3"
Phenostages$parabola<- as.numeric(Phenostages$parabola)


Phenostages_plot <- ggplot(Phenostages, aes(x=doy, y=parabola, color=Species, fill=Species)) +
  geom_jitter(height = .2) +
  geom_smooth(method="loess")
Phenostages_plot +
  xlab("Day of the Year") +
  ylab("Stage")+
  ylim(1,4)+


Phenostages$Phenostage <- as.factor(Phenostages$Phenostage)
hist1_pheno<-ggplot(data=Phenostages, aes(x=day, fill=Phenostage)) +
  geom_histogram(position = "stack")
hist1_pheno+
  ylab("Count")+
  xlab("Date of Observation") +
  scale_fill_discrete(name="Phenostage",
                      breaks=c("1", "2", "3","4","5","6","7"),
                      labels=c("Not in Bloom", "Buds", 
                               "Leading Bloom", "Full Bloom",
                               "Trailing Bloom", "Very Trailing Bloom", "Seeds/Fruit/Not in Bloom"))
#day should be calculating for the date. Then why is all my observations showing in late march? we know the observations were strongest at the beginning of the project 
# unless is it true for focal speices, did the observations of focal go way up at the end of the project? Yes look at iNat.R and Circular Observations.R


all_plot <- ggplot(Phenostages, aes(x=doy, y=parabola)) +
  geom_jitter(height = .2) +
  geom_smooth(method="loess")
all_plot +
  xlab("Day of the Year") +
  ylab("Stage") +ylim(1,4)
#woah now this is an interesting graph. Does the interpretation shift if view date of observation?
all_plot_day <- ggplot(Phenostages, aes(x=day, y=parabola)) +
  geom_jitter(height = .2) +
  geom_smooth(method="loess")
all_plot_day +
  xlab("Date of Observation") +
  ylab("Stage") +ylim(1,4)
# yes it does, shows summer is really hard time to get observations