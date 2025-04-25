library(ggplot2)
library(lubridate)
library(dplyr)




#Observations
Complete_Project<-read.csv("observations-537390.csv")
Complete_Project$day <- as.Date(Complete_Project$observed_on)
year_Project<-subset(Complete_Project,day <= "2020-04-01")
Summary_SubProjects<-read.csv("iNat_Projects.csv")
Summary_SubProjects$month<- as.Date(Summary_SubProjects$month_year, "%m/%d/%y")
year_SubProjects<-subset(Summary_SubProjects, month <="2020-04-01")

year_SubProjects %>% 
  ggplot()+
  geom_col(aes(x=month,y=observations,fill=bioblitz),position = "stack") +
  geom_col(aes(x=month, y=-observers, fill=bioblitz), position = "stack")
#looks like not a good way to do the wanted objective, and might be confusing for the viwer.
##wonder if could put the number in the bar?

