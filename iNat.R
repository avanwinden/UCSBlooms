library(ggplot2)
library(lubridate)
library(dplyr)

#SUMMARY
#Complete data exported from iNaturlaist for all observations in project
Complete_Project<-read.csv("observations-537390.csv")
Complete_Project$day <- as.Date(Complete_Project$observed_on)
year_Project<-subset(Complete_Project,day <= "2020-04-01")
#Breakdown (basic stats) of the sub-projects & umbrella project 
Summary_SubProjects<-read.csv("iNat_Projects.csv")
Summary_SubProjects$month<- as.Date(Summary_SubProjects$month_year, "%m/%d/%y") #anytime within month assigned to first of month
year_SubProjects<-subset(Summary_SubProjects, month <="2020-04-01")
Summary_Project<-data.frame("observations"=5876,"observers"=39,"species"=396)


#SPECIES
#Top 8 species in UCSBlooms Project
ProjectSpecies<-data.frame("Hawthorn"=419,"Pride"=413,"brittlebush"=301,
                           "daisy"=273,"poppy"=228,"blue lily"= 203,
                           "lemonade berry"=151,"natal lily"=125)
#All UCSB Species same time frame
Summary_GeoPlant<-data.frame("observations"=9048,"observers"=331,"species"=547)
Summary_GeoAll<-data.frame("observations"=12833,"observers"=572,"species"=1146)
#top 8 species of all observations in same area/time
GeoSpecies<-data.frame("pride"=468,"brittlebush"=458,"hawthorn"=435,"poppy"=298,
                       "daisy"=288,"blue lily"=209,"lemonade berry"=179,"natal lily"=132)

#OBSERVERS AND OBSERVATIONS
#UCSBlooms Project - Vested Interest Observers Question
PublicObservers<- Complete_Project %>% filter(user_login!="avanwinden",user_login!="taylorc",
                                              user_login!="charliethrift",
                                              user_login!="michellelee",user_login!="excarpobro")
year_PublicObservers<-year_Project%>% filter(user_login!="avanwinden",user_login!="taylorc",
                                             user_login!="charliethrift",
                                             user_login!="michellelee",user_login!="excarpobro")
#historagm of all observations
hist(Complete_Project$day, "month",freq = TRUE, xlab="Date of Observation", 
     ylab="Number of Observations", main = "UCSBlooms Observations")
hist(year_Project$day, "month",freq = TRUE, xlab="Date of Observation", 
     ylab="Number of Observations", main = "UCSBlooms Observations")
#graph separating type of observation
Summary_SubProjects %>% ggplot(aes(x=month,y=observations,fill=bioblitz))+ 
  geom_col(position = "stack") 
year_SubProjects %>% ggplot(aes(x=month,y=observations,fill=bioblitz))+ 
  geom_col(position = "stack")



