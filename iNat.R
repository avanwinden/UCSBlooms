library(tidyverse)

#Complete data exported from iNaturlaist for all observations in project
Complete_Project<-read.csv("observations-537390.csv")
#Breakdown (basic stats) of the sub-projects & umbrella project 
Summary_SubProjects<-read.csv("iNat_Projects.csv")
Summary_Project<-data.frame("observations"=5876,"observers"=39,"species"=396)
#UCSBlooms project first observation was March 11,2019 last was September 29,2021

#Top 8 species in UCSBlooms Project
ProjectSpecies<-data.frame("Hawthorn"=419,"Pride"=413,"brittlebush"=301,
                           "daisy"=273,"poppy"=228,"blue lily"= 203,
                           "lemonade berry"=151,"natal lily"=125)
#All UCSB Species same time frame
Summary_GeoPlant<-data.frame("observations"=9048,"observers"=331,"species"=547)
Summary_GeoAll<-data.frame("observations"=12833,"observers"=572,"species"=1146)
#top 8 species of all observations in same area/time... first non-plant is Honey Bee with 99
GeoSpecies<-data.frame("pride"=468,"brittlebush"=458,"hawthorn"=435,"poppy"=298,
                       "daisy"=288,"blue lily"=209,"lemonade berry"=179,"natal lily"=132)
#UCSBlooms project accounts for APPROX 65% of plant and 46% of all observations during the same time/area

#UCSBlooms Project - Vested Interest Observers Question
PublicObservers<- Complete_Project %>% filter(user_login!="avanwinden",user_login!="taylorc",
                                              user_login!="charliethrift",user_login!="michellelee",
                                              user_login!="excarpobro")
#Lab group made 3,331 observations. 5 people account for 56.69% of the observations in the project
