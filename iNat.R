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


## did species counts go down? did observers knowing the focal species change anything?

## number of observations in bloom vs not in bloom?
Phenostages<-read.csv("Species with Phenostage.csv")
flowers<-Phenostages%>%filter(Phenostage!="2",Phenostage!="1",Phenostage!="7")
poppy<-Phenostages%>%filter(Species=="California Poppy")
poppy_flowers<-poppy%>%filter(Phenostage!="2",Phenostage!="1",Phenostage!="7")
hawthorn<-Phenostages%>%filter(Species=="Indian Hawthorn")
hawthorn_flowers<-hawthorn%>%filter(Phenostage!="2",Phenostage!="1",Phenostage!="7")
brittlebush<-Phenostages%>%filter(Species=="California Brittlebush")
brittlebush_flowers<-brittlebush%>%filter(Phenostage!="2",Phenostage!="1",Phenostage!="7")
pride<-Phenostages%>%filter(Species=="Pride of Madeira")
pride_flowers<-pride%>%filter(Phenostage!="2",Phenostage!="1",Phenostage!="7")
berry<-Phenostages%>%filter(Species=="Lemonade Berry")
berry_flowers<-berry%>%filter(Phenostage!="2",Phenostage!="1",Phenostage!="7")
daisy<-Phenostages%>%filter(Species=="Trailing African Daisy ")
daisy_flowers<-daisy%>%filter(Phenostage!="2",Phenostage!="1",Phenostage!="7")
# worth running a test 
#run a statistical tests for all 6 species? is the amount of bloom of poppy differnet than the others?
blooms<-data.frame(In_Bloom=c(128,138,119,110,43,162),total=c(149,262,162,285,85,191),
                   percent=c(85.90,52.67,73.46,38.60,50.59,84.82), 
                   row.names = c("Poppy","Hawthorn","Brittlebush","Pride","Berry","Daisy"))
percent_bloom<-c(85.90,52.67,73.46,38.60,50.59,84.82)
t<-chisq.test(percent) #i dont think thisis correct

## any effects of the pandemic to explore?



