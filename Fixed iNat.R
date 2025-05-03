library(ggplot2)
library(lubridate)
library(dplyr)


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

