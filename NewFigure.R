library(ggplot2)
library(lubridate)
library(dplyr)
library(circular)
library(knitr)

Complete_Project<-read.csv("observations-537390.csv")
Complete_Project$day <- as.Date(Complete_Project$observed_on)
UCSBlooms<-subset(Complete_Project,day <= "2020-03-18")
UCSBlooms$month <- month(UCSBlooms$day)

users <- UCSBlooms$user_login


UCSBlooms %>% ggplot(aes(x=user_login))+ 
  geom_bar() + xlab("Observer") +
  ylab("Number of Observations")


UCSBlooms %>% 
  ggplot(aes(x=reorder(user_login, user_login, function(x)-length(x))))+ 
  geom_bar() + xlab("Observer") +
  ylab("Number of Observations")


UCSBlooms %>%
  count(user_login) %>%
  slice_max(n, n = 20) %>%  
  ggplot(aes(x = reorder(user_login, -n), y = n)) + 
  geom_bar(stat = "identity") + 
  xlab("Observer") + 
  ylab("Number of Observations") + 
  ggtitle("Count of observations of top 20 observers") +
  theme(axis.text.x = element_blank())




  