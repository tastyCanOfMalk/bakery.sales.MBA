if (!require(tibble)) install.packages("tibble")
if (!require(arules)) install.packages("arules")
if (!require(lubridate)) install.packages("lubridate")
if (!require(tidyverse)) install.packages("tidyverse")
library(tibble)
library(arules)
library(lubridate)
library(tidyverse)

setwd("/home/e/R/bakery.sales/")


# Import our csv and look at the structure
x <- read.csv("data/BreadBasket_DMS.csv") %>% 
  mutate(Date=as.Date(Date),
         Time=hms(Time)
         )
summary(x)
glimpse(x)

# Most popular Items
x %>% 
  group_by(Item) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count)) %>%
  slice(1:10) %>% 
  ggplot(aes(x=reorder(Item,Count),y=Count,fill=Item))+
  geom_bar(stat="identity")+
  coord_flip()+
  ggtitle("Most popular items")+
  theme(legend.position="none")
  scale_fill_brewer(palette="")

# sales per date
# not useful but shows there are some obvious patterns via the waves
# we'll see below these peaks occur on Sundays
x %>% 
  group_by(Date) %>% 
  summarise(Count= n()) %>% 
  ggplot(aes(x=Date,y=Count))+
  geom_bar(stat="identity")

# Our data seems to have been recorded between the dates of 
# 2016-10-30 and 2017-04-09
# This limits our any analysis from yearly or monthly sales to weekly sales
x$Date[1]
x$Date[nrow(x)]

# We can see the obvious top sales day is Sunday, followed by Mon/Sat
x %>% 
  mutate(wday = wday(Date)) %>% 
  group_by(wday) %>% 
  summarise(Count= n()) %>% 
  ggplot(aes(x=wday,y=Count))+
  geom_bar(stat="identity")+
  scale_x_discrete(limits=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))+
  ggtitle("Items sold per weekday")
  
# If we sort by the top sales days of all time and group by weekday
# the pattern becomes a little more easy to see, with Sunday being much
# more likely to take top item sales
x %>% 
  group_by(Date) %>% 
  summarise(Count= n()) %>% 
  arrange(desc(Count)) %>% 
  slice(1:50) %>% 
  mutate(wday = wday(Date)) %>% 
  ggplot(aes(x=wday,y=Count))+
  geom_bar(stat="identity")+
  scale_x_discrete(limits=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))+
  ggtitle("Top 50 sales days, grouped by weekday")

# We can also take a look at time data since we have it


glimpse(xx)
as.factor(hour(x$Time))



y <- read.transactions("data/BreadBasket_DMS.csv",
                       format="single",
                       cols=c(3,4),
                       sep=",",
                       )

itemFrequencyPlot(y,topN=10,type="absolute")




# summary(x)
# glimpse(x)

