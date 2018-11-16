if (!require(tibble)) install.packages("tibble")
if (!require(arules)) install.packages("arules")
if (!require(lubridate)) install.packages("lubridate")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(ggridges)) install.packages("ggridges")
library(tibble)
library(arules)
library(lubridate)
library(tidyverse)
library(ggridges)

setwd("/home/e/R/bakery.sales/")

# Import our csv and look at the structure
  # Some things to note: each item occupies one row, 
    # one transaction therefore may occupy multiple rows
  # Range of data collection is from 2016-10-30 to 2017-04-09
x <- read.csv("data/BreadBasket_DMS.csv") %>% 
  mutate(Date=as.Date(Date),
         Time=hms(Time)
         )
summary(x)
glimpse(x)

## EXPLORATION

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
  # scale_fill_brewer(palette="")

# Charting # sales per date shows obvious peaks and valleys,
# There seems to be a decreased amount of sold items on weekdays with peaks on weekends
x %>% 
  group_by(Date) %>% 
  summarise(Count= n()) %>% 
  ggplot(aes(x=Date,y=Count,fill=Count))+
  geom_bar(stat="identity")+
  theme(legend.position="none")

# We can see the obvious top sales day is Sunday, followed by Mon/Sat
x %>% 
  mutate(wday = wday(Date)) %>% 
  group_by(wday) %>% 
  summarise(Count= n()) %>% 
  ggplot(aes(x=wday,y=Count,fill=wday))+
  geom_bar(stat="identity")+
  scale_x_discrete(limits=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))+
  ggtitle("Items sold per weekday")+
  theme(legend.position="none")
  
# Grouping by unique transactions rather than line items tells a similar story
x %>% 
  mutate(wday=wday(Date)) %>% 
  group_by(wday,Transaction) %>% 
  summarise(n_distinct(Transaction)) %>% 
  summarise(Count=n()) %>% 
  ggplot(aes(x=wday,y=Count,fill=wday))+
  geom_bar(stat="identity")+
  scale_x_discrete(limits=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))+
  ggtitle("Unique transactions per weekday")+
  theme(legend.position="none")

# If we sort by the top sales days of all time and group by weekday
# the pattern becomes a little more easy to see, with Sunday being much
# more likely to take top item sales
x %>% 
  group_by(Date) %>% 
  summarise(Count= n()) %>% 
  arrange(desc(Count)) %>% 
  slice(1:50) %>% 
  mutate(wday = wday(Date)) %>% 
  ggplot(aes(x=wday,y=Count,fill=wday))+
  geom_bar(stat="identity")+
  scale_x_discrete(limits=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))+
  ggtitle("Top 50 sales days, grouped by weekday")+
  theme(legend.position="none")

# We can also take a look at time data
x %>%
  mutate(Hour = as.factor(hour(x$Time))) %>% 
  group_by(Hour) %>% 
  summarise(Count=n()) %>% 
  ggplot(aes(x=Hour,y=Count,fill=Hour))+
  geom_bar(stat="identity")+
  theme(legend.position="none")

# joyplot of above data for each different weekday
x %>% 
  mutate(Hour = hour(Time),
         Day = as.factor(wday(Date))) %>% 
  group_by(Hour, Day) %>% 
  summarise(Count=n()) %>% 
  ggplot(aes(x=Hour,y=Day,fill=Day))+
  stat_density_ridges(scale=2,quantile_lines = TRUE,alpha=.7)+
  scale_fill_viridis(discrete = TRUE)+
  scale_x_continuous(limits=c(1,24),
                     breaks=seq(1,24,1))+
  theme(legend.position="none")+
  ggtitle("Line items sold per hour per day (1-Mon,7-Sun)")


## MARKET BASKET ANALYSIS
# now that we've explored a little bit, we can get to the market basket analysis
# import transaction data

y <- read.transactions("data/BreadBasket_DMS.csv",
                       format="single",
                       cols=c(3,4),
                       sep=","
                       )
summary(y)
# generate the rules
rules <-  apriori(y, parameter=list(supp=.1, conf=.5))
rules <- apriori(y,parameter=list(supp = 0.001, conf=0.5))
summary(rules)
# hmm but how do we know optimal support and confidence?
# first of all, define the terms
# support = how often an itemset appears in the dataset
# confidence = strength of association rule, e.g.,
# confidence of 1 implies whenever LHS was purchased, RHS was purchased 100% of the time

summary(rules)
