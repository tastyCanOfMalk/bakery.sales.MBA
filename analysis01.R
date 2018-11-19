if (!require(tibble)) install.packages("tibble")
library(tibble)
if (!require(lazyeval)) install.packages("lazyeval")
library(lazyeval)
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
if (!require(lubridate)) install.packages("lubridate")
library(lubridate)
if (!require(viridis)) install.packages("viridis")
library(viridis)
if (!require(ggthemes)) install.packages("ggthemes")
library(ggthemes)
if (!require(gridExtra)) install.packages("gridExtra")
library(gridExtra)
if (!require(ggridges)) install.packages("ggridges")
library(ggridges)

if (!require(arulesViz)) install.packages("arulesViz")
library(arulesViz)
if (!require(arules)) install.packages("arules")
library(arules)

setwd("/home/e/R/bakery.sales/")
# setwd("C:/Users/e/Documents/R/bakery.sales.MBA")

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
difftime(x$Date[nrow(x)],x$Date[1])
## EXPLORATION

# Most popular Items
x1 <- x %>% 
  group_by(Item) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count)) %>%
  slice(1:10) %>% 
  ggplot(aes(x=reorder(Item,Count),y=Count,fill=Item))+
  geom_bar(stat="identity")+
  coord_flip()+
  ggtitle("Most popular items")+
  theme(legend.position="none")
x1

# Charting # sales per date shows obvious peaks and valleys,
# There seems to be a decreased amount of sold items on weekdays with peaks on weekends
x2 <- x %>% 
  group_by(Date) %>% 
  summarise(Count= n()) %>% 
  mutate(Day=wday(Date,label=T)) %>% 
  ggplot(aes(x=Date,y=Count,fill=Day))+
  theme_fivethirtyeight()+
  geom_bar(stat="identity")+
  ggtitle("Line items sold per day")+
  scale_fill_viridis(discrete=TRUE)
x2

# We can see the obvious top sales day is Saturday, followed by Sun/Mon
x3 <- x %>% 
  mutate(Day = wday(Date,label=T)) %>% 
  group_by(Day) %>% 
  summarise(Count= n()) %>% 
  ggplot(aes(x=Day,y=Count,fill=Day))+
  theme_fivethirtyeight()+
  geom_bar(stat="identity")+
  ggtitle("Total line items sold per weekday")+
  theme(legend.position="none")
x3

# Grouping by unique transactions rather than line items tells a similar story
x4 <- x %>% 
  mutate(wday=wday(Date,label=T)) %>% 
  group_by(wday,Transaction) %>% 
  summarise(n_distinct(Transaction)) %>% 
  summarise(Count=n()) %>% 
  ggplot(aes(x=wday,y=Count,fill=wday))+
  theme_fivethirtyeight()+
  geom_bar(stat="identity")+
  ggtitle("Total unique transactions per weekday")+
  theme(legend.position="none")
x4

# We see some difference in scale which means, unsurprisingly,
# that each purchase is not just one single item.
# This does bring to mind a question of how many items are purchased per transaction,
# Let's graph this versus day of the week as we've done above
x5.1 <- x %>% 
  mutate(Day = wday(Date,label=T)) %>% 
  group_by(Day) %>% 
  summarise(Count= n()) 

x5.2 <- x %>% 
  mutate(wday=wday(Date,label=T)) %>% 
  group_by(wday,Transaction) %>% 
  summarise(n_distinct(Transaction)) %>% 
  summarise(Count=n())

x5.3 <- data.frame(x5.1, # Days, total items
                   x5.2[2], # unique transactions
                   x5.1[2]/x5.2[2])  # items per unique transaction
colnames(x5.3) <- c("Day","Line","Unique","Items.Trans")

x5 <- 
  ggplot(x5.3,aes(x=Day,y=Items.Trans,fill=Day))+
  theme_fivethirtyeight()+
  geom_bar(stat="identity")+
  ggtitle("Total unique transactions per weekday")+
  theme(legend.position="none")+
  geom_text(aes(label=round(Items.Trans,1)), vjust=2)
x5

# If we sort by the top sales days of all time and group by weekday
# the pattern becomes a little more easy to see, with Sunday being much
# more likely to take top item sales
x6 <- x %>% 
  group_by(Date) %>% 
  summarise(Count= n()) %>% 
  arrange(desc(Count)) %>% 
  slice(1:50) %>% 
  mutate(wday = wday(Date,label=T)) %>% 
  ggplot(aes(x=wday,y=Count,fill=wday))+
  theme_fivethirtyeight()+
  geom_bar(stat="identity")+
  ggtitle("Top 50 sales days, grouped by weekday")+
  theme(legend.position="none")
x6 

# We can also take a look at time data
# We see most items are sold just before noon
x7 <- x %>%
  mutate(Hour = as.factor(hour(x$Time))) %>% 
  group_by(Hour) %>% 
  summarise(Count=n()) %>% 
  ggplot(aes(x=Hour,y=Count,fill=Hour))+
  theme_fivethirtyeight()+
  geom_bar(stat="identity")+
  ggtitle("Line items sold per Hour")+
  theme(legend.position="none")
x7

# Again, let's look at unique transactions
# And again we see the scale change while the pattern remains
x8 <- x %>% 
  mutate(Hour = as.factor(hour(Time)))%>% 
  group_by(Hour,Transaction) %>% 
  summarise(n_distinct(Transaction)) %>% 
  summarise(Count=n()) %>% 
  ggplot(aes(x=Hour,y=Count,fill=Hour))+
  theme_fivethirtyeight()+
  geom_bar(stat="identity")+
  ggtitle("Unique transactions per Hour")+
  theme(legend.position="none")
x8

# We might as well also look into how many items per transaction occur
x9.1 <- x %>% 
  mutate(Hour = as.factor(hour(Time)))%>% 
  group_by(Hour) %>% 
  summarise(Count= n()) 

x9.2 <- x %>% 
  mutate(Hour = as.factor(hour(Time)))%>% 
  group_by(Hour,Transaction) %>% 
  summarise(n_distinct(Transaction)) %>% 
  summarise(Count=n())

x9.3 <- data.frame(x9.1, # Days, total items
                   x9.2[2], # unique transactions
                   x9.1[2]/x9.2[2])  # items per unique transaction
colnames(x9.3) <- c("Hour","Line","Unique","Items.Trans")

x9 <- 
  ggplot(x9.3,aes(x=Hour,y=Items.Trans,fill=Hour))+
  theme_fivethirtyeight()+
  geom_bar(stat="identity")+
  ggtitle("Total items per transaction per hour")+
  theme(legend.position="none")+
  geom_text(aes(label=round(Items.Trans,1)), vjust=2)
x9

# ridge plot of above data for each different weekday
# We can easily see where the most dense transaction times are this way,
# in essence this sort of summarizes all the data we've messed with above
x10 <- x %>% 
  mutate(Hour=as.factor(hour(Time)),
         Day=wday(Date,label=T)) %>% 
  group_by(Day,Hour,Transaction) %>% 
  summarise(Count=n()) %>% 
  ggplot(aes(x=Hour,y=Day,group=Day,fill=..density..))+
  theme_fivethirtyeight()+
  ggtitle("Transaction density per Hour by Day")+
  theme(legend.position="none")+
  geom_density_ridges_gradient(scale=3, rel_min_height=0.01)+
  scale_fill_viridis(option="magma")
x10

## MARKET BASKET ANALYSIS
# now that we've explored a little bit, we can get to the market basket analysis
# import transaction data

y <- read.transactions("data/BreadBasket_DMS.csv",
                       format="single",
                       cols=c(3,4),
                       sep=","
                       )
itemFrequencyPlot(y,topN=20,type="relative")
itemFrequencyPlot(y,topN=20,type="absolute")


# how to choose support and confidence levels?
# https://stackoverflow.com/questions/43588163/how-can-we-find-support-and-confident-in-apriori-for-rules
summary(y)
# Max support = .48 probably Coffee
summary(itemFrequency(y))
# Want items that appear at least 30 times minimum support could be set to:
30/length(y)
# with some trial and error we use .004 for support and confidence of .5
# support of 30 means the LHS and RHS items are bought together at least 4% of the time
# and purchasing the LHS means a 50% likelihood of purchasing the RHS
# this gives us a decent amount of rules to analyze
rules <- apriori(y,parameter=list(support=0.004,confidence=.5))
# https://rawgit.com/mhahsler/Introduction_to_Data_Mining_R_Examples/master/chap6.html
rules
inspect(head(rules,by="support",n=20))
quality(rules) <- cbind(quality(rules),
                        interestMeasure(rules, measure=c("phi", "gini"),y)
                        )
inspect(head(rules,by="support"))
inspect(head(rules,by="phi"))
inspect(head(rules,by="gini"))
inspect(head(rules,by="confidence",n=10))
inspect(head(rules,by="lift",n=10))
inspect(head(rules))
inspect(rules)


## VISUALIZE
plot(rules, measure=c("support","lift"), shading="confidence")
plot(rules, measure=c("support","lift"), shading="confidence",interactive = T)

plot(rules, method="graph")

plot(rules, method="graph", control=list(layout=igraph::in_circle()))

plot(rules, method="grouped")

plot(rules, method="paracoord", control=list(reorder=TRUE))

plot(rules, method = "two-key plot")
plot(rules, method = "two-key plot", interactive=TRUE)
# 3-order rules, convert to combination meal or something?

plot(rules, method="matrix", measure="lift")




# hmm but how do we know optimal support and confidence?
# first of all, define the terms
# support = how often an itemset appears in the dataset
  # e.g. support @ 2.5% = LHS and RHS bought together 2.5% of the time
# confidence = strength of association rule, e.g.,
# confidence of 1 implies whenever LHS was purchased, RHS was purchased 100% of the time

