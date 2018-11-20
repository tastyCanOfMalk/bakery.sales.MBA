if (!require(tibble)) install.packages("tibble")
library(tibble)
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
if (!require(arules)) install.packages("arules")
library(arules)
if (!require(arulesViz)) install.packages("arulesViz")
library(arulesViz)

# IMPORT
setwd("/home/e/R/bakery.sales/")
# setwd("C:/Users/e/Documents/R/bakery.sales.MBA")

x <- read.csv("data/BreadBasket_DMS.csv") %>% 
  mutate(Date=as.Date(Date),
         Time=hms(Time)
         )
summary(x)
glimpse(x)
difftime(x$Date[nrow(x)],x$Date[1])

## EDA
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

# Market basket analysis
y <- read.transactions("data/BreadBasket_DMS.csv",
                       format="single",
                       cols=c(3,4),
                       sep=","
)
itemFrequencyPlot(y,topN=20,type="relative")
itemFrequencyPlot(y,topN=20,type="absolute")

summary(y)
summary(itemFrequency(y))
30/length(y)
rules <- apriori(y,parameter=list(support=0.004,confidence=.5))
rules
inspect(head(rules))
inspect(rules)
inspect(head(rules,by="support"))
inspect(head(rules,by="confidence",n=10))
inspect(head(rules,by="lift",n=10))

# VISUALIZE
plot(rules, measure=c("support","lift"), shading="confidence")
# plot(rules, measure=c("support","lift"), shading="confidence",interactive = T)
plot(rules, method="graph")
# plot(rules, method="graph",interactive = T)
plot(rules, method = "two-key plot")
plot(rules, method="grouped")
plot(rules, method="paracoord", control=list(reorder=TRUE))
# plot(rules, method = "two-key plot", interactive=TRUE)

ggsave("x1.png", x1, width = 6, height = 4, dpi = 300)
ggsave("x2.png", x2, width = 6, height = 4, dpi = 300)
ggsave("x3.png", x3, width = 6, height = 4, dpi = 300)
ggsave("x4.png", x4, width = 6, height = 4, dpi = 300)
ggsave("x5.png", x5, width = 6, height = 4, dpi = 300)
ggsave("x6.png", x6, width = 6, height = 4, dpi = 300)
ggsave("x7.png", x7, width = 6, height = 4, dpi = 300)
ggsave("x8.png", x8, width = 6, height = 4, dpi = 300)
ggsave("x9.png", x9, width = 6, height = 4, dpi = 300)
ggsave("x10.png", x10, width = 6, height = 4, dpi = 300)

