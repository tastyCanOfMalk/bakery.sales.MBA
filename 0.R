if (!require(tibble)) install.packages("tibble")
if (!require(arules)) install.packages("arules")
library(tibble)
library(arules)

setwd("/home/e/R/bakery.sales/")

x <- read.csv("data/BreadBasket_DMS.csv")
summary(x)
glimpse(x)

# great guide: https://www.kaggle.com/xvivancos/market-basket-analysis#
# transactions per month, week, hour?
# number of rules found vs confidence interval? @ various support levels

# Start with association analysis, "Market Basket Analysis"
# which items are bought with which other items? using arules package

y <- read.transactions("data/BreadBasket_DMS.csv", 
                       format="single",
                       cols=c(3,4),
                       sep=",",
                       rm.duplicates = T) 
# summary(y)
# glimpse(y)
# item frequencies, calculates support for common items
y.frequentItems <- eclat(y, parameter=list(supp=.05,maxlen=15))
inspect(y.frequentItems)
itemFrequencyPlot(y,topN=10,type="absolute",main="Item Frequency")

# generate rules
y.rules <- apriori(y,parameter=list(supp=.001,conf=.5))

# high support sorting
# support is indication of how frequently item appears
# support of an item = how often it appears out of all transactions
y.rules.supp <- sort (y.rules, by="support", decreasing=T)
inspect(head(y.rules.supp))
# high confidence sorting
# confidence of 1 implies whenever LHS was purchased, 
# RHS was purchased 100% of the time
# confidence of item = strength of the association rule
y.rules.conf <- sort (y.rules, by="confidence", decreasing=T)
inspect(head(y.rules.conf))
# high lift  sorting
# a rule with 10, implies LHS and RHS 10x more likely purchased together
# calculates how much the presence of Y influences the presence of X
y.rules.lift <- sort (y.rules, by="lift", decreasing=T)
inspect(head(y.rules.lift))

