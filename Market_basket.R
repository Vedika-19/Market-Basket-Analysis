#dataset has been taken from UCI Machine Learning Repository
library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(arules)
library(arulesViz)
library(plyr)
library(dplyr)

#READING THE DATA AND PREPROCESSING
retail <- read_excel('Online Retail.xlsx')
retail <- retail[complete.cases(retail), ]
retail <- retail %>% mutate(Description = as.factor(Description))
retail <- retail %>% mutate(Country = as.factor(Country))
retail$Date <- as.Date(retail$InvoiceDate)
retail$Time <- format(retail$InvoiceDate,"%H:%M:%S")
retail$InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))
glimpse(retail)

#WHAT TIME DO PEOPLE OFTER PURCHASE ONLINE?
retail$Time <- as.factor(retail$Time)
a <- hms(as.character(retail$Time))
head(a)
retail$Time = hour(a)
retail %>% ggplot(aes(x=Time)) + geom_histogram(stat="count",fill="indianred")

#HOW MANY ITEMS DO EACH CUSTOMER BUY
detach("package:plyr", unload=TRUE)
retail %>% 
  group_by(InvoiceNo) %>% 
  summarize(n_items = mean(Quantity)) %>%
  ggplot(aes(x=n_items))+
  geom_histogram(fill="indianred", bins = 100000) + 
  geom_rug()+
  coord_cartesian(xlim=c(0,80))

#Top 10 best sellers
tmp <- retail %>% 
  group_by(StockCode, Description) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))
tmp <- head(tmp, n=10)
tmp
tmp %>% 
  ggplot(aes(x=reorder(Description,count), y=count))+
  geom_bar(stat="identity",fill="indian red")+
  coord_flip()

#creating item list 
retail_sorted <- retail[order(retail$CustomerID),]
library(plyr)
itemList <- ddply(retail,c("CustomerID","Date"), 
                  function(df1)paste(df1$Description, 
                                     collapse = ","))
#removing cust. ID and date as we only need the list of items purchased
itemList$CustomerID <- NULL
itemList$Date <- NULL
colnames(itemList) <- c("items")

write.csv(itemList,"market_basket.csv", quote = FALSE, row.names = TRUE)
tr <- read.transactions('market_basket.csv', format = 'basket', sep=',')
tr
summary(tr)

library(RColorBrewer)
itemFrequencyPlot(tr,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")
itemFrequencyPlot(tr,topN=20,type="relative",col=brewer.pal(8,'Pastel2'), main="Relative Item Frequency Plot")

#APRIORI ALGORITHM
rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8))
rules <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules)

inspect(rules[1:10])
topRules <- rules[1:10]
plot(rules)
plot(topRules, method="graph",engine='htmlwidget')
plot(topRules, method = "grouped")
subRules2<-head(rules, n=20, by="lift")
plot(subRules2, method="paracoord")
plot(topRules, method="paracoord")
