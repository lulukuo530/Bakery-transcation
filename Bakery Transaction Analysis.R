rm(list=ls())

#import libraries
# ctrl+shift+c: multiple lines comments
library(tidyverse)
library(dplyr)
library(reshape2)
library(data.table)   #data資料G以上
library(lubridate)
library(arules)    #association rules
library(arulesViz) #association rules
#library(cowplot)   #跟ggplot類似
library(ggplot2)
#library(plotrix)
library(RColorBrewer)


##### 1 - Explore Data #####
data = fread("~/Desktop/R/meetup/bakery-transaction-master/BreadBasket_DMS.csv",sep=",",stringsAsFactors=F)
str(data)
summary(data)
head(data)
tail(data)

###Correct date and time column types
data <- data %>%
  mutate(
    Date = as.Date(Date),  #data$Date = mutate(data, as.Date(Date))
    Time = as.ITime(Time)  #date$Time = mutate(data, as.ITime(Time))
  )
str(data)

###find how many unique items that we have
data_item <- data %>%
  group_by(Item) %>%
  summarise(Count = n()) 

lengths(data_item)[2]

# Count 
# 95 


##### 2 - Clean Data #####
###clean the data
word_list = c("NaN", "-", "nan", "NAN", "None", "NONE", "none", " ", "_", ".")
found<-c()
 for ( i in c(1:length(word_list))) {
    if (sum(which(data$Item==word_list[i]))>0) {
    found=c(found,i)
    }
 }   #可以用%in%寫


# Found word types is 1 so only one of thing in my list founded in our data ("NONE")    
found_words=word_list[found]

# how many of them are "NONE" 
nrow(data[which(data$Item == "NONE"),])
# [1] 786

# Data include 786 missing values let's drop them
data_clean = data[!data$Item %in% found_words, ]
# data_clean = data[data$Item!="NONE",]
# data_clean = data[!data$Item=="NONE",]
# data_clean = data[-which(data$Item=="NONE"),]
#以上這幾行意思都一樣

# Let's look again unique Item list it must be 94.
data_item1 <- data_clean %>%
  group_by(Item) %>%
  summarise(Count = n()) %>%
  data.frame()

lengths(data_item1)[2]

# Count 
# 94

#fwrite(data_clean,"~/Desktop/R/meetup/bakery-transaction-master/BreadBasket_DMS_clean.csv")

## ggplot theme
theme = theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

### Decrease graph size from standard
options(repr.plot.width = 9, repr.plot.height = 3)


##### 3 - Explortary Data Analysis #####
### Descriptive Statistic with time variable ###
### Transcations by Date/Year
data_clean %>% group_by(Date, Item) %>% summarise(Count=n()) %>% 
  mutate(Year = year(Date)) %>%
  ggplot(aes(x=Date, y=Count, colour=factor(Year))) +
  geom_line() +
  theme(legend.position = "bottom") +
  scale_color_manual(values=c("coral", "cornflowerblue")) +
  labs(y="Transactions", title="Total Transactions by Date", colour="Year") 

### Transcations by Month
data_clean %>% 
  mutate(Month = as.factor(month(Date))) %>%
  group_by(Month, Item) %>% 
  summarise(Count=n()) %>%
  ggplot(aes(x=Month)) +
  geom_histogram(stat="count", fill="cornflowerblue", width=.6) +
  labs(y="Transactions", title="Total Transactions by Month") +
  scale_y_continuous(limits = c(0,5000))

data_clean %>% 
  mutate(Month = as.factor(month(Date))) %>%
  group_by(Month, Transaction) %>% 
  summarise(Count=n()) %>%
  ggplot(aes(x=Month, y=Count, fill=Month)) +
  geom_boxplot() +
  labs(y="Transactions", title="Total Transactions by Month") +
  scale_fill_brewer(type="seq", palette = "Set3") +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0,15)) 
  
### Transcations by Weekday
data_clean %>% mutate(Weekday = weekdays(Date),
                      Weekday = factor(Weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
  group_by(Weekday, Transaction) %>%
  summarise(Count=n()) %>%
  ggplot(aes(x=Weekday, fill=Weekday)) +
  geom_histogram(stat="count") +
  scale_y_continuous(limits=c(0,5000)) +
  labs(y="Transactions", title="Total Transactions by Weekday") +
  scale_fill_brewer(type="seq", palette = "Set3")
  
data_clean %>% mutate(Weekday = weekdays(Date),
                      Weekday = factor(Weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
  group_by(Weekday, Transaction) %>%
  summarise(Count=n()) %>%
  ggplot(aes(x=Weekday, y=Count, fill=Weekday)) +
  geom_boxplot() +
  labs(y="Items per Transaction", title="Items per Transactions by Weekday") +
  scale_y_continuous(limits=c(0,15)) +
  scale_fill_brewer(type="seq", palette = "Set3")

### Graph of transactions per hour
data_hr = data_clean %>% mutate(Hour = as.factor(hour(Time))) %>% 
  group_by(Hour, Transaction) %>% summarise(Count = n())

# set colour for plots
colourCount = length(unique(data_hr$Hour))
getPalette = colorRampPalette(brewer.pal(12, "Set3"))

### How many transactions were happend in each hour
data_hr %>%
  ggplot(aes(x = Hour, fill = Hour)) +
  geom_histogram(stat="count") +
  theme(legend.position="none") +
  labs(x = "Hour", y = "Transactions", title = "Transactions per hour (Unique Transaction ID)") +
  scale_fill_manual(values = getPalette(colourCount)) 

data_hr %>%
  ggplot(aes(x = Hour, y=Count, fill = Hour)) +
  geom_boxplot() +
  theme(legend.position="none") +
  labs(x = "Hour", y = "Transactions", title = "Transactions per hour (Unique Transaction ID)") +
  scale_fill_manual(values = getPalette(colourCount)) 

## Find most popular Items
colourCount = 20
getPalette = colorRampPalette(brewer.pal(12, "Set3"))

data_clean %>% group_by(Item) %>% summarise(Count=n()) %>%
  arrange(desc(Count)) %>% head(20) %>%
  ggplot(aes(x=reorder(Item, Count), y=Count, fill=Item)) +
  geom_bar(stat="identity") +
  coord_flip() +
  theme(legend.position="none") +
  labs(x="Item", y="Counts", title="Top 20 popular items") +
  scale_fill_manual(values = getPalette(colourCount)) 

  
#find top 10 items, and sum the rest into "others"
head(data_item1)
rank_data = arrange(data_item1, desc(Count)) %>% data.frame()
top10 = rank_data[1:10,]
top10[11,"Count"] = sum(rank_data$Count) - sum(top10$Count)
top10[11,"Item"] = "Other"
top10

###bar plot for top 10 items
ggplot(top10, aes(x=reorder(Item, Count), y=Count, fill=Item)) +
  geom_bar(stat="identity") +
  coord_flip() +
  scale_fill_brewer(type="seq", palette = "Set3") +
  theme(legend.position="none") +
  labs(x = "Item", y = "Transaction", title = "Top 10 Transactions") 

###create a blank theme
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

###pie chart for top 10 items
top10$Percent = round(top10$Count/sum(top10$Count),4)*100
top10$Item = factor(top10$Item, c("Coffee","Bread","Tea","Cake","Pastry","Sandwich","Medialuna","Hot chocolate","Cookies","Brownie","Other"))
ggplot(top10, aes(x="", y=Count, fill=Item)) +
  blank_theme +
  geom_bar(width = 1, stat = "identity", colour = "white") +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Set3") +
  geom_text(aes(label = paste0(Percent, "%")),
            position = position_stack(vjust = 0.5), size=4) + #check_overlap = TRUE
  labs(fill="Item", title = "Top 10 Transcations") +
  theme(axis.text.x=element_blank(), 
        plot.title = element_text(hjust=.5, size=15),
        legend.text = element_text(size=10))


##### 4 - Item Combination
### Find which transactions purchased more than two items
data.more = data_clean %>% 
  group_by(Transaction) %>%
  mutate(ItemsPurchased = n(),
         Combined = ifelse(ItemsPurchased>1, TRUE, FALSE)) 
temp = data.more %>%
  filter(Combined=="TRUE") %>%
  group_by(Item, ItemsPurchased) %>%
  summarise(TransactionMoments = n()) %>%
  filter(TransactionMoments>100 & ItemsPurchased>1)  #select only the most popular ones 

colourCount = temp$Item %>% unique() %>% length()
temp %>%    
  ggplot(aes(x=Item, y=TransactionMoments, fill=Item)) +
  geom_bar(stat="identity") +
  facet_grid(ItemsPurchased~.) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5),
        legend.position = "none") +
  labs(title="Popular Item Purchased Combination") +
  scale_fill_manual(values = getPalette(colourCount)) 

###算哪些item都是獨立買的(去除combination有的items)
combined.items = data.more %>% 
  group_by(Item, ItemsPurchased) %>%
  summarise(TransactionMoments = n()) %>%
  filter(TransactionMoments>100 & ItemsPurchased>1)
temp = data.more %>% filter(Combined=="FALSE") %>%
  group_by(Item) %>% summarise(TransactionMoments = n())
temp1 = temp[!(temp$Item %in% unique(combined.items$Item)),] 
colourCount = temp1$Item %>% unique() %>% length()
temp1 %>%
  ggplot(aes(x=reorder(Item, -TransactionMoments), y=TransactionMoments, fill=Item)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5),
        legend.position = "none") +
  labs(x="Item", title="Items Bought Independently") +
  scale_fill_manual(values = getPalette(colourCount))


##### 5 - Find the best item combinations with coffee #####  
coffee_data = data[which(data$Item=="Coffee"),]   
ncoffee_data = data[which(data$Item!="Coffee"),]
###買咖啡的人也同時買了什麼
coffee_transac_list = coffee_data$Transaction
ncoffee_data[ncoffee_data$Transaction %in% coffee_transac_list == TRUE,"Coffee"] = "with"
ncoffee_data[ncoffee_data$Transaction %in% coffee_transac_list == FALSE,"Coffee"] = "without"
buywithcoffee = ncoffee_data %>%
  group_by(Item, Coffee) %>%
  summarise(Count = n())
head(buywithcoffee)
###只看top 10是否有跟coffee一起買的數量  
top10buy = buywithcoffee[buywithcoffee$Item %in% top10$Item == TRUE,] %>% data.frame()
top10buy$Coffee = factor(top10buy$Coffee, levels = c("without", "with"))
top10buy1 = dcast(top10buy, Item~Coffee, value.var = c("Count"))
top10buy1$compare = round(top10buy1$with/top10buy1$without,2)
#top10buy1 = arrange(top10buy1, desc(-compare))
###top10buy2 is for text use 
top10buy2 = top10buy %>% group_by(Item) %>% summarise(Sum=sum(Count)) %>% arrange(desc(Sum))
top10buy2$compare = NULL
for(i in 1:nrow(top10buy2)){
  for(j in 1:nrow(top10buy1)){
    if (top10buy2$Item[i] == top10buy1$Item[j]) top10buy2$compare[i] = top10buy1$compare[j]
  }
}

ggplot(top10buy, aes(x=reorder(Item, -Count), y=Count, fill=Coffee)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(labels=c("with" = "With coffee", "without" = "Without coffee"),
                    values=c("with"="lightskyblue2", "without" = "cornflowerblue")) +
  coord_flip() +
  labs(x="Item", y="Count", fill="", title="Sales Information of Top 10 Items With/Without Coffee") +
  theme(legend.position = "bottom") +
  geom_text(aes(label = paste0(Count)),
            position = position_stack(vjust = 0.5), size=3, colour="white") +
  scale_x_discrete(labels = paste0(top10buy2$Item," ", top10buy2$compare, "%")) +
  guides(fill = guide_legend(reverse = TRUE))


##### 6 - Item Association Rules (Market Basket Analysis) #####
head(data)
transaction.data <- read.transactions("~/Desktop/R/meetup/bakery-transaction-master/BreadBasket_DMS_Clean.csv", # file path
  format = "single", # single is used because each column has 1 item
  cols = c(3,4), # specifies the item and transaction columns
  sep = ",")

itemFrequencyPlot(transaction.data,
  topN=4, # Show top 4 items
  type="absolute", # absolute frequency instead of relative
  main="Most popular items sold (absolute frequency)")

association.rules = apriori(transaction.data,
                            appearance = list(default="lhs",rhs="Coffee"),
                            parameter = list(supp = 0.005, # Minimum support level, frequency of items
                                             conf = 0.6 # Minimum confidence level
                            )
)

association.rules = sort(association.rules, by = 'support', decreasing = TRUE)
summary(association.rules)
#We arrive at 6 rules, shown as below:
inspect(head(association.rules))
#lift: the confidence value of the rules (confidence/expected confidence)
#if lift > 1, has positive effects on the rules
#People who bought Toast and Coffee together represent 2.5% of the transactions.
#We have 72.9% certainty that if a person buys toast, he/she will also buy a coffee.

plot(association.rules)
plot(association.rules, method="graph")
#the red colour is based on lift, the redder the greater the lift value (more significant)





