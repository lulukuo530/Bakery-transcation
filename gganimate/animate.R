rm(list=ls())
#devtools::install_github('thomasp85/gganimate')
library(data.table)
library(dplyr)
library(reshape2)
library(Hmisc)
library(lubridate)
library(ggplot2)
library(gganimate)

bread = fread("~/Desktop/R/R-Ladies/bakery-transaction-master/BreadBasket_DMS_clean.csv",sep=",",stringsAsFactors=F)
head(bread)
str(bread) #glimpse(bread)
describe(bread)
bread = bread %>%
  mutate(
    Date = as.Date(Date),
    Time = as.ITime(Time)
    )

##### Line Chart #####
#Count how many items were sold on each day
#data.frame(summarise(group_by(bread, Date, Item), Count = n()))
item.sold = bread %>%
  group_by(Date) %>%
  summarise(Count = n()) %>%
  data.frame()

item.sold = item.sold %>%
  mutate(
    Year = year(Date),
    Month = month(Date)
  )
item.sold$Category = ifelse(item.sold$Count>145, "Above Half", "Below Half")
table(item.sold$Category)
head(item.sold)

p2 = ggplot(item.sold, aes(x = Date, y = Count)) +
  geom_line(aes(linetype=factor(Year), colour = Category)) + 
  scale_color_manual(values=c("coral", "cornflowerblue")) +
  scale_linetype_manual(values=c("solid","longdash")) +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  labs(linetype="Year")
p2
item.sold$Year = as.integer(item.sold$Year)

#save animate plot
setwd("~/Desktop/R/Animate")
p3 = p2 + transition_reveal(Date)
anim_save("Date.gif", animation = p3)


##### Bar Chart #####
bb = bread %>%
  mutate(
    Month = month(Time),
    Day = day(Time),
    Hour = hour(Time)
  ) 

itemdata = summarise(group_by(bb, Item), Count=n()) %>% data.frame()
items = arrange(itemdata, desc(Count))[1:10,"Item"]
top10 = subset(bb, bb$Item %in% items)
top10 = summarise(group_by(top10, Hour, Item), Count=n()) %>% data.frame()
top10$Hour = as.factor(top10$Hour)
staticplot = ggplot(top10, aes(x=reorder(Item, Count), y=Count, fill=Item)) +
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_brewer(type="seq", palette="Set3") +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x="Item")

#save animate plot
ss = staticplot + transition_states(Hour) +
  labs(title = "Hour: {closest_state}")
anim_save("bar chart Hour.gif", animation = ss)  

  
  

