# Loading Data
library("plyr")
setwd("C:/Users/julie/GitHub/Stats")
mydata = read.table("data.txt")
View(mydata)
mydata$V3
mydata$V3[2]

# Data Tables 
names(mydata) = c("group", "eeg", "rt")
mydata$rt[mydata$group == 2]
mydata$group = factor(mydata$group)

t.test(mydata$rt~mydata$group)

