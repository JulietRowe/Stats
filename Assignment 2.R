#2a.Mean, median, mode
setwd("C:/Users/julie/GitHub/Stats")
mydata = read.table("data.txt")
names(mydata) = c("group", "eeg", 'rt')

mean(mydata$rt[mydata$group == 1])
mean(mydata$rt[mydata$group == 2])
median(mydata$rt[mydata$group == 1])
median(mydata$rt[mydata$group == 2])

mean(mydata$eeg[mydata$group == 1])
mean(mydata$eeg[mydata$group == 2])
median(mydata$eeg[mydata$group == 1])
median(mydata$eeg[mydata$group == 2])

par(mfrow = c(1,2))
plot(mydata$rt)
abline(a = mean(mydata$rt), b = 0, col = "red")
abline(a = median(mydata$rt), b = 0, col = "blue")

plot(mydata$eeg)
abline(a = mean(mydata$eeg), b = 0, col = "red")
abline(a = median(mydata$eeg), b = 0, col = "blue")

#2b. Variance
#Question 1
data1 <- rnorm(1000, 500, 10)
data2 <- rnorm(1000, 500, 50)
data3 <- rnorm(1000, 500, 100)

par(mfrow = c(1,3))
plot(data1, ylim = c(0, 1000))
plot(data2, ylim = c(0, 1000))
plot(data3, ylim = c(0, 1000))

var(data1)
var(data2)
var(data3)

#Question 2
squared_error <- data.frame(data1, data2, data3)
SE1 <- squared_error$data1 - mean(squared_error$data1)
squared_error$SE1 <- SE1 * SE1

SE2 <- squared_error$data2 - mean(squared_error$data2)
squared_error$SE2 <- SE2 * SE2

SE3 <- squared_error$data3 - mean(squared_error$data3)
squared_error$SE3 <- SE3 * SE3

SSE1 = sum(squared_error$SE1)
SSE2 = sum(squared_error$SE2)
SSE3 = sum(squared_error$SE3)
var1 = var(data1)
var2 = var(data2)
var3 = var(data3)

#Question 3
par(mfrow = c(1,3))
hist(data1, xlim = c(0, 1000), ylim = c(1,300))
hist(data2, xlim = c(0, 1000), ylim = c(1,300))
hist(data3, xlim = c(0, 1000), ylim = c(1,300))

output = hist(data1, xlim = c(0, 1000), ylim = c(1,300))
output

#2c. Confidence Intervals
weightdata = read.table("weightdata.txt")
names(weightdata) = c("time", "weight")

df = length(weightdata$weight[weightdata$time == 1])
crt = qt(0.05, df)
std = sd(weightdata$weight[weightdata$time == 1])
sqn = sqrt(length(weightdata$weight[weightdata$time == 1]))
ci = abs(crt*std/sqn)
lower_ci = mean(weightdata$weight[weightdata$time == 1]) - ci
upper_ci = mean(weightdata$weight[weightdata$time == 1]) + ci

df2 = length(weightdata$weight[weightdata$time == 2])
crt2 = qt(0.05, df)
std2 = sd(weightdata$weight[weightdata$time == 2])
sqn2 = sqrt(length(weightdata$weight[weightdata$time == 2]))
ci2 = abs(crt*std/sqn)
lower_ci2 = mean(weightdata$weight[weightdata$time == 2]) - ci
upper_ci2 = mean(weightdata$weight[weightdata$time == 2]) + ci

weightdata$diff = weightdata$weight[weightdata$time == 2] - weightdata$weight[weightdata$time == 1]
df3 = length(weightdata$diff)
crt3 = qt(0.05, df)
std3 = sd(weightdata$diff)
sqn3 = sqrt(length(weightdata$diff))
ci3 = abs(crt*std/sqn)
lower_ci3 = mean(weightdata$diff) - ci
upper_ci3 = mean(weightdata$diff) + ci

#Challenge question
barplot(mean(weightdata$diff))



