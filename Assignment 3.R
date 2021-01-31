setwd("C:/Users/julie/GitHub/Stats")
data = read.table("data.txt")
names(data) = c("group", "eeg", "rt")

#3a Plotting Basics
means = aggregate(rt~group, data, mean)

par(mfrow = c(1,2))
plot(data$rt[data$group == 1], main = "Group 1 Reaction Times", xlab = "Group 1", ylab = "Reaction Time (ms)", ylim = c(150, 500))
abline(a = means$rt[1], b = 0, col = "red")
plot(data$rt[data$group == 2], main = "Group 2 Reaction Times", xlab = "Group 2", ylab = "Reaction Time (ms)", ylim = c(150, 500))
abline(a = means$rt[2], b = 0, col = "red")

#3b Bar plots
data = read.table("sampleplotdata.txt")
names(data) = c("subject", "group", "rt")
means = aggregate(data$rt, list(data$group), mean)
means = means$x

sds = aggregate(data$rt, list(data$group), sd)
sds = sds$x
 
cis = abs(qt(0.05, 49)* sds/sqrt(50))
bp = barplot(means)
arrows(bp, means-cis, bp, means+cis, angle = 90, code = 3, length = 0.5)

#3c Box plots
boxplot(data$rt~data$group)
group1 = data$rt[data$group == 1]
median(group1)
min(group1)
max(group1)
stuff = boxplot(data$rt~data$group)
stuff

#3D Histograms 
hist(data$rt)
hist(data$rt[data$group ==1], col = rgb(1,0,0,0.5), ylim = c(0,25))
hist(data$rt[data$group == 3], col = rgb(0,0,1,0.5), ylim = c(0,25), add = T)
stuff = hist(data$rt)
stuff
