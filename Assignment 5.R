data = read.table("mrassignmentdata.txt")

y = data$V1
x1 = data$V2
x2 = data$V3
x3 = data$V4
x4 = data$V5
x5 = data$V6

results = lm(y~x1+x2+x3+x4+x5)
summary(results)

library(car)
durbinWatsonTest(results)

vif(results)
mean(vif(results))
1/vif(results)

res = resid(results)
hist(res)

plot(results)

cooks = cooks.distance(results)
plot(cooks)