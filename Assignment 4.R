setwd("C:/Users/julie/GitHub/Stats")
data = read.table("mrdata2.txt")

#4a. Correlations
cor.test(data$V1, data$V2)
plot(data$V1, data$V2)

cor.test(data$V1, data$V3)
plot(data$V1, data$V3)

cor.test(data$V1, data$V4)
plot(data$V1, data$V4)

cor.test(data$V1, data$V5)
plot(data$V1, data$V5)

cor.test(data$V1, data$V6)
plot(data$V1, data$V6)

#4b. Regressions
model = lm(data$V1~data$V2)
summary(model)
plot(data$V2~data$V1)
abline(lm(data$V2~data$V1))

model = lm(data$V1~data$V3)
summary(model)
plot(data$V3~data$V1)
abline(lm(data$V3~data$V1))

model = lm(data$V1~data$V4)
summary(model)
plot(data$V4~data$V1)
abline(lm(data$V4~data$V1))

model = lm(data$V1~data$V5)
summary(model)
plot(data$V5~data$V1)
abline(lm(data$V5~data$V1))

model = lm(data$V1~data$V6)
summary(model)
plot(data$V6~data$V1)
abline(lm(data$V6~data$V1))
