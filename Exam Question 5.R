data = read.table("exam_question5.txt")

plot(data)
cor(data$V1, data$V2)
cor.test(data$V1, data$V2) #formal statistical test against the null hypothesis

model = lm(data$V1~data$V2)
summary(model)
#The model that fits p < 0.05. Gives B0 and B1 to determine the regression equation
plot(data$V2~data$V1)
abline(lm(data$V2~data$V1))