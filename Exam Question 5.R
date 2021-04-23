data = read.table("exam_question5.txt")

plot(data)
cor(data$V1, data$V2)
cor.test(data$V1, data$V2) 
#r(98) = 0.20, p < 0.05

model = lm(data$V2~data$V1)
summary(model)
plot(data$V2~data$V1)
abline(lm(data$V2~data$V1))


#F(98) = 4.125, p < 0.05
# y = 0.007906x + 7.395410