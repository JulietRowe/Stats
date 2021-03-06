data = read.table("rmassignmentdata.txt")
names(data) = c("subject", "condition", "data")

#Testing Assumptions
data$subject = factor(data$subject)
data$condition = factor(data$condition)

par(mfcol = c(2,3))
hist(data$data[data$condition == 1])
hist(data$data[data$condition == 2])
hist(data$data[data$condition == 3])
hist(data$data[data$condition == 4])
hist(data$data[data$condition == 5])
hist(data$data[data$condition == 6])

qqnorm(data$data[data$condition == 1])
qqline(data$data[data$condition == 1])
qqnorm(data$data[data$condition == 2])
qqline(data$data[data$condition == 2])
qqnorm(data$data[data$condition == 3])
qqline(data$data[data$condition == 3])
qqnorm(data$data[data$condition == 4])
qqline(data$data[data$condition == 4])
qqnorm(data$data[data$condition == 5])
qqline(data$data[data$condition == 5])
qqnorm(data$data[data$condition == 6])
qqline(data$data[data$condition == 6])
#Data is normally distributed 
#How do include participants when testing anova assumptions?
shapiro.test(data$data[data$condition == 1])
shapiro.test(data$data[data$condition == 2])
shapiro.test(data$data[data$condition == 3])
shapiro.test(data$data[data$condition == 4])
shapiro.test(data$data[data$condition == 5])
shapiro.test(data$data[data$condition == 6])

bartlett.test(data$data~data$condition)
#Assumption of homogeneity of variances is met p = 0.8395

#Repeated measures ANOVA
model = aov(data$data~data$condition + Error(data$subject/data$condition))
#removing between participant variance from the overall error variance
summary(model)
#There exists a significant difference among the conditions but we do not know if the 
#assumption of sphericity is met and whether or not we need a correct p value

#Assumption of sphericity and repeated ANOVA
library("ez")
model = ezANOVA(data=data, dv = .(data), wid = .(subject), within = .(condition), detailed = TRUE, type = 3)
model
#Assumption of sphericity is met p > 0.05
#There does exist a statistical difference p < 0.05

#Post-Hoc Analysis of RM ANOVA
pairwise.t.test(data$data, data$condition, p.adjust = 'bonf', paired = TRUE)
#There is a trend that is significant, but it isn't between any two conditions
#If you run linear regression you would find it to be significant
