data = read.table("exam_question3.txt")
names(data) = c('subject', 'time', 'calories')

data$time = factor(data$time)

#Testing assumptions
#The response variables are continuous
#Normality
difscores = data$calories[data$time == 1] - data$calories[data$time == 2]
shapiro.test(difscores)
#Assumption that the difference between the two related groups is normally distributed is met, p > 0.05
boxplot(diffscores)
#No outliers

analysis = t.test(data$calories~data$time, paired = TRUE)
print(analysis)
#Our analysis reveal that the caloric intake was different between the two times, t(19) = 12.793, p < 0.001

#Plot of individual means and confidence intervals
n = 20
df = n - 1
means = aggregate(data$calories, list(data$time), mean)
means = means$x

sds = aggregate(data$calories, list(data$time), sd)
sds = sds$x

cis = abs(qt(0.05, df)*sds/sqrt(n))

dev.new()
bp = barplot(means, ylim = c(0,2500), xlab = 'Time', ylab = 'Caloric intake (kcal)', main = 'Mean caloric intake for start (left) and end (right) of intervention')
arrows(bp, means-cis, bp, means+cis, angle = 90, code = 3, length = 0.50)

#Plot of the difference scores
dev.new()
mean = mean(difscores)
sd = sd(difscores)
ci = abs(qt(0.05, 19)*sd/sqrt(20))
bp = barplot(mean, ylim = c(0,120), xlab = 'Start - End', ylab = 'Caloric intake difference', main = 'Mean difference caloric intake')
arrows(bp, mean-ci, bp, mean+ci, angle = 90, code = 3, length = 0.50)


