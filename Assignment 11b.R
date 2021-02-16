my.data = read.table('samplermdata.txt')
names(my.data) = c('Subject', 'Week', 'RT')
my.data$Subject = factor(my.data$Subject)
my.data$Week = factor(my.data$Week)
plot(my.data$RT~my.data$Week)

library('emmeans')
myRMANOVA = aov(RT~Week + Error(Subject/Week), data = my.data)
print(summary(myRMANOVA))
weekContrast = emmeans(myRMANOVA, "Week")
poly = contrast(weekContrast, 'poly')
print(poly)

#Significant repeated measures ANOVA
#Significant linear and quadratic trends