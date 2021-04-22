data = read.table("exam_question3.txt")
names(data) = c('subject', 'time', 'calories')

data$time = factor(data$time)

#Testing assumptions
#Normality
shapiro.test(data$calories[data$time == 1])
shapiro.test(data$calories[data$time == 2])
#P-values > 0.05 for both time measures. Assumption of normality is met

#Homogeneity
bartlett.test(data$calories~data$time)
#p < 0.05 so the assumption of homogeneity is not met.
#Violating this assumption appears to matter if you have unequal group sizes. 
#But since the groups sizes are the same, this test result can be ignored

analysis = t.test(data$calories~data$time, paired = TRUE)
print(analysis)
#Our analysis reveal that the caloric intake was different between the two times, t(19) = 12.793, p < 0.001

means = aggregate(data$calories, list(data$time), mean)
means = means$x

sds = aggregate(data$calories, list(data$time), sd)
sds = sds$x

cis = abs(qt(0.05, 19)*sds/sqrt(20))

bp = barplot(means, ylim = c(0,2500), xlab = 'Time (days)', ylab = 'Caloric intake (kcal)', main = 'Mean caloric intake for day 1 (left) and day 2 (right) Reaction time for young (left)')
arrows(bp, means-cis, bp, means+cis, angle = 90, code = 3, length = 0.50)

library(ggplot2)
ggplot(aes(x = time, y= calories), data = data)+
  stat_summary(fun = mean, geom = 'bar')+
  stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', width = 0.5)+
  scale_y_continuous(expand = c(0,0))+
  theme_classic()