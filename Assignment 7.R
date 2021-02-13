#Question 1
data = read.table("assignmentdata1.txt")

analysis = t.test(data$V1, mu = 2900)
print(analysis)
#Our analysis showed that the sample mean was equal to the population mean, t(49) = 1.4161, p = 0.1631

mean = mean(data$V1)

sd = sd(data$V1)

cis = abs(qt(0.05, 49)*sd/sqrt(50))

plot_data = data.frame(
  Values = data$V1,
  Group = c('sample 1')
)

library(ggplot2)

ggplot(aes(x = Group, y= Values), data = plot_data)+
  stat_summary(fun = mean, geom = 'bar')+
  stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', width = 0.25)+
  scale_y_continuous(expand = c(0,0))+
  theme_classic()

#Question 2
data2 = read.table("assignmentdata2.txt")
names(data2) = c('subject', 'time', 'calories')

data2$time = factor(data2$time)
analysis2 = t.test(data2$calories~data2$time, paired = TRUE)
print(analysis2)
#Our analysis revealed that the caloric intake was not different between two times, t(49) = 1.447, p = 0.1543

means = aggregate(data2$calories, list(data2$time), mean)
means = means$x

sds = aggregate(data2$calories, list(data2$time), sd)
sds = sds$x

cis_time1 = abs(qt(0.05, 49)*sds[1]/sqrt(50))
cis_time2 = abs(qt(0.05, 49)*sds[2]/sqrt(50))

dev.new()
ggplot(aes(x = time, y= calories), data = data2)+
  stat_summary(fun = mean, geom = 'bar')+
  stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', width = 0.25)+
  scale_y_continuous(expand = c(0,0))+
  theme_classic()

data2$difference = data2$calories[data2$time == 1] - data2$calories[data2$time == 2]

plot_data2 = data.frame(
  Values = data2$difference[0:50],
  Group = c('Difference')
)

mean_difference = mean(plot_data2$Values)
sd_difference = sd(plot_data2$Values)
cis_difference = abs(qt(0.05, 49)*sd_difference/sqrt(50))

dev.new()
ggplot(aes(x = Group, y= Values), data = plot_data2)+
  stat_summary(fun = mean, geom = 'bar')+
  stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', width = 0.5)+
  scale_y_continuous(expand = c(0,0))+
  theme_classic()

#Question 3
data3 = read.table("assignmentdata3.txt")
names(data3) = c('subject', 'group', 'calories')

data$group = factor(data$group)
analysis3 = t.test(data3$calories~data3$group)
print(analysis3)
#Our analysis revealed that the caloric intake was different between the two groups, t(49) = 2.587, p < 0.05

means3 = aggregate(data3$calories, list(data3$group), mean)
means3 = means3$x

sds3 = aggregate(data3$calories, list(data3$group), sd)
sds3 = sds3$x

cis_group1 = abs(qt(0.05, 49)*sds3[1]/sqrt(50))
cis_group2 = abs(qt(0.05, 49)*sds3[2]/sqrt(50))

dev.new()
ggplot(aes(x = group, y= calories), data = data3)+
  stat_summary(fun = mean, geom = 'bar')+
  stat_summary(fun.data = mean_cl_normal, geom = 'errorbar', width = 0.25)+
  scale_y_continuous(expand = c(0,0))+
  theme_classic()