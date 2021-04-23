data = read.table("exam_question4.txt")
names(data) = c('Participant', 'group', 'EEG')

#Testing Normality
shapiro.test(data$EEG)
#P < 0.05. This data is not normally distributed. 

wilcox.test(data$EEG, mu = 0)
#The one sample Wilcoxon signed rank test is used for non parametric data
#V = 146, p > 0.05

df = length(data$EEG) - 1
n = length(data$EEG)
mean = mean(data$EEG)
sd = sd(data$EEG)
CI = abs(qt(0.05, df)*sd/sqrt(n))
LL_CI = mean - CI
UL_CI = mean + CI
#The 95% CI does not include the population mean 95% CI = [0.108, 2.273]

library(BEST)
results = BESTmcmc(data$EEG)
plot(results)




