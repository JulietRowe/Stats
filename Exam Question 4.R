data = read.table("exam_question4.txt")
names(data) = c('Participant', 'group', 'EEG')

analysis = t.test(data$EEG, mu = 0)
print(analysis)

mean = mean(data$EEG)
sd = sd(data$EEG)
CI = abs(qt(0.05, 19)*sd/sqrt(20))
LL_CI = mean - CI
UL_CI = mean + CI

library(BEST)
results = BESTmcmc(data$EEG)
plot(results)



