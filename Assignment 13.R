data = read.table("data1.txt")

diffscores = data$V2 - data$V1
mean = mean(diffscores)
sd = sd(diffscores)
CI = abs(qt(0.05, 19)*sd/sqrt(20))
LL_CI = mean - CI
UL_CI = mean + CI
#Bayesion equivalent of a single sample t-test
library(BEST)
results1 = BESTmcmc(diffscores)
plot(results1)

#Bayesian equivalent of an independent samples t-test
results2 = BESTmcmc(data$V1, data$V2)
plot(results2)
#Because zero is not included in the Bayesian Credible Interval
#For the mean difference scores
#You would infer that these two groups are from different populations

#Question 20
data2 = read.table('data3.txt')
results3 = BESTmcmc(data2$V1, data2$V2)
plot(results3)
#The groups do not differ as zero is included within the 
#Bayesian Credible Interval range
