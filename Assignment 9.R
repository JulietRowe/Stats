#Wilcox Test
data = read.table("ttestdata.txt")

library(car)
leveneTest(data$V3~factor(data$V2)) #checking for Homogeneity of Variance
#Test is significant, thus indicating a violation of the assumption 

t.test(data$V3~factor(data$V2))
#Test is significant, but is inappropriate due to the failure of assumption

wilcox.test(data$V3~factor(data$V2))

#Wilcoxon Signed Rank Test
data = read.table("ttestdata2.txt")

data1 = data$V3[data$V2 == 1]
data2 = data$V3[data$V2 == 2]
differences = data2 - data1
hist(differences)

#The resulting histogram looks normal, so can use a dependent samples ttest
t.test(data$V3~factor(data$V2), paired = TRUE)

#But, if you were concerned about a violation of the assumption you could use the Wilcoxon Rank test
wilcox.test(data$V3~factor(data$V2), paired = TRUE)

#Multiple Groups
data = read.table("anovadata.txt")

group = factor(data$V2)
dv = data$V3
boxplot(dv~group)

library(car)
leveneTest(dv~group)
#Definitely non-parametric

#Non-parametric ANOVA
kruskal.test(dv~group)

#Non-parametric Post-Hoc
library(pgirmess)
kruskalmc(dv~group)#Shows that all the groups are difference (significance = TRUE)

#Non Parametric Equivalent of RM ANOVA
friedman.test(as.matrix(data))
#Post Hoc
friedmanmc(as.matrix(data))
