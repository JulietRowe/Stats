data = read.table("sampleanovadata2.txt")
names(data) = c('subject', 'group', 'rt')

data$group = factor(data$group)

#Testing Assumptions 
par(mfcol = c(1,4))
hist(data$rt[data$group == 1])
hist(data$rt[data$group == 2])
hist(data$rt[data$group == 3])
hist(data$rt[data$group == 4])

library("moments")
skewness(data$rt[data$group ==1])
kurtosis(data$rt[data$group == 1])
skewness(data$rt[data$group == 2])
kurtosis(data$rt[data$group == 2])
skewness(data$rt[data$group == 3])
kurtosis(data$rt[data$group == 3])
skewness(data$rt[data$group == 4])
kurtosis(data$rt[data$group == 4])

qqnorm(data$rt[data$group == 1])
qqline(data$rt[data$group == 1])
qqnorm(data$rt[data$group == 2])
qqline(data$rt[data$group == 2])
qqnorm(data$rt[data$group == 3])
qqline(data$rt[data$group == 3])
qqnorm(data$rt[data$group == 4])
qqline(data$rt[data$group == 4])

var(data$rt[data$group == 1])
var(data$rt[data$group == 2])
var(data$rt[data$group == 3])
var(data$rt[data$group == 4])

bartlett.test(data$rt~data$group)

library(car)
leveneTest(data$rt~data$group) 

#Visualizing Data
boxplot(data$rt~data$group)

#ANOVA
analysis = aov(data$rt~data$group)
summary(analysis)

#F(3,196) = 14.27, p < 0.001

library("gplots")
plotmeans(data$rt~data$group)

#Post Hoc Analysis
TukeyHSD(analysis)

pairwise.t.test(data$rt, data$group, p.adjust = "bonf")

library('agricolae')
out = LSD.test(data$rt, data$group, 196, 3586, p.adj = "none", console = TRUE)
#196 and 3586 represent the degrees of freedom and mean squared error of the residuals in the ANOVA model

summary.lm(analysis) #Contrasts of groups 2,3, and 4 relative to group 1

contrasts(data$group) = contr.treatment(4, base = 4)
modelnew = aov(data$rt~data$group)
summary.lm(modelnew)
#Above conducts a contrast comparing the groups to the 4th group

contrasts(data$group) = contr.poly(4)
modelnew = aov(data$rt~data$group)
summary.lm(modelnew)
#Above conducts a contrast examining the polynomial trends between groups (linear, quadratic)

#Based on the ANOVA we know that there is a significant difference among the sample means
#F(3,196) = 14.27, p < 0.001
#Based on the post-hoc analysis we know that the differences exist between groups 1 and 3, 1 and 4, 2 and 3, and 2 and 4. 
#Thus, 1 and 2 are similar and 3 and 4 are similar, but (1 & 2) differ from (3 & 4)