data = read.table("exam_question1.txt")
names(data) = c('subject', 'group', 'RT')

data$group = factor(data$group)

#Testing Assumptions 
#The groups are independent
#We can assume that the members of each group are randomly sampled and independent of each other

#Normality
for(counter in 1:3)
{
  analysis = shapiro.test(data$RT[data$group == counter])
  print(analysis)
}

#All p-values > 0.05. Assumption of normality is met

#Homogeneity
bartlett.test(data$RT~data$group)
#p < 0.05 so the assumption of homogeneity is not met. 
#However, the ANOVA is consider a "robust" test that can still be run with this assumption not met
#And dealing with the violation of this assumption is "beyond the scope of this course"

#ANOVA
analysis = aov(data$RT~data$group)
summary(analysis)

#F(2,57) = 4.731, p < 0.05
#There is a significant difference between the groups 

#Post Hoc
TukeyHSD(analysis)
#Reaction times differ between the young and the old (p < 0.05) 
# and between the middle and the old age groups (p < 0.05)

