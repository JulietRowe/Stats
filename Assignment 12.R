my.data = read.table('mixedANOVAData.txt')
names(my.data) = c('Subject', 'Group', 'Time', 'RT')

my.data$Subject = factor(my.data$Subject)
my.data$Group = factor(my.data$Group)
my.data$Time = factor(my.data$Time)
myMixedANOVA = aov(RT~Group + Time + Group*Time + Error(Subject/Time), data = my.data)
#Group is not included in the error term so it is treated as 
#Between subjects factor
#Whereas, time is included in the error term so it is treated as a within subjects factor
print(summary(myMixedANOVA))
#Top portion is the result for the between subjects analysis of Group
#The bottom portions are the results for the within subjects analysis of Time and the Group by Time interaction
boxplot(my.data$RT~my.data$Time*my.data$Group)

#PostHoc
pairwise.t.test(my.data$RT, my.data$Time, p.adjust = "bonf")
pairwise.t.test(my.data$RT, my.data$Group, p.adjust = "bonf")