data = read.table("exam_question1.txt")
names(data) = c('subject', 'group', 'RT')

means = aggregate(data$RT, list(data$group), mean)
means = means$x

sds = aggregate(data$RT, list(data$group), sd)
sds = sds$x

cis = abs(qt(0.05, 19)*sds/sqrt(20))
bp = barplot(means, ylim = c(0,310), xlab = 'Age groups', ylab = 'Reaction Time (ms)', main = 'Reaction time for young (left), middle, and old (right) age groups')
arrows(bp, means-cis, bp, means+cis, angle = 90, code = 3, length = 0.25)