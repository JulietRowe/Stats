data.means = NA
data.sds = NA

for(counter in 1:10)
{
  data = read.table(sprintf("subject%d.txt", counter), sep = " ")
  data.means[counter] = mean(data$V1)
  data.sds[counter] = sd(data$V1)
}

data.means[counter] = mean(data$V1)
data.sds[counter] = sd(data$V1)
data.ranges[counter] = range(data$V1)
hist(data$V1)
shapiro.test(data$V1)
ks.test(data, pnorm, mean(data$V1), sd(data$V1))