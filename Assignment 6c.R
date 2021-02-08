data.means = NA
data.sds = NA
data.SW_normality = NA
data.KS_normality = NA

for(counter in 1:10)
{
  data = read.table(sprintf("subject%d.txt", counter), sep = " ")
  data = stack(data)
  data.means[counter] = mean(data$values)
  data.sds[counter] = sd(data$values)
  print(range(data$values))
  dev.new()
  hist(data$values, main = counter)
  data.SW_normality[counter] = shapiro.test(data$values)$p.value
  data.KS_normality[counter] = ks.test(data$values, pnorm, mean(data$values), sd(data$values))$p.value
}

#Assumption of normality is the assumption that a a sample's means are distributed normally. 


