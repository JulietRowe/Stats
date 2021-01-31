sample1.data = NA
sample1.means = NA
sample2.data = NA
sample2.means = NA
sample3.data = NA
sample3.means = NA
sample4.data = NA
sample4.means = NA
sample5.data = NA
sample5.means = NA
sample6.data = NA
sample6.means = NA

for(counter in 1:100)
{
  sample1.data = rnorm(5,300,25) 
  sample1.means[counter] = mean(sample1.data)
  sample2.data = rnorm(10,300,25) 
  sample2.means[counter] = mean(sample2.data) 
  sample3.data = rnorm(50,300,25) 
  sample3.means[counter] = mean(sample3.data) 
  sample4.data = rnorm(100,300,25) 
  sample4.means[counter] = mean(sample4.data) 
  sample5.data = rnorm(1000,300,25) 
  sample5.means[counter] = mean(sample5.data) 
  sample6.data = rnorm(10000,300,25) 
  sample6.means[counter] = mean(sample6.data) 
}

par(mfrow = c(2,3))
plot(sample1.means, xlab = "Samples", ylab = "Mean", main = "n = 5", ylim = c(250, 350))
plot(sample2.means, xlab = "Samples", ylab = "Mean", main = "n = 10", ylim = c(250, 350))
plot(sample3.means, xlab = "Samples", ylab = "Mean", main = "n = 50", ylim = c(250, 350))
plot(sample4.means, xlab = "Samples", ylab = "Mean", main = "n = 100", ylim = c(250, 350))
plot(sample5.means, xlab = "Samples", ylab = "Mean", main = "n = 1000", ylim = c(250, 350))
plot(sample6.means, xlab = "Samples", ylab = "Mean", main = "n = 10000", ylim = c(250, 350))

variance = NA
variance[1] = var(sample1.means)
variance[2] = var(sample2.means)
variance[3] = var(sample3.means)
variance[4] = var(sample4.means)
variance[5] = var(sample5.means)
variance[6] = var(sample6.means)

sample_size <- c('5', '10', '50', '100', '1000', '10000')
data <- data.frame(sample_size, variance)

dev.new()
plot(data)

