#Normal Distributions
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
sample7.data = NA
sample7.means = NA
sample8.data = NA
sample8.means = NA
sample9.data = NA
sample9.means = NA
sample10.data = NA
sample10.means = NA

for(counter in 1:100)
{
  sample1.data = rnorm(10,500,100) 
  sample1.means[counter] = mean(sample1.data)
  sample2.data = rnorm(20,500,100) 
  sample2.means[counter] = mean(sample2.data) 
  sample3.data = rnorm(30,500,100) 
  sample3.means[counter] = mean(sample3.data) 
  sample4.data = rnorm(50,500,100) 
  sample4.means[counter] = mean(sample4.data) 
  sample5.data = rnorm(100,500,100) 
  sample5.means[counter] = mean(sample5.data) 
  sample6.data = rnorm(1000,500,100) 
  sample6.means[counter] = mean(sample6.data)
  sample7.data = rnorm(2500,500,100) 
  sample7.means[counter] = mean(sample7.data) 
  sample8.data = rnorm(5000,500,100) 
  sample8.means[counter] = mean(sample8.data) 
  sample9.data = rnorm(10000,500,100) 
  sample9.means[counter] = mean(sample9.data) 
  sample10.data = rnorm(100000,500,100) 
  sample10.means[counter] = mean(sample10.data) 
}

par(mfrow = c(5,2))
hist(sample1.means, main = "n = 10", xlim = c(400,600), ylim = c(0,40))
hist(sample2.means, main = "n = 20", xlim = c(400,600), ylim = c(0,40))
hist(sample3.means, main = "n = 30", xlim = c(400,600), ylim = c(0,40))
hist(sample4.means, main = "n = 50", xlim = c(400,600), ylim = c(0,40))
hist(sample5.means, main = "n = 100", xlim = c(400,600), ylim = c(0,40))
hist(sample6.means, main = "n = 1000", xlim = c(400,600), ylim = c(0,40))
hist(sample7.means, main = "n = 2500", xlim = c(400,600), ylim = c(0,40))
hist(sample8.means, main = "n = 5000", xlim = c(400,600), ylim = c(0,40))
hist(sample9.means, main = "n = 10000", xlim = c(400,600), ylim = c(0,40))
hist(sample10.means, main = "n = 100000", xlim = c(400,600), ylim = c(0,40))

#Random Distributions
sample11.data = NA
sample11.means = NA
sample12.data = NA
sample12.means = NA
sample13.data = NA
sample13.means = NA
sample14.data = NA
sample14.means = NA
sample15.data = NA
sample15.means = NA
sample16.data = NA
sample16.means = NA
sample17.data = NA
sample17.means = NA
sample18.data = NA
sample18.means = NA
sample19.data = NA
sample19.means = NA
sample20.data = NA
sample20.means = NA

for(counter in 1:100)
{
  sample11.data = runif(10,1,1000) 
  sample11.means[counter] = mean(sample11.data)
  sample12.data = runif(20,1,1000) 
  sample12.means[counter] = mean(sample12.data) 
  sample13.data = runif(30,1,1000) 
  sample13.means[counter] = mean(sample13.data) 
  sample14.data = runif(50,1,1000) 
  sample14.means[counter] = mean(sample14.data) 
  sample15.data = runif(100,1,1000) 
  sample15.means[counter] = mean(sample15.data) 
  sample16.data = runif(1000,1,1000) 
  sample16.means[counter] = mean(sample16.data)
  sample17.data = runif(2500,1,1000) 
  sample17.means[counter] = mean(sample17.data) 
  sample18.data = runif(5000,1,1000) 
  sample18.means[counter] = mean(sample18.data) 
  sample19.data = runif(10000,1,1000) 
  sample19.means[counter] = mean(sample19.data) 
  sample20.data = runif(100000,1,1000) 
  sample20.means[counter] = mean(sample20.data) 
}

par(mfrow = c(5,2))
hist(sample11.means, main = "n = 10", xlim = c(200,850), ylim = c(0,40))
hist(sample12.means, main = "n = 20", xlim = c(200,850), ylim = c(0,40))
hist(sample13.means, main = "n = 30", xlim = c(200,850), ylim = c(0,40))
hist(sample14.means, main = "n = 50", xlim = c(200,850), ylim = c(0,40))
hist(sample15.means, main = "n = 100", xlim = c(200,850), ylim = c(0,40))
hist(sample16.means, main = "n = 1000", xlim = c(200,850), ylim = c(0,40))
hist(sample17.means, main = "n = 2500", xlim = c(200,850), ylim = c(0,40))
hist(sample18.means, main = "n = 5000", xlim = c(200,850), ylim = c(0,40))
hist(sample19.means, main = "n = 10000", xlim = c(200,850), ylim = c(0,40))
hist(sample20.means, main = "n = 100000", xlim = c(200,850), ylim = c(0,40))

#Question 3
newsample.data = NA
newsample.means = NA

for(counter in 1:100000)
{
  newsample.data = rnorm(30, 500, 100)
  newsample.means[counter] = mean(newsample.data)
}

stuff = hist(newsample.means)
stuff
