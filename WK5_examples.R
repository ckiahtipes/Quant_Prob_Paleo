#Week 4 Homework Solutions

#Making a normal distribution of length 10,000.

my_dist <- rnorm(10000, mean = 400, sd = 50)

head(my_dist)

#Make a distribtuion of this using density()

fq_dist <- density(my_dist)

plot(fq_dist) #Simple plotting

#Let's make a dataset based on the log normal distribution.
#you don't have to specify arguments so long as you put them in order.

my_log <- rlnorm(100000, 150, 25)

log_dist <- density(my_log)

plot(log_dist) #This won't plot correctly! Needs log axis.

plot(log_dist, 
     log = "x")

#I want to remember that I'm human and I make mistakes and it's okay.

plot(log(log_dist)) #This also doesn't work.

plot(log_dist$x, log = "x")

#These do work!
#Can also use a histogram

hist(my_log)

#Increase the number of breaks

hist(my_log, breaks = 100)

hist(log(my_log), breaks = 100) #this is better

#Make a distribution of non-normal data and plot it.

my_exp = rexp(1000000, 2)

hist(my_exp)

#We can use the square root to make this normal

hist(sqrt(my_exp))





#Week 5 lab coding examples


par(mfrow = c(2, 2))

my_sd = c(5, 10, 20, 40)

for(i in 1:length(my_sd)){
  plot(density(rnorm(10000, 100, my_sd[i])), xlim = c(-50, 250))
}

par(mfrow = c(1, 1))

#Finding area around specific values

my_normal = rnorm(10000, mean = 100, sd = 20)
norm_dist = density(my_normal)

plot(norm_dist)


lines(c(rep(75,2)),
      c(0,0.02))

arrows(mean(my_normal), 0.010, 75, 0.010, col = "blue")

#Distribution examples

plot(density(rnorm(10000, mean = 100, sd = 20)))

lines(c(rep(75,2)), c(0,0.02))

arrows(100, 0.010, 75, 0.010, col = "blue")

sample(my_normal, 10, replace = TRUE) #Sampling distribution to prove a point.

#Reading fake core and calculating percents.

fake_core = read.csv("data/Fake_core.csv", header = TRUE, row.names = "depth")

fake_sums = apply(fake_core, 1, sum)

fake_pct = (fake_core/fake_sums)*100

#Here's how we check that the percent calculation is correct. 

apply(fake_pct, 1, sum)

