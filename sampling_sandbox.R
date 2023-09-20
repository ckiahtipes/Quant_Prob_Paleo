#Working out sampling from probability distributions

my_log <- rlogis(1000)
my_exp <- rexp(1000)
my_nrm <- rnorm(1000)
my_skw <- rbeta(1000, 10, 2)

my_pop = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")

my_dist <- data.frame(log = my_log, my_exp, my_nrm, my_skw)


prob_dist <- data.frame(log = abs(my_log)/max(abs(my_log)), #We take absolute value and divide by max
                        exponential = my_exp/max(my_exp), #We divide by the max value
                        normal =  (my_nrm + abs(min(my_nrm)))/(max(my_nrm + abs(min(my_nrm)))),  #Add the minimum (to get to 0), then divide by the new maximum.
                        right_skew = my_skw) #Beta-generated distributions are prob. distributions!

my_spl = sample(my_pop,
                size = 1000,
                replace = TRUE,
                prob = dnorm(x = seq(10,100,10)))

plot(table(my_spl))

#We need to add some realistic parameters to put this into practice.

#Imagine a population of three critters, who make up different proportions of the total population (reasons may vary).

pop_prop = data.frame(species = c("A", "B", "C"),
                      pop_prp = c(0.6, 0.3, 0.1))

#Can we use this to define a fossil population based on time-averaging?

fossil_pops = data.frame(carbonate = sample(pop_prop$species,
                                   size = 2000,
                                   replace = TRUE,
                                   prob = pop_prop$pop_prp),
                         silaceous = sample(pop_prop$species,
                                    size = 4000,
                                    replace = TRUE,
                                    prob = pop_prop$pop_prp))

#No, this just gives us proportional increase. We're not including the time variable in the probabilities?

#We make a population, stating the proportion of the total population (assuming stable community) and their generation times.
  #Unsure if this is mathematically possible (community-wise vis-a-vis proportions and stable pops...)

somepop <- data.frame(taxon = c("A", "B", "C"),
                      model_pop = c(60, 30, 10),
                      pop_prop = c(0.6, 0.3, 0.1),
                      gen_time = c(5, 20, 50))

#So we turn this into a yearly fossil population, dividing 1 by the gen_time giving us deaths/year.

time_avg = 2000

fossil_pops = matrix(nrow = time_avg, ncol = length(somepop$taxon))
fossil_pops = as.data.frame(fossil_pops)
colnames(fossil_pops) = somepop$taxon

for(i in 1:time_avg){
  
  if(i == 1){
    fossil_pops[i, ] = somepop$model_pop * (1/somepop$gen_time)
  } else {
    fossil_pops[i,] = fossil_pops[i - 1,] + (somepop$model_pop * (1/somepop$gen_time))
  }
  
}

plot(prob_dist$right_skew)

#Translating some things from a text...

#Say we have four possible counts for a taxa.

poss_obs = c(1, 2, 3, 4)

#If we think of all the possible outcomes, how many are there are what are their probabilities?

times = 2

dist_mean = vector("numeric", length = length(my_pop)^times)

for(i in 1:length(dist_mean)){
  dist_mean[i] = mean(sample(poss_obs, 2, replace = TRUE))
}

#So, despite having even distributions of each individual outcome, combinations of calculations of the mean produce a normally range of values.

#This is the sampling distribution of the mean.

#The standard deviation of the sampling distribution is: the standard deviation of the parent population divided by the square root of the sample size.

#stdev_sample_dist = sd(parent_pop)/sqrt(n) #This roughly covers it.

#This is the standard error of the mean.

times = 10000

parent_pop = rnorm(10000, mean = 450, sd = 50)
parent_dns = density(parent_pop)

dist_mean = vector("numeric", length = times)

for(i in 1:length(dist_mean)){
  dist_mean[i] = mean(sample(parent_pop, 50, replace = TRUE))
}

sd(dist_mean)
dens_mean = density(dist_mean)


#plot(table(dist_mean))

plot(parent_dns$x, parent_dns$y/max(parent_dns$y), type = "l", ylim = c(0,1))
lines(dens_mean$x, dens_mean$y/max(dens_mean$y), col = "red", lty = 3)


#Formalize the experiment

par(mfrow = c(2,2))

sample_size = c(5, 10, 20, 40, 80)
times = 10000

parent_pop = rnorm(10000, mean = 450, sd = 50)
parent_dns = density(parent_pop)

plot(parent_dns$x, parent_dns$y/max(parent_dns$y), type = "l", ylim = c(0,1.3))
#legend(575, 0.8, c("parent distribution", sample_size), lty = c(1, 1:length(sample_size)), col = c("black",rep("red", length())))
lines(c(rep(mean(parent_pop),2)),c(0,1),col = "red")
lines(c(rep(median(parent_pop),2)),c(0,1),col = "blue")

for(i in 1:length(sample_size)){
  
  dist_mean = vector("numeric", length = times)
  dist_medi = vector("numeric", length = times)
  
  for(j in 1:times){
    dist_mean[j] = mean(sample(parent_pop, sample_size[i], replace = TRUE))
    dist_medi[j] = median(sample(parent_pop, sample_size[i], replace = TRUE))
  }
  
  dens_mean = density(dist_mean)
  dens_medi = density(dist_medi)
  lines(dens_medi$x, dens_medi$y/max(dens_medi$y), col = "blue", lty =i)
  lines(dens_mean$x, dens_mean$y/max(dens_mean$y), col = "red", lty = i)
  
}

#The larger the sample we take, the narrower the standard error. 

#This can be done for a skewed distribution also

sample_size = c(5, 10, 20, 40, 80)
times = 10000

parent_pop = prob_dist$right_skew
parent_dns = density(parent_pop)

plot(parent_dns$x, parent_dns$y/max(parent_dns$y), type = "l", ylim = c(0,1.3))
#legend(575, 0.8, c("parent distribution", sample_size), lty = c(1, 1:length(sample_size)), col = c("black",rep("red", length())))
lines(c(rep(mean(parent_pop),2)),c(0,1),col = "red")
lines(c(rep(median(parent_pop),2)),c(0,1),col = "blue")

for(i in 1:length(sample_size)){
  
  dist_mean = vector("numeric", length = times)
  dist_medi = vector("numeric", length = times)
  
  for(j in 1:times){
    dist_mean[j] = mean(sample(parent_pop, sample_size[i], replace = TRUE))
    dist_medi[j] = median(sample(parent_pop, sample_size[i], replace = TRUE))
  }
  
  dens_mean = density(dist_mean)
  dens_medi = density(dist_medi)
  lines(dens_medi$x, dens_medi$y/max(dens_medi$y), col = "blue", lty =i)
  lines(dens_mean$x, dens_mean$y/max(dens_mean$y), col = "red", lty = i)
  
}

#Also for exponential

sample_size = c(5, 10, 20, 40, 80)
times = 10000

parent_pop = prob_dist$exponential
parent_dns = density(parent_pop)

plot(parent_dns$x, parent_dns$y/max(parent_dns$y), type = "l", ylim = c(0,1.3))
#legend(575, 0.8, c("parent distribution", sample_size), lty = c(1, 1:length(sample_size)), col = c("black",rep("red", length())))
lines(c(rep(mean(parent_pop),2)),c(0,1),col = "red")
lines(c(rep(median(parent_pop),2)),c(0,1),col = "blue")

for(i in 1:length(sample_size)){
  
  dist_mean = vector("numeric", length = times)
  dist_medi = vector("numeric", length = times)
  
  for(j in 1:times){
    dist_mean[j] = mean(sample(parent_pop, sample_size[i], replace = TRUE))
    dist_medi[j] = median(sample(parent_pop, sample_size[i], replace = TRUE))
  }
  
  dens_mean = density(dist_mean)
  dens_medi = density(dist_medi)
  lines(dens_medi$x, dens_medi$y/max(dens_medi$y), col = "blue", lty =i)
  lines(dens_mean$x, dens_mean$y/max(dens_mean$y), col = "red", lty = i)
  
}

plot(0,0, pch = NA, ann = FALSE, axes = FALSE, xlim = c(0,10), ylim = c(0,10))
legend(1,10,
       c("parent distribution", rep(sample_size,2)), 
       lty = c(1, rep(1:length(sample_size),2)), 
       col = c("black", rep("red", length(sample_size)), rep("blue", length(sample_size))),
       cex = 0.8)

par(mfrow = c(1,1))

#Okay all of the above is cool and can go into week 5 lab.


#Going to do some area under the curve stuff here, need to quickly shade areas...

pop_dist = density(rnorm(10000, 100, 20))

plot(pop_dist)

pnorm(150, 100, 20)

norm_area = function(dist, value, col = "gray"){
  yt = dist$y[dist$x < value]
  yb = rep(0, length(yt))
  
  x = dist$x[dist$x < value]
  
  
  polygon(c(x, rev(x)), c(yt,yb), col = col)
}

norm_area(pop_dist, 150, col = "red")

arrows(100, 0.010, 150, 0.010)
lines(c(150,150), c(0,0.020), lty = 3)
text(151, 0.015, "z = 2.5, p = 0.0062")

#How about non-normal distributions

x_vals = seq(0, 10, 0.1)
y_dist = dexp(x_vals, 2)

plot(x_vals, y_dist, "l")

x_rnge = x_vals[x_vals < 2]
y_rnge = y_dist[x_vals < 2]

polygon(c(x_rnge, rev(x_rnge)), c(y_rnge,rep(0, length(y_rnge))), col = "red")

#Any way to cross these ideas with the fake core example and look at different scales?

fake_core <- read.csv("data/Fake_core.csv",
                      header = TRUE,
                      row.names = "depth")

zscore = function(x){
  mew = mean(x)
  sd = sd(x)
  z = (mew-x)/sd
  z
}

make_pct = function(x){
  sum = sum(x, na.rm = TRUE)
  pct = (x/sum)*100
}

fake_pct = apply(fake_core, 2, make_pct)
fake_zcore = apply(fake_core, 2, zscore)

boxplot(fake_pct, horizontal = TRUE)
boxplot(fake_zcore, horizontal = TRUE)
boxplot(fake_core, horizontal = TRUE)
