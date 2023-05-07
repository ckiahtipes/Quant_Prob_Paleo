#Sandbox for course development code.

#Let's do some basic stuff with the Gulf of Mexico data.

live = read.csv('data/Live.csv', header = TRUE, row.names = "X")

stations = row.names(live)

station_sums = apply(live,1,sum)

taxa_sums = apply(live,2,sum)

#Let's make a toy dataset. 10 stations, 80 possible taxa.

toy_n = 10

toy_stations = paste0("station", c(1:toy_n))
toy_catchmnt = c(rep("Hills",toy_n*0.4),
                 rep("Calah",toy_n*0.3),
                 rep("Everg",toy_n*0.3))
toy_live = c(sample(c(1:100),toy_n))
toy_G1 = c(sample(c(50:150),toy_n))

toy_data = data.frame(toy_stations,toy_catchmnt,toy_live,toy_G1)

#This makes a roughly scalable matrix-generating set of commands. Now we can explore it a little.

hist(toy_data$toy_live)

hist(toy_data$toy_G1)

boxplot(toy_data$toy_live ~ toy_data$toy_catchmnt)

toy.aov = aov(toy_data$toy_live ~ toy_data$toy_catchmnt)
toy_results = unlist(summary(toy.aov))

#Can extract the values for plotting.

toy_results['F value1']
toy_results['Pr(>F)1']

#Barplot to see basic results

barplot(t(toy_data[,3:4]),horiz = TRUE, names.arg = toy_data$toy_stations, cex.names = 0.75, srt = 90, main = "Toy Data Boxplot",col=c(1,2))
legend(160,9,c("Live","G1"),pch=22,pt.bg=c(1,2))

#It's randomized, but the above will occasionally give us a significant result! Toy with the sample size and see how it behaves.

#Notice how using a variable (toy_n) to replace numbers makes it easier to go back and adjust it.

#Since we know how to build one random dataset, we should consider making a function. Our goal is to replace all of the numbers in the code above with variables.

make_toy = function(x, #All it needs is for us to define the sample size. Now we can scale up our observations.
                    cat.names = c("Hills","Calah","Everg"), 
                    range_live = c(1,100), 
                    range_G1 = c(50,150), 
                    c.probs = c(0.4,0.3,0.3)) #We define the variables in our new function here.
  { #Here is where we code out what the function does
  toy_stations = paste0("station", c(1:toy_n))
  toy_catchmnt = c(rep(cat.names,c.probs*toy_n))
  toy_live = sample(c(range_live[1]:range_live[2]),toy_n)
  toy_G1 = sample(c(range_G1[1]:range_G1[2]),toy_n)
  
  data.frame(toy_stations,toy_catchmnt,toy_live,toy_G1)
}

make_toy(10)

#Now, instead of looking at results individually, let's see what the range of possibilities are based on these parameters.

model_runs = 100 #Let's start with 10 runs.
toy_size = 40
Fstat = vector("numeric",model_runs)
Pval = vector("numeric",model_runs)

for(i in 1:model_runs){ #For every model run...
  toy_data = make_toy(toy_size) #Make a toy dataset
  toy.aov = aov(toy_data$toy_live ~ toy_data$toy_catchmnt) #Do the ANOVA on live vs G1
  toy_results = unlist(summary(toy.aov)) #Break down the results
  Fstat[i] = toy_results['F value1'] #Take F statistic and write to new variable
  Pval[i] = toy_results['Pr(>F)1'] #Take P value and write to new variable
}

#Now we can see how, given our paremeters, what the range of plausible outcomes are.

#How many times (out of 100) do we get a statistically significant result?

length(Pval[Pval < 0.05])/model_runs

points(Fstat,Pval,col="green")
#hist(Fstat)

#We can loop this one more time and save our model runs at different levels of sampling effort (toy_size)

sampling_bins = seq(10,100,5)

Pval_ratio = vector("numeric",length(sampling_bins))
Fstat_ratio = vector("numeric",length(sampling_bins))
mean_Pval = vector("numeric",length(sampling_bins))
mean_Fstat = vector("numeric",length(sampling_bins))

for(j in 1:length(sampling_bins)){
  model_runs = 100
  toy_size = sampling_bins[j]
  Fstat = vector("numeric",model_runs)
  Pval = vector("numeric",model_runs)
  
  for(i in 1:model_runs){ #For every model run...
    toy_data = make_toy(toy_size) #Make a toy dataset
    toy.aov = aov(toy_data$toy_live ~ toy_data$toy_catchmnt) #Do the ANOVA on live vs G1
    toy_results = unlist(summary(toy.aov)) #Break down the results
    Fstat[i] = toy_results['F value1'] #Take F statistic and write to new variable
    Pval[i] = toy_results['Pr(>F)1'] #Take P value and write to new variable
  }
  
  Pval_ratio[j] = length(Pval[Pval < 0.05])/model_runs
  Fstat_ratio[j] = length(Fstat[Fstat > 5])/model_runs
  mean_Pval[j] = mean(Pval)
  mean_Fstat[j] = mean(Fstat)
  
}



