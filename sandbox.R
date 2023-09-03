#Sandbox for course development code.

#Let's do some basic stuff with the Gulf of Mexico data.

live = read.csv('data/Live.csv', header = TRUE, row.names = "X") #Read data.
G1 = read.csv('data/G1.csv', header = TRUE, row.names = "X")

stations = row.names(live)

station_sums = apply(live,1,sum)

taxa_sums = apply(live,2,sum)

#Let's make a toy dataset. 10 stations, 80 possible taxa.

toy_n = 50

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
                    c.probs = c(0.4,0.3,0.3),
                    use_norm = FALSE) #We define the variables in our new function here.
  { #Here is where we code out what the function does
  toy_stations = paste0("station", c(1:x))
  toy_catchmnt = c(rep(cat.names,c.probs*x))
  if(use_norm == FALSE){
    toy_live = sample(c(range_live[1]:range_live[2]),x)
    toy_G1 = sample(c(range_G1[1]:range_G1[2]),x)
  } else {
    toy_live = rnorm(x,range_live[2] - ((range_live[2]-range_live[1])/2), 20)
    toy_G1 = rnorm(x,range_G1[2] - ((range_G1[2]-range_G1[1])/2), 15)
  }

  
  data.frame(toy_stations,toy_catchmnt,toy_live,toy_G1)
}

toy_data = make_toy(20, use_norm = TRUE)

barplot(t(toy_data[,3:4]),horiz = TRUE, names.arg = toy_data$toy_stations, cex.names = 0.75, srt = 90, main = "Toy Data Boxplot",col=c(1,2))
legend(160,9,c("Live","G1"),pch=22,pt.bg=c(1,2))

#Now, instead of looking at results individually, let's see what the range of possibilities are based on these parameters.

model_runs = 10 #Let's start with 10 runs.
toy_size = 100
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

#points(Fstat,Pval,col="green")
#hist(Fstat)

#We can loop this one more time and save our model runs at different levels of sampling effort (toy_size)

sampling_bins = seq(10,100,10)
bin_colors = heat.colors(length(sampling_bins),rev = TRUE)

Pval_ratio = vector("numeric",length(sampling_bins))
Fstat_ratio = vector("numeric",length(sampling_bins))
mean_Pval = vector("numeric",length(sampling_bins))
mean_Fstat = vector("numeric",length(sampling_bins))

Pval_matrix = matrix(nrow = 100, ncol = length(sampling_bins))
Fstat_matrix = matrix(nrow = 100, ncol = length(sampling_bins))

plot(0,0,xlim=c(-5,5),ylim=c(0,1),pch=NA,xlab="F-statistic",ylab="P value")

for(j in 1:length(sampling_bins)){
  
  model_runs = 100
  toy_size = sampling_bins[j]
  Fstat = vector("numeric",model_runs)
  Pval = vector("numeric",model_runs)
  
  for(i in 1:model_runs){ #For every model run...
    toy_data = make_toy(toy_size,use_norm = TRUE) #Make a toy dataset
    toy.aov = aov(toy_data$toy_live ~ toy_data$toy_catchmnt) #Do the ANOVA on live vs G1
    toy_results = unlist(summary(toy.aov)) #Break down the results
    Fstat[i] = toy_results['F value1'] #Take F statistic and write to new variable
    Pval[i] = toy_results['Pr(>F)1'] #Take P value and write to new variable
  }
  
  points(log(Fstat),Pval,pch=21,bg=bin_colors[j])
  
  Pval_ratio[j] = length(Pval[Pval < 0.05])/model_runs
  Fstat_ratio[j] = length(Fstat[Fstat > 5])/model_runs
  mean_Pval[j] = mean(Pval)
  mean_Fstat[j] = mean(Fstat)
  
  Pval_matrix[,j] = Pval
  Fstat_matrix[,j] = Fstat
  
}

plot(sampling_bins,Pval_ratio)
plot(sampling_bins,Fstat_ratio)
boxplot(Pval_matrix,ylab = "P value")

#What did we learn here?

#There's no relationship between the number of significant results and the sample size of the toy data sets! This seems weird. 

#The results tell us a couple things. F-statistic and P value show a logarithmic relationship in every set of modeled results, regardless of sample size.

#The likelihood of getting a spurious P value does not go down with sample size. We are always at risk of being misled by false positives.

#Let's do it again and make some real differences in the sample populations.

sampling_bins = seq(10,500,10)
bin_colors = heat.colors(length(sampling_bins),rev = TRUE)

Pval_ratio = vector("numeric",length(sampling_bins))
Fstat_ratio = vector("numeric",length(sampling_bins))
mean_Pval = vector("numeric",length(sampling_bins))
mean_Fstat = vector("numeric",length(sampling_bins))

Pval_matrix = matrix(nrow = 100, ncol = length(sampling_bins))
Fstat_matrix = matrix(nrow = 100, ncol = length(sampling_bins))

plot(0,0,xlim=c(-5,5),ylim=c(0,1),pch=NA,xlab="F-statistic",ylab="P value")

for(j in 1:length(sampling_bins)){
  
  model_runs = 100
  toy_size = sampling_bins[j]
  Fstat = vector("numeric",model_runs)
  Pval = vector("numeric",model_runs)
  
  for(i in 1:model_runs){ #For every model run...
    toy_data = make_toy(toy_size,use_norm = TRUE,range_live = c(100,150), range_G1 = c(250,300)) #Make a toy dataset
    toy.aov = aov(toy_data$toy_live ~ toy_data$toy_catchmnt) #Do the ANOVA on live vs G1
    toy_results = unlist(summary(toy.aov)) #Break down the results
    Fstat[i] = toy_results['F value1'] #Take F statistic and write to new variable
    Pval[i] = toy_results['Pr(>F)1'] #Take P value and write to new variable
  }
  
  points(log(Fstat),Pval,pch=21,bg=bin_colors[j])
  
  Pval_ratio[j] = length(Pval[Pval < 0.05])/model_runs
  Fstat_ratio[j] = length(Fstat[Fstat > 5])/model_runs
  mean_Pval[j] = mean(Pval)
  mean_Fstat[j] = mean(Fstat)
  
  Pval_matrix[,j] = Pval
  Fstat_matrix[,j] = Fstat
  
}

plot(sampling_bins,Pval_ratio)
plot(sampling_bins,Fstat_ratio)
boxplot(Pval_matrix,ylab = "P value")

#Found a basic fix for the simple model.

Hlive = toy_data[toy_data$toy_catchmnt == "Hills", 'toy_live']
HG1 = toy_data[toy_data$toy_catchmnt == "Hills", 'toy_G1']
all_obs = c(Hlive,HG1)
all_class= c(rep("live",length(Hlive)),rep("dead",length(HG1)))

test.aov = aov(all_obs~all_class)
summary(test.aov)
boxplot(all_obs ~ all_class)

#Looking at some different avenues for comparing things.

#Structure is off here need to work up a more interesting toy model.

#What does it need to look like in the end to reflect GoM surveys?
###Three tables (minimum)

###Station by species for each class (live, G1, etc.)
###Table of station data (coordinates, salinity, catchment, etc.)



#Thus, the model data set needs to be stations, taxa, and numbers. Numbers are defined by a probability distribution.

###This means that inputs need to be: stations, taxa_list, and distribution type.

#Will need to make some assumptions about spatial parameters, but we can assume a sort of reasonable grid for now.

### New Section - Coming up with a sampling scheme for critters on a map.

#We need to build a map object that holds key details.

fake_map = data.frame(x = rep(c(1:10), 10), #X grid coordinates up to desired sample size. Could modify to use another object.
                      y = as.vector(sapply(1:10, function(x){ #Y grid needs to be groups of 10 digits ascending to max.
                        rep(x, 10)
                      })),
                      elevation = as.vector(sapply(seq(10, 100, 10), function(x){ #Going to use elevation as an environmental gradient for dispersing taxa.
                        rep(x, 10)
                      })),
                      substrate = c(rep("clay", 20), rep("silt",40), rep("sand",40))) #Going to use this as taphonomic barrier.

palette(heat.colors(max(fake_map$elevation)))

substrates = unique(fake_map$substrate)

subst_vals = vector("numeric", length(fake_map$x))

for(i in 1:length(substrates)){
  subst_vals[fake_map$substrate == substrates[i]] = i
}


plot(fake_map$x, fake_map$y, pch = 20+subst_vals, bg = fake_map$elevation)



