#CAK's Week 4 notes

fake_core <- read.csv("data/Fake_core.csv",
         header = TRUE,
         row.names = "depth")

mad_cyp = sum( abs(fake_core$Cyperaceae.undiff. - median(fake_core$Cyperaceae.undiff.) ) ) / length(fake_core$Cyperaceae.undiff.)

mad_cyp

#using variance

var(fake_core$Cyperaceae.undiff.)

var_cyp = mean((fake_core$Cyperaceae.undiff. - mean(fake_core$Cyperaceae.undiff.))^2)


var(fake_core$Cyperaceae.undiff.)

var(fake_core$Celtis)

#Making data frame for variance tests

my_df = sapply(1:10, function(x){ #Using function to create progressively larger runs of numbers that are equally spaced.
  seq(x, x*100, x)
})

df_var = apply(my_df, 2, var)

#Looking at changes in length of variable and counts...

norm_range = #Using <shift+enter>, we can already make a visualization of the counts...
  c(5, 5,
    6, 6, 6,
    7, 7, 7, 7,
    8, 8, 8, 8, 8,
    9, 9, 9, 9, 9, 9,
    10, 10, 10, 10, 10,
    11, 11, 11, 11,
    12, 12, 12,
    13, 13)

plot_var = sapply(1:100, function(x){
  var(rep(norm_range, x))
})

plot_mad = sapply(1:100, function(x){
  mad(rep(norm_range, x))
})

#lets make this for means

plot_mean = sapply(1:100, function(x){
  mean(rep(norm_range, x))
})

par(mfrow = c(1, 2))

plot(plot_var, ylim = c(floor(min(plot_mad)) - 0.5, ceiling(max(plot_var)) +0.5), pch = 21, bg = "orange")

lines(plot_mad)

plot(table(rep(norm_range,100)))

v_range = c(
  5, 5, 5, 5, 5, 5,
  6, 6, 6, 6, 6, 
  7, 7, 7, 7, 
  8, 8, 8,
  9, 9, 
  10, 10, 10, 
  11, 11, 11, 11,
  12, 12, 12, 12, 12,
  13, 13, 13, 13, 13, 13)

plot_var = sapply(1:100, function(x){
  var(rep(v_range, x))
})

plot_mad = sapply(1:100, function(x){
  mad(rep(v_range, x))
})

par(mfrow = c(1, 2))

plot(plot_var, ylim = c(floor(min(plot_mad)) - 0.5, ceiling(max(plot_var)) +0.5), pch = 21, bg = "orange")

lines(plot_mad)

plot(table(rep(v_range,100)))

#Function for plotting various univariate statistics

stat_lines = function(x,#Creating a function called "stat_lines" it expects some vector of data "x" and the following arguments
                      times = length(x), #Setting this to "length(x)" by default, so calling this won't always be necessary. But we can add our own numbers!
                      title = NA){ #Setting up a title object, defaulting to NA
  
  plot_u = sapply(1:times, function(y){ #Here's that fast custom function that creates means for the repeated set of numbers.
    mean(rep(x, y))
  })
  
  plot_med = sapply(1:times, function(y){ #Same, but for the median.
    median(rep(x, y))
  })
  
  plot_var = sapply(1:times, function(y){ #Same, but for variance.
    var(rep(x, y))
  })
  
  plot_mad = sapply(1:times, function(y){ #Same, but for median absolute deviation
    mad(rep(x, y))
  })
  
  stat_labels = c("mean", "median", "variance", "median abs. dev.")
  
  plot_stats = data.frame(plot_u, plot_med, plot_var, plot_mad)
  
  plot(0, 0, xlim = c(0, times), ylim = c(0, max(plot_stats)+1), pch = NA, xlab = "N repeats", ylab = "value", main =paste0(title," Repeated Statistics"))
  
  for(i in 1:ncol(plot_stats)){
    points(plot_stats[, i], type = "o", pch = 21, lty = i, bg = i, cex = 0.7)
    text(times*0.05+(i*(0.2*times)), min(plot_stats[, i])+(0.04*max(plot_stats)), labels = stat_labels[i])
  }
  
}

stat_lines(v_range, title = "V-Shaped Range")

#Comparing different variables

par(mfrow = c(1,2))

stat_lines(v_range, title = "V-Shaped Range", times = 100)

stat_lines(norm_range, title = "Normal Range", times = 100)

par(mfrow = c(1,1))

#Updating custom function

stat_lines = function(x,#Creating a function called "stat_lines" it expects some vector of data "x" and the following arguments
                      times = length(x), #Setting this to "length(x)" by default, so calling this won't always be necessary. But we can add our own numbers!
                      title = NA){ #Setting up a title object, defaulting to NA
  
  plot_u = sapply(1:times, function(y){ #Here's that fast custom function that creates means for the repeated set of numbers.
    mean(rep(x, y))
  })
  
  plot_med = sapply(1:times, function(y){ #Same, but for the median.
    median(rep(x, y))
  })
  
  #plot_var = sapply(1:times, function(y){ #Same, but for variance. #We can keep our code and just hash this out.
  #  var(rep(x, y))
  #})
  
  plot_mad = sapply(1:times, function(y){ #Same, but for median absolute deviation
    mad(rep(x, y))
  })
  
  plot_sd = sapply(1:times, function(y){
    sd(rep(x, y))
  })
  
  stat_labels = c("mean", "median", "median abs. dev.", "SD") #Because we automate everything below, all we have to do is modify the names.
  
  plot_stats = data.frame(plot_u, plot_med, plot_mad, plot_sd) #We're using the plot_stats dataframe below, so we modify the entry here.
  
  plot(0, 0, xlim = c(0, times), ylim = c(0, max(plot_stats)+1), pch = NA, xlab = "N repeats", ylab = "value", main =paste0(title," Repeated Statistics"))
  
  for(i in 1:ncol(plot_stats)){
    points(plot_stats[, i], type = "o", pch = 21, lty = i, bg = i, cex = 0.7)
    text((times/ncol(plot_stats)*i), min(plot_stats[, i])+(0.04*max(plot_stats)), labels = stat_labels[i]) #Changing text plotting to fit new variable.
  }
  
}

#Plotting across some fake_core data

par(mfrow = c(2,2))

stat_lines(fake_core$Cyperaceae.undiff., title = "Cyperaceae")
stat_lines(fake_core$Typha, title = "Typha")
stat_lines(fake_core$Alchornea, title = "Alchornea")
stat_lines(fake_core$Guibourtia.demeusei, title = "Guibourtia")

par(mfrow = c(1,1))

#Normal distributions

norm_vector <- rnorm(n = 100, mean = 50, sd = 10)

hist(norm_vector)

#Increasing normal by orders of magnitude

par(mfrow = c(3,2)) #We're making six plots

invisible(sapply(1:6, function(x){ #Using a custom function six times
  hist(rnorm(n = 10^x, #Call histogram for a normally distributed set of random numbers, increasing by orders of magnitude (10^i)
             mean = 50, #Set the mean of the random values
             sd = 10), #Set the standard deviation
       breaks = 50, #Breaks for histogram
       main = paste0("Histogram for n = ", 10^x)) #Title for plots
}))

#Lets do rbeta distributions

par(mfrow = c(3,2))

sapply(exp(seq(0.2, 1.2, 0.2)), function(x){ #Using a custom function six times
  hist(rbeta(1000000, #Call histogram for a normally distributed set of random numbers, increasing by orders of magnitude (10^i)
             x, #Setting shape values
             10), #Setting shape values
       breaks = 50, #Breaks for histogram
       main = paste0("Histogram for Skewed Data")) #Title for plots
})

par(mfrow = c(1,1))

#Multiple random number generating functions

my_log <- rlogis(1000)
my_exp <- rexp(1000)
my_nrm <- rnorm(1000)
my_skw <- rbeta(1000, 10, 2)

my_dist <- data.frame(log = my_log, my_exp, my_nrm, my_skw)

my_hist <- function(x, data){
  hist(data[,x], breaks = 50, main = colnames(data)[x])
}

par(mfrow = c(3,2))

invisible(lapply(1:ncol(my_dist), my_hist, data = my_dist))

par(mfrow = c(1,1))

#Notmalizing random number draws

prob_dist <- data.frame(log = abs(my_log)/max(abs(my_log)), #We take absolute value and divide by max
                        exponential = my_exp/max(my_exp), #We divide by the max value
                        normal =  (my_nrm + abs(min(my_nrm)))/(max(my_nrm + abs(min(my_nrm)))),  #Add the minimum (to get to 0), then divide by the new maximum.
                        right_skew = my_skw) #Beta-generated distributions are prob. distributions!


#plotting adjusted random number draws

par(mar = c(5, 6, 4, 2) + 0.1)

vioplot::vioplot(prob_dist,
                 horizontal = TRUE,
                 col = "gold",
                 las = 1,
                 main = "Violin Plots of Prob Distributions")

par(mar = c(5, 4, 4, 2) + 0.1)

#Sampling

my_pop = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")

my_spl = sample(my_pop, size = 5)

#Sampling with args

my_spl = sample(my_pop, 
                size = 10,
                replace = FALSE)

my_spl[order(my_spl)]
