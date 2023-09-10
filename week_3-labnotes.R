### Script for Week 3

packages <- c("vioplot") #Make an object listing packages

install.packages(setdiff(packages, row.names(installed.packages())))

#We use packages via library

library(vioplot)

vioplot::vioplot() #This lets us call a function without loading the library.

#Let's read our fake data.

fake_core <- read.csv("data/Fake_core.csv",
                      header = TRUE,
                      row.names = "depth")

ncol(fake_core) #How many variables?
nrow(fake_core) #How many samples?

#Summing fake_core

sum(fake_core)

sum(fake_core$)

#How chris writes his code.

fake_core <- read.csv("data/Fake_core.csv", 
                      header = TRUE, 
                      row.names = "depth")

#Median

median(fake_core$Cyperaceae.undiff.)

vls_cyp <- unique(fake_core$Cyperaceae.undiff.) #Unique gives in order encountered.

#We need these in order, however.

ord_cyp <- vls_cyp[order(vls_cyp)] #Here, we are taking those values and organizing them by rank-order. This works in a cool way.

ord_cyp

#Lets change what variable we use?

plot(fake_core$Alchornea,
     100:1,
     xlim = c(0, 100),
     pch = 19)

lines(table(fake_core$Alchornea)*10,
      lty = 3,
      col = "darkred",
      lwd = 1) #Adding line width argument here, lets us define the width of lines we're plotting.

lines(c(mean(fake_core$Alchornea),
        mean(fake_core$Alchornea)),
      c(0, 100),
      lty = 1,
      lwd = 3)

lines(c(median(fake_core$Alchornea),
        median(fake_core$Alchornea)),
      c(0, 100),
      lty = 1,
      col = "blue",
      lwd = 3)

#Custom code for plotting with table for freq dist.

cust_plot = function(x){ #Here we define the function name and what it needs...
  par(mfrow = c(3, 3)) #Warning! Custom code! 3 x 3 plot matrix.
  title_names = colnames(x)
  for(i in 1:ncol(x)){ #Calling for loop - will run 9 times! 
    plot(x[,i], #Every loop draws a plot 9
         1:length(x[,i]),
         xlim = c(0, max(x[,i])),
         pch = 19,
         xlab = "NISP",
         ylab = "sample")
    
    lines(rep(mean(x[,i]),2), #Adding mean lines!
          c(0, length(x[,i])),
          lwd = 3)
    
    lines(rep(median(x[,i]),2), #Adding median lines!
          c(0, length(x[,i])),
          lwd = 3,
          col = "blue")
    
    lines((table(x[,i])/sum(table(x[,i])))*100, #Using percent of table results!
          lty = 3,
          col = "darkred",
          lwd = 1)
    
    title(main = title_names[i]) #
    axis(4, at = seq(10,100,10), labels = seq(0.1,1,0.1))
  }
  par(mfrow = c(1, 1))
}

#Plotting sedge and histogram.

par(mfrow = c(2, 1))

plot(fake_core$Cyperaceae.undiff.,
     100:1,
     xlim = c(0, 50),
     pch = 19)

lines(c(mean(fake_core$Cyperaceae.undiff.),
        mean(fake_core$Cyperaceae.undiff.)),
      c(0, 100),
      lty = 3)

lines(c(median(fake_core$Cyperaceae.undiff.),
        median(fake_core$Cyperaceae.undiff.)),
      c(0, 100),
      lty = 2)


hist(fake_core$Cyperaceae.undiff., 
     breaks = 100,
     xlim = c(0,50))

par(mfrow = c(1, 1))

#Box and whisker plot.

par(mar = c(5, 6, 4, 2) + 0.1)

boxplot(fake_core, 
        horizontal = TRUE, 
        las = 2, #Aspect for labeling, 1 = vert, 2 = horizonatal.
        cex.axis = 0.5) #Size of axis labels. 

par(mar = c(5, 4, 4, 2) + 0.1)

#Kernel density plotting, custom function.

dens_plot = lapply(fake_core, function(x){
  density(x)
})

#Plotting kernel densities.

par(mar = c(5, 6, 4, 2) + 0.1)

boxplot(fake_core, 
        horizontal = TRUE, 
        las = 2, 
        cex.axis = 0.5,
        main = "Boxplots and Kernel Densities")

for(i in 1:length(dens_plot)){
  ylines = (dens_plot[[i]][['y']]/max(dens_plot[[i]][['y']]))*0.5
  lines(dens_plot[[i]][['x']], ylines + i, lty = 3, col = "blue")
}













