# Week 6 - Live without a net (except internet!)

#Goals:

#Install a package

#Load a library

#Read some data

#Use that library to make a plot.

#Calculate percents of observations

#Plot percents, polygons, bars.

#Summarize by monocots and dicots.

#Use barplot to show changes through time.

####CODING REGION

#Package installation - there's multiple methods!

#Packages are hypothetical objects on the internet and are called with chararacter strings.

#We're using Juggin's "rioja" package.

#install.packages("rioja")

#Now we have the package installed, let's load the library.

#Libraries are known objects to R, it knows where to find them! 

library(rioja)

#Don't forget to cite!

#Let's load our fake core data.

fake_core <- read.csv("data/Fake_core.csv", header = TRUE, row.names = "depth")

#Check data with head() or tail()

head(fake_core)
tail(fake_core)

#Let's use rioja to make a stratigraphic plot.

###Coding strategy! Start simple.

strat.plot(fake_core)

#Reversing y-axis and adding colors.

#Note we concatenate the colors!

strat.plot(fake_core, y.rev = TRUE, col.line = c("purple", "pink"))

#Another thing I love about rioja are polygon plots.

#Making percents.

sample_sums <- apply(fake_core, 1, sum)

fake_pct <- (fake_core/sample_sums)*100

#Check percents with apply across the rows.

apply(fake_pct, 1, sum)

#Let's compress this code a bit.

fake_pct <- (fake_core/apply(fake_core, 1, sum))*100

#We've successfully made percents, let's plot them.

strat.plot(fake_pct, 
           scale.percent = TRUE, 
           y.rev = TRUE, 
           col.line = c("pink", "purple"))


#Let's plot polygons and let's color by monocot vs. dicot.

plant_class = c(rep("mono", 3), rep("dicot", 6))

plant_class <- c(rep("mono", 3), 
                 rep("dicot", ncol(fake_pct) - 3))

#Chris' experiment, don't copy

Naucoridae <- floor(rnorm(100, 20, 4))
fake_core <- cbind(fake_core, Naucoridae)

#Let's make colors.

plot_colors <- vector("character", length = ncol(fake_pct))

plot_colors[plant_class == "mono"] = "orange"
plot_colors[plant_class != "mono"] = "turquoise3"

#Let's plot with colors.

strat.plot(fake_pct, 
           scale.percent = TRUE, 
           y.rev = TRUE, 
           plot.line = FALSE, 
           plot.poly = TRUE,
           col.poly = plot_colors)

#Let's try bars

strat.plot(fake_pct, 
           scale.percent = TRUE, 
           y.rev = TRUE,
           plot.bar = TRUE, #We add plot.bar = TRUE
           plot.line = FALSE, 
           plot.poly = FALSE, #We switch this to FALSE
           col.bar = plot_colors) #We change to col.bar instead of col.poly.

#Let's use apply and subsetting simultaneously.

md_table <- matrix(ncol = 2, nrow = nrow(fake_core))

#We have to use apply and subsetting.

md_table[ , 1] = apply(fake_pct[ , plant_class == "mono"], 1, sum)
md_table[ , 2] = apply(fake_pct[ , plant_class != "mono"], 1, sum)

apply(md_table, 1, sum)

#Let's make a barplot

barplot(md_table)

#It works but it is ugly (we know how you feel little code...)

#Add beside and horizontal arguments

barplot(md_table, beside = TRUE, horiz = TRUE)

#Plots are still ugly. Barplot expects the samples to be columns. We need to transpose.

fx_table <- t(md_table)

#Now let's make a barplot.

barplot(fx_table, 
        horiz = TRUE, 
        col = c("orange", "turquoise3"))

#Let's add some labels.

barplot(fx_table, 
        horiz = TRUE, 
        col = c("orange", "turquoise3"),
        main = "Barplot of Mono/Dicot Ratio")

#We can label yaxis with depths

barplot(fx_table, 
        horiz = TRUE, 
        col = c("orange", "turquoise3"),
        main = "Barplot of Mono/Dicot Ratio",
        names.arg = row.names(fake_core))

#We can make labels horizontal using las = 

barplot(fx_table, 
        horiz = TRUE, 
        col = c("orange", "turquoise3"),
        main = "Barplot of Mono/Dicot Ratio",
        names.arg = row.names(fake_core),
        las = 1)

#We can also change the text size and add a legend.

barplot(fx_table, 
        horiz = TRUE, 
        col = c("orange", "turquoise3"),
        main = "Barplot of Mono/Dicot Ratio",
        names.arg = row.names(fake_core),
        las = 1,
        cex.names = 0.5)

legend(60, 80, c("mono", "dicot"), pch = 22, pt.bg = c("orange", "turquoise3"))

#Let's get the legend off of the bar graph.

barplot(fx_table, #Calling the object
        horiz = TRUE, #Make it horizontal
        col = c("orange", "turquoise3"), #Give it colors
        main = "Barplot of Mono/Dicot Ratio", #Main title
        names.arg = row.names(fake_core), #Names of samples
        las = 1, #Make them horizontal
        cex.names = 0.5, #Make them half the normal size
        xlim = c(0, 150), #Make the limits of the x axis 0 and 150
        axes = FALSE) #Tell R not to draw axes.

legend(110, 80, c("mono", "dicot"), pch = 22, pt.bg = c("orange", "turquoise3"))
axis(1, at = seq(0,100,10)) #Use axis function to make a new axis.

