#Week 7 - Importing Data and Testing Categoricals

#Libraries

#install.packages("wesanderson")

library(wesanderson)

#Goals:

##1 - read data. CHECK

##1A - take three files, combine, and save. CHECK

##1B - NOSE, CHECK.

##2 - plot amino acid results by station, by category. CHECK-ish

##3 - test amino acid results against categories.

##4 - test whether AAR relationship is the same in both stations.

#Read the AAR data.

IB <- read.csv("QPP_Week7/stationIB.csv", header = TRUE)
IVC <- read.csv("QPP_Week7/stationIVC.csv", header = TRUE)
TF <- read.csv("QPP_Week7/thompson.csv", header = TRUE)

#Make a single data.frame with data.

all_data <- rbind(IB, IVC)
#
stations <- c(rep("IB",nrow(IB)),
              rep("IVC",nrow(IVC)))
#
all_data <- cbind(stations, all_data)
#
#write.csv(all_data, "all_stations.csv") #write.csv wants the object to be written first, then the file name.

#AAdata <- read.csv("QPP_Week7/AAdata.csv", header = TRUE) #Complicated! Creates errors!

#Check that data is read correctly.

head(IB)
tail(IVC)

#Let's make a color vector.

plot_colors <- terrain.colors(3, rev = TRUE)

#Let's look at the two amino acids for IB

plot(IB$D.L.Ratio...GLU,
     IB$D.L.Ratio...ASP,
     pch = IB$Grade+20,
     main = "Station IB AAR Data", #Short, but useful names are best.
     xlab = "GLU Racemization", #use xlab to clean up name.
     ylab = "ASP Recemization",
     xlim = c(0,max(IB$D.L.Ratio...GLU)),
     ylim = c(0,max(IB$D.L.Ratio...ASP)),#xlim sets range of x axis
     bg = plot_colors) #col = sets colors.

#Let's add a legend.

legend(0, 
       max(IB$D.L.Ratio...ASP), 
       c("Grade 1", "Grade 3", "Grade 5"),
       pch = unique(IB$Grade)+20,
       pt.bg = plot_colors)

#Here's some useful color functions.

heat.colors(3)
rainbow(3)
terrain.colors(3)

#Let's manipulate the color palette.

palette(wes_palette("Royal1", 100, type = "continuous"))

#Let's test our palette.

plot(1:100, 1:100, col = 1:100)

#Let's plot both stations simultaneously.

IB_selector = all_data$stations == "IB"
IVC_selector = all_data$stations == "IVC"

palette("default")

plot(all_data$D.L.Ratio...GLU[all_data$stations == "IB"],
     all_data$D.L.Ratio...ASP[all_data$stations == "IB"],
     xlim = c(0,max(all_data$D.L.Ratio...GLU[all_data$stations == "IB"])))

plot(all_data$D.L.Ratio...GLU[IB_selector],
     all_data$D.L.Ratio...ASP[IB_selector],
     xlim = c(0,max(all_data$D.L.Ratio...GLU)),
     ylim = c(0,max(all_data$D.L.Ratio...ASP)))

#Adding points to plot.

points(all_data$D.L.Ratio...GLU[IVC_selector],
       all_data$D.L.Ratio...ASP[IVC_selector],
       col = "blue")

#The above is an intentionally bad example. Use discrete objects!

#This plots the two stations.

plot(IB$D.L.Ratio...GLU,
     IB$D.L.Ratio...ASP,
     pch = 21,
     main = "IB vs IVC AAR Data", #Short, but useful names are best.
     xlab = "GLU Racemization", #use xlab to clean up name.
     ylab = "ASP Recemization",
     xlim = c(0,max(c(IB$D.L.Ratio...GLU, IVC$D.L.Ratio...GLU))),
     ylim = c(0,max(c(IB$D.L.Ratio...ASP, IVC$D.L.Ratio...ASP))),#xlim sets range of x axis
     bg = "red")

points(IVC$D.L.Ratio...GLU,
       IVC$D.L.Ratio...ASP,
       pch = 22,
       bg = "blue")

#Making histograms of results from two stations. Use barplot to compare.

hist(IB$D.L.Ratio...GLU)
hist(IB$D.L.Ratio...ASP)

plot(table(IB$D.L.Ratio...GLU))

IB_GLUhist <- hist(IB$D.L.Ratio...GLU, plot = FALSE) #Saving histogram as object.
IVC_GLUhist <- hist(IVC$D.L.Ratio...GLU, plot = FALSE) #Saving IVC histogram as object.

IB_GLUhist$counts #This recovers the frequencies of the bins...

barplot(IB_GLUhist$counts, 
        ylim = c(-20,20), 
        xlim = c(0,length(IVC_GLUhist$breaks)), 
        col = "blue",
        main = "Chris' way better barplot") #Use barplot to plot counts.

barplot(IVC_GLUhist$counts*-1, 
        add = TRUE, 
        col = "red")

#Let's finally compare taphonomic grades.

boxplot(IB$D.L.Ratio...GLU~IB$Grade, horizontal = TRUE)

vioplot::vioplot(IB$D.L.Ratio...GLU~IB$Grade, horizontal = TRUE)

library(vioplot)

vioplot(IB$D.L.Ratio...GLU~IB$Grade, horizontal = TRUE)

aov(IB$D.L.Ratio...GLU~IB$Grade)

IB.model <- aov(IB$D.L.Ratio...GLU~IB$Grade)

#TukeyHSD(IB.model)

#Let's look at linear models of GLU/ASP relationship

par(mfrow = c(1, 2), mar = c(6, 4, 4, 3) + 0.1)

IB.AAlm <- lm(IB$D.L.Ratio...ASP~IB$D.L.Ratio...GLU) #Y-axis first, X-axis second.
IVC.AAlm <- lm(IVC$D.L.Ratio...ASP~IVC$D.L.Ratio...GLU)

plot(IB$D.L.Ratio...GLU,
     IB$D.L.Ratio...ASP,
     pch = 21,
     main = "IB vs IVC AAR Data", #Short, but useful names are best.
     xlab = "GLU Racemization", #use xlab to clean up name.
     ylab = "ASP Recemization",
     xlim = c(min(c(IB$D.L.Ratio...GLU, IVC$D.L.Ratio...GLU, TF$D.L.Glu)),max(c(IB$D.L.Ratio...GLU, IVC$D.L.Ratio...GLU, TF$D.L.Glu))+0.4),
     ylim = c(min(c(IB$D.L.Ratio...ASP, IVC$D.L.Ratio...ASP, TF$D.L.Asp)),max(c(IB$D.L.Ratio...ASP, IVC$D.L.Ratio...ASP, TF$D.L.Asp))),#xlim sets range of x axis
     bg = IB$Grade)

points(IVC$D.L.Ratio...GLU,
       IVC$D.L.Ratio...ASP,
       pch = 22,
       bg = IVC$Grade)

points(TF$D.L.Glu,
       TF$D.L.Asp,
       pch = 23,
       bg = "gold")


abline(IB.AAlm, lty =3, col = "darkred")
abline(IVC.AAlm, lty =3, col = "darkblue")

legend("bottomright",
       c("Station IB", "Station IVC", "Thomp. Form.", "G1", "G2", "G3", "IB reg line", "IVC reg line"),
       pch = c(21, 22, 23, 21, 21, 21, NA, NA),
       pt.bg = c(NA, NA, "gold", 1, 3, 5, NA, NA),
       lty = c(NA, NA, NA, NA, NA, NA, 3, 3),
       col = c(1, 1, 1, 1, 1, "darkred", "darkblue"),
       cex = 0.75)

#Log-transformed plotting below.

IB.AAlm <- lm(log(IB$D.L.Ratio...ASP)~log(IB$D.L.Ratio...GLU)) #Y-axis first, X-axis second.
IVC.AAlm <- lm(log(IVC$D.L.Ratio...ASP)~log(IVC$D.L.Ratio...GLU))

plot(log(IB$D.L.Ratio...GLU),
     log(IB$D.L.Ratio...ASP),
     pch = 21,
     main = "Log-Transformed IB vs IVC AAR Data", #Short, but useful names are best.
     xlab = "log GLU Racemization", #use xlab to clean up name.
     ylab = "log ASP Recemization",
     xlim = c(min(log(c(IB$D.L.Ratio...GLU, IVC$D.L.Ratio...GLU, TF$D.L.Glu))),max(log(c(IB$D.L.Ratio...GLU, IVC$D.L.Ratio...GLU, TF$D.L.Glu)))+1),
     ylim = c(min(log(c(IB$D.L.Ratio...ASP, IVC$D.L.Ratio...ASP, TF$D.L.Asp))),max(log(c(IB$D.L.Ratio...ASP, IVC$D.L.Ratio...ASP, TF$D.L.Asp)))),#xlim sets range of x axis
     bg = IB$Grade)

points(log(IVC$D.L.Ratio...GLU),
       log(IVC$D.L.Ratio...ASP),
       pch = 22,
       bg = IVC$Grade)

points(log(TF$D.L.Glu),
       log(TF$D.L.Asp),
       pch = 23,
       bg = "gold")


abline(IB.AAlm, lty =3, col = "darkred")
abline(IVC.AAlm, lty =3, col = "darkblue")

legend("bottomright",
       c("Station IB", "Station IVC", "Thomp. Form.", "G1", "G2", "G3", "IB reg line", "IVC reg line"),
       pch = c(21, 22, 23, 21, 21, 21, NA, NA),
       pt.bg = c(NA, NA, "gold", 1, 3, 5, NA, NA),
       lty = c(NA, NA, NA, NA, NA, NA, 3, 3),
       col = c(1, 1, 1, 1, 1, "darkred", "darkblue"),
       cex = 0.75)

par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)

#You can call details of linear model with summary()

summary(IB.AAlm)

#Aov attempt

IB.aov <- aov(IB$D.L.Ratio...GLU~as.factor(IB$Grade)) #Works if you make these factors!

IB_aov <- summary(IB.aov)

TukeyHSD(IB.aov)

plot(TukeyHSD(IB.aov))

IBGLU_G1 <- IB$D.L.Ratio...GLU[IB$Grade == 1]
IBGLU_G3 <- IB$D.L.Ratio...GLU[IB$Grade == 3]
IBGLU_G5 <- IB$D.L.Ratio...GLU[IB$Grade == 5]

