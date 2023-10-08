### WEEK 7 Exercises

zscore = function(x){
  mew = mean(x)
  sd = sd(x)
  z = (mew-x)/sd
  z
}

#Ways to test differences

#Load a dataset

fake_core <- read.csv('data/Fake_core_plus.csv', header = TRUE, row.names = 'depth')

#or

IB <- read.csv('QPP_Week7/stationIB.csv', header = TRUE)
IVC <- read.csv('QPP_Week7/stationIVC.csv', header = TRUE)
thompson <- read.csv('QPP_Week7/thompson.csv', header = TRUE)

#Let's look at results by category for one of the sites.

hist(IB$D.L.Ratio...GLU)
hist(log(IVC$D.L.Ratio...GLU))

#Basic plots

plot(IB$D.L.Ratio...GLU, 
     IB$D.L.Ratio...ASP, 
     pch = IB$Grade, 
     xlim = c(0, max(c(IB$D.L.Ratio...GLU, IVC$D.L.Ratio...GLU))),
     ylim = c(0, max(c(IB$D.L.Ratio...ASP, IVC$D.L.Ratio...ASP))))
points(IVC$D.L.Ratio...GLU, IVC$D.L.Ratio...ASP, pch = IVC$Grade, col = "blue")

#We can ask about relationships here...

IB.line <- lm(IB$D.L.Ratio...ASP~IB$D.L.Ratio...GLU)
IVC.line <- lm(IVC$D.L.Ratio...ASP~IVC$D.L.Ratio...GLU)
abline(IB.line, lty = 3)
abline(IVC.line, lty = 3, col = "blue")

#How useful are the different grades?

boxplot(IB$D.L.Ratio...GLU~IB$Grade, horizontal = TRUE)
boxplot(IVC$D.L.Ratio...GLU~IVC$Grade, horizontal = TRUE)

#