#Ordination Examples and Exercises

#Key functions

zscore = function(x){
  mew = mean(x)
  sd = sd(x)
  z = (mew-x)/sd
  z
}

make_pct = function(x){
  samp_sum = apply(x, 1, sum)
  pct = (x/samp_sum)*100
  pct
}

#Libraries

library(vegan)
library(MASS)
library(rioja)

#We want to use transformed and un-transformed examples.

#Read data

fake_core <- read.csv("data/Fake_core.csv",
                      header = TRUE,
                      row.names = "depth")

#Derive percents

fake_pct <- make_pct(fake_core)

#Derive z-scores

fake_z <- apply(fake_core, 2, zscore)

#Run PCA on each.

fake.pca <- rda(fake_core)

fpct.pca <- rda(fake_pct)

fzcr.pca <- rda(fake_z)

#Create summary objects for plotting.

fake_pca <- summary(fake.pca)

fpct_pca <- summary(fpct.pca)

fzcr_pca <- summary(fzcr.pca)

#Make plot

plot(0, 0,
     pch = NA,
     xlim = c(min(fake_pca$sites[,1]),max(fake_pca$sites[,1])),
     ylim = c(min(fake_pca$sites[,2]),max(fake_pca$sites[,2])),
     )

for(i in 1:nrow(fake_pca$sites)){
  lines(c(fake_pca$sites[i,1], fpct_pca$sites[i,1], fzcr_pca$sites[i,1]),
        c(fake_pca$sites[i,2], fpct_pca$sites[i,2], fzcr_pca$sites[i,2]),
        lty = 3)
}

points(fake_pca$sites[,1], fake_pca$sites[,2], pch = 21, bg = "gold")
points(fpct_pca$sites[,1], fpct_pca$sites[,2], pch = 21, bg = "darkblue")
points(fzcr_pca$sites[,1], fzcr_pca$sites[,2], pch = 21, bg = "darkgreen")

#let's do NMDS to get a sense of ordination. 
#First we compute a dissimilarity matrix, then we use it for NMDS.

fake.dis <- vegdist(fake_core)
fake.mds <- isoMDS(fake.dis)
fpct.dis <- vegdist(fake_pct)
fpct.mds <- isoMDS(fpct.dis)

#Make a stressplot, showing relationship between dissimilarity and ordination distance.

stressplot(fake.mds, fake.dis)
stressplot(fpct.mds, fpct.dis)

ordiplot(fpct.mds, type = "t")

#Metadata MDS uses multiple runs of NMDS, compares stress, and picks best run.

fpct.Mmds <- metaMDS(fake_pct)

#We can clean this up, and plot next to the PCA result.

ordiplot(fpct.Mmds, type = "t")

par(mfrow = c(1,2))

plot(fpct.mds$points[,1], fpct.mds$points[,2], pch = 21, bg = "gold")
text(fpct.Mmds$points[,1]+0.02, fpct.Mmds$points[,2]+0.02, row.names(fpct.mds$points))

plot(fzcr_pca$sites[,1], fzcr_pca$sites[,2], pch = 21, bg = "darkgreen")
text(fzcr_pca$sites[,1]+0.05, fzcr_pca$sites[,2]+0.05, row.names(fzcr_pca$sites))

par(mfrow = c(1,1))

#Compare PCA and NMDS

plot(0, 0,
     pch = NA,
     xlim = c(min(fzcr_pca$sites[,1]),max(fzcr_pca$sites[,1])),
     ylim = c(min(fzcr_pca$sites[,2]),max(fzcr_pca$sites[,2])),
     xlab = "PC1/MDS1",
     ylab = "PC2/MDS2"
)

for(i in 1:nrow(fake_pca$sites)){
  lines(c(fpct.mds$points[i,1], fzcr_pca$sites[i,1]),
        c(fpct.mds$points[i,2], fzcr_pca$sites[i,2]),
        lty = 3)
}

points(fpct.Mmds$points[,1], fpct.Mmds$points[,2], pch = 21, bg = "gold")
points(fzcr_pca$sites[,1], fzcr_pca$sites[,2], pch = 21, bg = "darkgreen")

#Another way to look at the differences.

plot(fzcr_pca$sites[,1], row.names(fzcr_pca$sites), type = "o", pch = 21, bg = "darkgreen")
points(fpct.Mmds$points[,1], row.names(fpct.Mmds$points), type = "o", pch = 21, bg = "gold")

#Other things to do with a dissimilarity matrix, using chclust from rioja.

fake_clust <- chclust(fpct.dis)

plot(fake_clust, horiz = TRUE, hang = -1)

#Procrustes rotation between MDS runs and NMDS and z-score PCA ordination.

md_pro <- procrustes(fpct.Mmds, fpct.mds)

plot(md_pro)

zd_pro <- procrustes(fpct.Mmds, fzcr.pca)

plot(zd_pro)

plot(zd_pro, kind = 2)












