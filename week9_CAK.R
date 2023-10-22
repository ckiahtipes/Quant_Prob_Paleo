### Week 9A - Ordination

library(vegan)

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

#Goals

#Read Data

#Make NMDS Ordination

#Inspire students - YOU ARE NOT ALONE


#Let's read some data.

clap <- read.csv("week9_data/Clapham.csv", header = TRUE, row.names = "taxon")

#Drop last row to clean up data.

clap <- clap[-c(nrow(clap)), ]

tail(clap)

#Let's evaluate data.

class(clap)
is.character(clap$PC)
is.character(clap$D)

clap$D <- as.numeric(gsub(",", "", clap$D))
clap$Ey <- as.numeric(  gsub(",", "", clap$Ey)                      )

c_sums <- apply(clap, 2, sum)

###FUN BUT USELESS
#bad_column_finder = vector("logical", length = ncol(clap))
#
#for(i in 1:ncol(clap)){
#  result = is.character(clap[,i])
#  bad_column_finder[i]
#}
#
#clap[, bad_column_finder] <- as.numeric(gsub(",", "", clap[ , bad_column_finder]))

#Table needs to be transposed. Ordination likes columns to be species.

clap <- as.data.frame(t(clap))

#Let's do ordination, at last.

clap.dist <- vegdist(clap)
clap.mds <- isoMDS(clap.dist)

clap.mds$stress
clap.mds$points

stressplot(clap.mds, clap.dist)

#Method for plotting MDS result

ordiplot(clap.mds, type = "t")

#Method for meta MDS, it remembers species attributes!

clap.Mmds <- metaMDS(clap)

ordiplot(clap.Mmds, type = "t")

#Make better plots

sites <- clap.Mmds$points
species <- clap.Mmds$species

#Basic plot that works here. 

plot(sites, main = "meta-MDS plot")
points(species, col = "darkred")

#Let's make arrows for the species loadings on the dimensions

plot(sites, 
     main = "meta-MDS plot", 
     pch = 21, 
     bg = "black",
     xlim = c(-2, 2),
     ylim = c(-2, 2))

#Adding text to plots.

text(species[, 1] + (species[, 1]*0.20),
     species[, 2] + (species[, 2]*0.20), 
     row.names(species),
     cex = 0.8)

#Adding arrows.

arrows(x0 = rep(0, nrow(species)),
       y0 = rep(0, nrow(species)),
       x1 = species[,1],
       y1 = species[,2],
       length = 0.1)

#Let's update with select species names.

sel_species <- species[abs(species[,1]) > 0.5 | abs(species[,2]) > 0.5, ]

#Filtering by species loading.

plot(sites, 
     main = "meta-MDS plot", 
     pch = 21, 
     bg = "black",
     xlim = c(-2, 2),
     ylim = c(-2, 2))

#Adding text to plots.

text(sel_species[, 1] + (sel_species[, 1]*0.20),
     sel_species[, 2] + (sel_species[, 2]*0.20), 
     row.names(sel_species),
     cex = 0.8)

#Adding arrows.

arrows(x0 = rep(0, nrow(sel_species)),
       y0 = rep(0, nrow(sel_species)),
       x1 = sel_species[,1],
       y1 = sel_species[,2],
       length = 0.1)

#Let's do principal components with raw counts, percents, z-scores.

site_sums <- apply(clap, 1, sum)

clap_pct <- (clap/site_sums) * 100
clap_z <- (clap - apply(clap, 1, mean))/apply(clap, 1, sd)

clap.pca <- rda(clap) #Doing PCA on unmodified couns
cpct.pca <- rda(clap_pct) #Doing PCA on percentages
clzc.pca <- rda(clap_z) #Doing PCA on z-scores

clap_pca <- summary(clap.pca) #Summarize each output to get sites/species.
cpct_pca <- summary(cpct.pca)
clzc_pca <- summary(clzc.pca)

plot(clap_pca$sites, pch = 21, bg = "black")
points(cpct_pca$sites, pch = 22, bg = "gold")
points(clzc_pca$sites, pch = 23, bg = "green")
#text(clap_pca$species, row.names(clap_pca$species))

clap_pca$cont$importance

barplot(clzc_pca$cont$importance[3,])

#Constrained cluster analysis

clap.cca <- cca(clap)

plot(clap.cca)

clap.cca$CA$u

#Diversity

#Let's just count the number of different things.

n_obs <- apply(clap, 1, sum)
n_taxa <- vector("numeric", length = nrow(clap))

for(i in 1:nrow(clap)){
  n_count <- clap[i,clap[i, ] > 0] 
  n_taxa[i] <- length(n_count)
}

#Let's resample using frequencies from our data.

all_taxa <- colnames(clap) #We need a list of names.

probs <- clap_pct/100

#Basic method for sampling our own data.
resampled_names <- sample(all_taxa, 100, replace = TRUE, prob = probs[1 , ])

times <- 200

n_things <- vector("numeric", length = times)
mx_thing <- vector("numeric", length = times)
mn_thing <- vector("numeric", length = times)
thing_matrix <- matrix(ncol = 200, nrow = 200)

for(i in 1:times){
  clap_sample <- sample(all_taxa, i, replace = TRUE, prob = probs[5, ])
  thing_matrix[1:i, ] = clap_sample
  n_things[i] <- length(unique(clap_sample))
}

n_things

plot(1:200, n_things, pch = 22, bg = heat.colors(200))

#We can use vegan and then extract at different sampling size.

clap.rare <- rarecurve(clap)
clap_array <- array(clap.rare, dimnames = list(row.names(clap)))

#Plotting example.

plot(1:200,(clap_array$Ey[1:200]),type = "l")
lines(1:200, clap_array$D[1:200])




