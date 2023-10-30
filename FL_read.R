###Basic Neotoma read

#install.packages("neotoma2")

library(neotoma2)

FL_ndbread <- read.csv("neotoma_searches/FL_holocene.csv", header = TRUE)

FL_sites <- get_sites(FL_sites$siteid)

FL_datasets <- get_downloads(FL_sites, all_data = FALSE)

FL_samples <- samples(FL_datasets)

FL_samples[FL_samples$element == "pollen",] #This shows you pollen samples.

unique(FL_samples$element)
