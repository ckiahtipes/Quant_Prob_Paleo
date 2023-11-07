#Neotoma Reads on Holocene Records in FL

library(neotoma2)
library(rcarbon)

#Use Neotoma call to pull FL sites.

#FL_sites <- get_sites(gpid = "Florida", ageold = 20000)

FL_read <- read.csv("neotoma_searches/FL_search.csv", header = TRUE)

sites_call <- get_sites(FL_read$siteid)

all_collunits <- summary(sites_call)

FL_datasets <- get_datasets(sites_call, all_data = TRUE)

FL_download <- get_downloads(FL_datasets, all_data = TRUE)

#Okay, let's get some FL chroncontrols.

FL_chron <- chroncontrols(FL_download)

nrow(FL_chron)

#This works!
#Just the dates, ma'am. 

FL_radiocarbon <- FL_chron[FL_chron$chroncontroltype == "Radiocarbon" & is.na(FL_chron$chroncontroltype) == FALSE, ]
FL_select <- FL_radiocarbon[FL_radiocarbon$chroncontrolage < 11700, ]

#Now we use rcarbon package to make a SPD plot.

FL.caldates <- calibrate(x=FL_select$chroncontrolage, 
                         errors = FL_select$agelimitolder-FL_select$agelimityounger, calCurves = 'intcal20')
FL.spd <- spd(FL.caldates, timeRange = c(11700,100))

plot(FL.spd)

#Binning data

FL.bins <- binPrep(sites=FL_select$siteid, ages = FL_select$chroncontrolage, h = 100)

FL.spd.bins <- spd(FL.caldates, bins=FL.bins, timeRange = c(11700,100))

#Kernel density

FL.randates <- sampleDates(FL.caldates, bins=FL.bins, nsim = 500, verbose = FALSE)

FL.ckde <- ckde(FL.randates, timeRange = c(11700,100), bw=200)

plot(FL.ckde)

#This is a basic method that works.

#Next step may be to derive SPDs from latitudinal or rainfall bins?




