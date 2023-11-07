#Mapping Bantu script

#Libraries

library(raster)
library(terra)
library(lipdR)
library(neotoma2)
library(maps)

#Start with locations of cores, extract climate and vegetation data.

#1) Needs a list of records from Neotoma.
#2) Needs a list of records from APD.

#Extract chronological data from each record.

#Pull and extract spatial data.

tavg.files=list.files("worldclim/wc2.1_30s_tavg/",".tif",full.names=TRUE)
tavg=stack(tavg.files)

prec.files=list.files("worldclim/wc2.1_30s_prec/",".tif",full.names=TRUE)
prec=stack(prec.files)

month=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

names(tavg)=month
names(prec)=month

###Extracts the data based on lat and long.
#Making average temp data frame

#tavg.data=extract(tavg,sample_latlong)
#tavg.data=as.data.frame(tavg.data)
#row.names(tavg.data)=sites_used
#
##Making precip data frame
#
#prec.data=extract(prec,sample_latlong)
#prec.data=as.data.frame(prec.data)
#row.names(prec.data)=sites_used

#Basic mapping.

###MAPPING SETUP

#LAT_RANGE=c(-10,15)
#LON_RANGE=c(-5,30)

LAT_RANGE=c(20,30)
LON_RANGE=c(-90,-80)


tempcol=colorRampPalette(c("purple","blue","skyblue","green","lightgreen","yellow","orange","red","darkred")) #This is a cool means of constructing gradient colors

#map_LAT=c(-25,25) #Defining a different mapped area from the latitude/longitude selection of the taxa
#map_LON=c(-20,40) #Defining a different mapped area from the latitude/longitude selection of the taxa

map_LAT=c(15,35) #Defining a different mapped area from the latitude/longitude selection of the taxa
map_LON=c(-100,-70) #Defining a different mapped area from the latitude/longitude selection of the taxa

plot_months=c("Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov")

old.mar=par(mar=c(5, 4, 4, 2) + 0.1)
old.par=par(mfrow=c(1,1))

plot(0,0,xlim=LON_RANGE,ylim=LAT_RANGE,xlab="Longitude",ylab="Latitude",pch=NA) #Solving plotting problems by making an empty plot with defined boundaries
plot(tavg$Jan,col=tempcol(100),xlim=map_LON,ylim=map_LAT,add=TRUE) #Draw map within predefined area up to the area limits.
map("world",add=TRUE,xlim=map_LON) #Can also control the drawing here, tends to bleed over. 
title(main="January temperatures, WorldClim 2.1")





