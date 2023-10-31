###Script for reading GoM data and organizing.

#Goals:

#1) Read .csv of original data.
#1a) Fix station data to be numeric.
#2) Organize data as tables where stations are columns, taxa are rows.
#2a) We will make a table for each relevant taphonomic grade (Live, Dead = G1+G2+G3)

#Read station data.

stations <- read.csv("GoM_data/stations.csv", header = TRUE, row.names = "Sites")

#check data

stations$I.A.T

#Let's transpose, 
#fix the letters in the Lat/Long problem
#and correct the longitude (make negative).

stations <- as.data.frame(t(stations))

stations$`Lat DD` = as.numeric(gsub("N", "", stations$`Lat DD`))
stations$`Long DD` =  as.numeric(gsub("W", "", stations$`Long DD`))
stations$`Long DD` = stations$`Long DD`*-1

#Quick plot of station locations.

plot(stations$`Long DD`, stations$`Lat DD`)

#Let's save the station data.

write.csv(stations, "GoM_data/stations_fixed.csv")

#Let's read the taxa and extract data by taphonomic group.

all_counts <- read.csv("GoM_data/all_taxa_grades.csv", header = TRUE)

#Pull column names, will use to make a list of station names.

all_columns <- colnames(all_counts)

#We use seq() to grab every 7th column (starting at 2 to skip taxa).

station_names <- all_columns[seq(from = 2, to = length(all_columns) - 1, by = 7)]

#We extract the list of taxa to use as row names.

original_taxa <- all_counts$taxon

taxa_table <- table(original_taxa)

taxa <- unique(original_taxa)

taxa = taxa[-c(1)]

#There are duplicate taxa names! We will need to fix this later.

counts_trimmed <- all_counts[-c(1),-c(1,1612)] #This cuts first row.

#We have trimmed data that's just counts. Now we make it numeric and replace "NA"

counts_trimmed <- lapply(counts_trimmed, as.numeric)
counts_trimmed <- as.data.frame(counts_trimmed)
counts_trimmed[is.na(counts_trimmed)] = 0

stations_check <- data.frame(colnames(counts_trimmed), row.names(stations))

nrow(stations)
nrow(counts_trimmed)
ncol(counts_trimmed)

#We need to repair the doubled taxon name now. 

doubles <- names(taxa_table[taxa_table ==2])

#Find where the doubles occur in the taxa list.

double_finder <- grep(doubles, original_taxa)

#Use the locations to pull and sum those rows.

Strictispira.sp <- apply(counts_trimmed[double_finder,], 2, sum)

#Replace one of these with the sum of both.

counts_trimmed[205, ] = Strictispira.sp

#Cut the other location.

counts_trimmed = counts_trimmed[-c(85), ]

#Let's pull the live data.

live_table <- matrix(nrow = nrow(counts_trimmed), ncol = length(station_names))

live_table = counts_trimmed[ , seq(5, ncol(counts_trimmed), 7)]

#Name rows and columns.

row.names(live_table) = taxa
colnames(live_table) = station_names

#Write data.

write.csv(live_table, "GoM_data/Live_table.csv")

#Now we pull dead data, sum and do the same.

#We need a for loop to grab the dead classes, add them, and save it.

dead_table <- matrix(nrow = nrow(counts_trimmed), ncol = length(station_names))

for(i in 1:length(station_names)){
  G1 <- counts_trimmed[ , 5 + ((i-1)*7)]
  G2 <- counts_trimmed[ , 6 + ((i-1)*7)]
  G3 <- counts_trimmed[ , 7 + ((i-1)*7)]
  d_value <- G1 + G2 + G3
  dead_table[, i] = d_value
}

row.names(dead_table) = taxa
colnames(dead_table) = station_names

#Save dead table.

write.csv(dead_table, file = "GoM_data/dead_table.csv")

