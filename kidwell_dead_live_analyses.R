live <- read.csv("GOM_data/live_table.csv", header = TRUE, row.names = "X")
dead <- read.csv("GOM_data/dead_table.csv", header = TRUE, row.names = "X")

live <- as.matrix(t(live))
dead <- as.matrix(t(dead))

#STARTING WITH LIVE
#editing table to remove names for apply function and insert them as row names instead

live_count <- as.numeric(apply(live, 2, sum))

#live <- cbind(live, live_count)
#live_adj <- live[live$live_count >= 20, ]


#DEAD NEXT 
dead_count <- as.numeric(apply(dead, 2, sum))

#dead<- cbind(dead, dead_count)
#dead_adj <- dead[dead$dead_count >= 20, ]

#summing all species counts across stations 

#CONDUCT LIVE DEAD AGREEMENT BY SITE

station_names <- colnames(live)


#CONDUCT LIVE DEAD AGREEMENT BY SPECIES? 
#possibly by site, unsure what species would yield 

live_dead <- data.frame(live_count, dead_count, row.names = station_names)
live_dead_20 <- live_dead[live_dead$live_count >= 20 & live_dead$dead_count >=20, ]
