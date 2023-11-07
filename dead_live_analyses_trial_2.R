live <- read.csv("live_table.csv", header = TRUE)
dead <- read.csv("dead_table.csv", header = TRUE)

#STARTING WITH LIVE
#editing table to remove names for apply function and insert them as row names instead
row.names(live) = live$X
live_test <- live[,-1]
live <- live_test

#sums calculated by station rather than species
live_test <- t(live_test)
live_test_count <- as.numeric(apply(live_test, 1, sum))
live_test <- cbind(live_test, live_test_count)

live_test_adj <- live_test[live_test$live_test_count >= 20, ]


#DEAD NEXT 
row.names(dead) = dead$X
dead_test <- dead[,-1]
dead <- dead_test

#summing all species counts across stations 
dead_count <- as.numeric(apply(dead, 1, sum))
dead<- cbind(dead, dead_count)

dead_adj <- dead[dead$dead_count >= 20, ]