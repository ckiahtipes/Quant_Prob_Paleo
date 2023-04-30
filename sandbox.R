#Sandbox for course development code.

#Let's do some basic stuff with the Gulf of Mexico data.

live = read.csv('data/Live.csv', header = TRUE, row.names = "X")

stations = row.names(live)

station_sums = apply(live,1,sum)

taxa_sums = apply(live,2,sum)

#Let's make a toy dataset. 10 stations, 80 possible taxa.

toy_n = 20

toy_stations = paste0("station", c(1:toy_n))
toy_catchmnt = c(rep("Hills",toy_n*0.4),
                 rep("Calah",toy_n*0.3),
                 rep("Everg",toy_n*0.3))
toy_live = c(sample(c(1:100),toy_n))
toy_G1 = c(sample(c(50:150),toy_n))

toy_data = data.frame(toy_stations,toy_catchmnt,toy_live,toy_G1)

#This makes a roughly scalable matrix-generating set of commands. Now we can explore it a little.

hist(toy_data$toy_live)

hist(toy_data$toy_G1)

boxplot(toy_data$toy_live ~ toy_data$toy_catchmnt)

toy.aov = aov(toy_data$toy_live ~ toy_data$toy_catchmnt)
summary(toy.aov)

#It's randomized, but the above will occasionally give us a significant result! Let's up the sample size and see how it behaves.