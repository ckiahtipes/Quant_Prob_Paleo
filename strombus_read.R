strombus <- read.csv("week9_data/strombus_alatus.csv", header = TRUE)

station_names <- unique(test$Raw.Site)

strombus$Lat.DD <- as.numeric(gsub("N", "", strombus$Lat.DD)) #Fix Lat
strombus$Long.DD <- as.numeric(gsub("W", "", strombus$Long.DD)) #Fix Long

grade_array <- lapply(1:7,function(x) { #We use a custom function to make 7 tables within this object.
  matrix(nrow = 230, ncol = 7)
})

for(i in 1:length(grade_array)){ #We take the object length and use it to pull the subsets of the data.
  new_table <- strombus[seq(i, nrow(strombus), 7), ]
  grade_array[[i]] <- new_table
}

array_names <- list("T", "W", "LR", "Live", "G1", "G2", "G3") #Create names.
grade_array <- as.array(grade_array) #Make object an array.
names(grade_array) <- array_names #Name the tables in array.
  
strombus_dead <- data.frame(grade_array$G1$Strombus_alatus, grade_array$G2$Strombus_alatus, grade_array$G3$Strombus_alatus)
