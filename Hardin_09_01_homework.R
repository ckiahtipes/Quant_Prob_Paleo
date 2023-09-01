x <- c(2,2,2,3,3,4,4,5,5,6,6,6)
y <- c(4,5,6,4,6,4,6,4,6,4,5,6)

#pdf(file = "C:/Users/alize/Box/Coursework/Fall 2023/Quantitative Paleoecology/Hardin_09_01_homework_2.pdf", #This caused an error for me because we have different file paths! CAK

pdf(file = paste0(getwd(),'/Hardin_multiplotA.pdf'), #Modifying this call - if you use the getwd() function, it will work on any machine. CAK
    # The directory you want to save the file in
    width = 7, # The width of the plot in inches
    height = 7) # The height of the plot in inches
par(mfrow = c(1, 2)) #You create multiple plots in one window using par(mfrow = c(nrow, ncol)) CAK
plot(x,y, ylim = c(0, 10), xlim = c(0,10),
     col = c("goldenrod", "blueviolet", "orchid", "skyblue", "limegreen"),
     pch = 18, cex = 2.5, main = "Hardin Homework 1", xlab = "x-axis", ylab = "y-axis")

#Inserting dino code to make second plot.
draw_me = list(x = c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 7.5, 7.5, 7.25, 7, 6.75, #We open the list() function here...
                     6.5, 6.25, 6.12, 6.25, 6.5, 6.75, 7, 7.25, 7.25, 7, 6.5, 6, 5.75, 5.8, 6, 6.4, 6.1, 6.4, 5.8, 5.5,
                     5.25, 5, 4.5, 4, 3.8, 3.7, 3.5, 4, 3.6, 4, 3.5, 3.3, 3.1, 3, 2.9, 2.8, 3, 2.8, 2.5, 2,
                     1.5, 1, 0.5), #Note that we end with a comma here because we have to enter the y object still...
               
               y = c(7, 6.8, 6.6, 6.5, 6.4, 6.5, 6.6, 6.7, 6.8, 7, 7.5, 8, 8.2, 8.2, 8, 7.5, 7, 6.75, 7, 6.75,
                     7, 6.9, 6.7, 6.5, 6.75, 6.5, 6.75, 6.5, 6.5, 6.3, 6.3, 6.2, 6, 5.75, 5.5, 5.5, 5.25, 5, 5.25, 5.5,
                     5.8, 5.6, 5.4, 5.4, 5, 4.5, 4, 4.1, 3.8, 3.6, 3.5, 3, 3.2, 3.4, 3.5, 4, 4.5, 5, 5.5, 6,
                     6.2, 6.5, 7)) #Here we add the last parentheses to close out the list function call above.


plot(draw_me, 
     type = "l",
     xlim = c(0,10),
     ylim = c(0,10))

par(mfrow = c(1, 1)) #Set par settings back to default of one plot per window. CAK
dev.off()

#Example making four plots

pdf(file = paste0(getwd(),'/Hardin_multiplotB.pdf'), #Modifying this call - if you use the getwd() function, it will work on any machine. CAK
    # The directory you want to save the file in
    width = 7, # The width of the plot in inches
    height = 7) # The height of the plot in inches
par(mfrow = c(2, 2)) #This time we make four plot windows, two rows and two columns. CAK
plot(x,y, ylim = c(0, 10), xlim = c(0,10),
     col = c("goldenrod", "blueviolet", "orchid", "skyblue", "limegreen"),
     pch = 18, cex = 2.5, main = "Hardin Homework 1", xlab = "x-axis", ylab = "y-axis")

plot(1:10, 1:10, main = "plot 2") #Making the additional plots easier to see. Can also let you check order.
plot(10:1, 10:1, main = "plot 3")
plot(1:10, 10:1, main = "plot 4")
par(mfrow = c(1, 1)) #Set par settings back to default of one plot per window. CAK
dev.off()