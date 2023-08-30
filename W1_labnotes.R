### Lab notebook for Quantitative Problems in Paleoecology, Fall 2023

getwd() #Fetches location of R's working directory, tells me where R will look for files

#Here's my first objects.

x <- 5
y <- 6

#Setting working directory.

#Keyboard shortcut = alt + shift + h

getwd()

#R has three kinds of things. Operators, Functions, and Objects.

#Functions always have a call and argument area. It looks like "function_name()"

#One of the most important functions is concatenate, it looks like:

x = c(9,1,7,3,2,8,4,5,6,10)

#Functions for querying obejcts.

is.vector() #Use this to find out if an object is a vector. Output = logical

class() #tells me the class of an object. Output = character.

#Remember, square brackets navigate objects, parentheses are for functions.

mychr[c(7, 1, 2, 9)] #We can use concatenate to extract a non-consecutive set of values from this object.

#Let's try to read this out loud

z[t == TRUE & myint[3:8] == 8]

#WE make vectors with the vector() function.

plot(my_list,
     col = my_list$col)

#Plotting exercise: Draw a thing!



x <- c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 7.5, 7.5, 7.25, 7, 6.75,
       6.5, 6.25, 6.12, 6.25, 6.5, 6.75, 7, 7.25, 7.25, 7, 6.5, 6, 5.75, 5.8, 6, 6.4, 6.1, 6.4, 5.8, 5.5,
       5.25, 5, 4.5, 4, 3.8, 3.7, 3.5, 4, 3.6, 4, 3.5, 3.3, 3.1, 3, 2.9, 2.8, 3, 2.8, 2.5, 2,
       1.5, 1, 0.5)

y <- c(7, 6.8, 6.6, 6.5, 6.4, 6.5, 6.6, 6.7, 6.8, 7, 7.5, 8, 8.2, 8.2, 8, 7.5, 7, 6.75, 7, 6.75,
       7, 6.9, 6.7, 6.5, 6.75, 6.5, 6.75, 6.5, 6.5, 6.3, 6.3, 6.2, 6, 5.75, 5.5, 5.5, 5.25, 5, 5.25, 5.5,
       5.8, 5.6, 5.4, 5.4, 5, 4.5, 4, 4.1, 3.8, 3.6, 3.5, 3, 3.2, 3.4, 3.5, 4, 4.5, 5, 5.5, 6,
       6.2, 6.5, 7)

plot(x, y, type = "l")
text(x, y+0.1, c(1:length(y)))

