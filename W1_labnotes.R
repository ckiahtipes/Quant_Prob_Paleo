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
