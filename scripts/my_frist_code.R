#need this for the training week
install.packages(c("tidyverse", "patchwork", "hexbin", "corrplot", "corrr", "broom", "ggfortify", "BiocManager"))
#followed by
BiocManager::install("ComplexHeatmap")

library(ComplexHeatmap)

3 + 4

weight_kg <- 55


my_weight_kg <- 50
(my_weight_kg <- 50)  
my_weight_kg

2.2 * my_weight_kg

weight_kg <- 57
2.2 * weight_kg

weight_lbs <- 2.2*weight_kg
#I have to update the value of the weight_kg object. 
weight_kg <- 100 

#use tab/enter to autocpmplete values. 

#challenge number 1
mass <- 47.5
age <- 122
mass <- mass*2
age <- age -20
mass_index <- mass/age
mass_index


weight_kg <- sqrt(9)
#not ervey function will return a value, sqrt only takes one argument.
#A function could also return more than numbers. 

round(4.678)
ceiling(4.678) #this will round up
floor(4.678) #this will essentially round down. 


round(3.14159)
round(3.14159, digits = 2)
round(3.14159, 2)

args(round)


# making my own function 
my_function <- function(x){
  x <- x + 2}
myres <- my_function(3)


#-------------------- Vectors

weight_g <- c(50, 60, 65, 82) #i am defining a vector by using c. 

animals <- c("mouse", "rat", ' dog')

#gives us the length of the vector
length(animals)
length(weight_g)

#give us the type of data in the vector
class(animals)
class(weight_g)

#this function gives you the structure of the object. 
str(animals)

#add another animal (element) to the beginning of the animal vector

animals <- c("chinchilla", animals)

#add another elemnent to the end of the vector
animals <- c(animals, "frog")
animals


typeof(animals)


#challenge number 2
num_char <- c(1,2,3,"a")
num_logical <- c(1,2,3,TRUE)
char_logical <- c("a", "b", "c", TRUE)
tricky <- c(1,2,3,"4")

class(num_logical) #it changed all of the elements to numeric and made T into 1
class(num_char) #it changed all of the elements to strings
class(char_logical)
class(tricky)

num_char
num_logical
char_logical
tricky

#logical -> numeric -> character
# logical -> character 

# if its a logical it can become a number
# if its numeric it can become a character
# if it s logical it can become a character


#subsetting vectors - this means accessing different parts of the vector. 
#call the name of the vector with [] and give position
animals[3]
# R indexes starting from 1

animals[c(2,3)]

#the brackets have a vector of positions, that animals can use square brackets to find these items. 

more_animals <- animals[c(1,2,3,2,1,4)]

# animals <- c(). - i used this to empty the vector because it got out of hand 

weight_g

#can subset through a vector of logicals
weight_g[c(F, F, T, T)] #This tells it to ignore the first 2 and give me the last two options
weight_g > 63 #logical operation, seeing which values are above 63.
weight_g[weight_g > 63] #this will give me the numbers that are over 63, it will do T/F then identify the indexes that I need to keep
weight_g[weight_g> 63 & weight_g < 80] #and written as &
weight_g[weight_g < 58 | weight_g > 80] # or is written as | 
weight_g==65


animals[animals=="rat" | animals == "frog"]
# %in%  - helps us to find all elements that correspond to a vector of our choice.
animals %in% c("rat", "frog", "cat", "duck", "dog") 
animals #the answer is given in the context of the vector

animals[animals %in% c("rat", "frog", "cat", "duck", "dog")]

# R can handle missing data. R will use NA as a placeholder. This means Not Available. 

#an example of a vector with missing data
#define vector 
heights <- c(2,4,4,NA, 6) #if you have empty space in dataframe R will add NA. 
mean(heights, na.rm = T) #na.rm removes NA before the mean is computed. 
max(heights, na.rm = T)
min(heights, na.rm = T)

is.na(heights) #checks if an element is and NA value 
#the answer tells me where the NA value is. 
heights[!is.na(heights)]
na.omit(heights) #remove and NA but also tell you what was NA and which one it removed. 
complete.cases(heights) #is this a complete case or a missing case 

# Challenge 3 - 
#Using this vector of heights in inches create a new vector. 

heights <-  c(63,69,60,NA,68,61,59,64,69,63,6)
heights_no_na <- na.omit(heights)
heights_no_na <- heights[!is.na(heights)]
heights_no_na

median(heights_no_na)

median(heights, na.rm = T)

#Use R to work out how many people are taller than 67 inches. 

heights_no_na[heights_no_na > 67]
length(heights_no_na[heights_no_na > 67])
sum(heights_no_na > 67) #this also works because logicals can be transformed into numbers, therefore every True = 1, 
                  #so the SUM works becasue it can add 1+1+1. This is because sum is expecting a numeric vector. 


