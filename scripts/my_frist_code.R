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

