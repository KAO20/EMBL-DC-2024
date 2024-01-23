#data.frames
#load data from a spreadsheet 

#downloading data from somewhere - DOES NOT IMPORT IT!
download.file(url = "https://ndownloader.figshare.com/files/2292169", 
              destfile = "data_raw/portal_data_joined.csv")
#dest file is a destination file, where you want R to put the data. 

#Import the file here - import library I need first. 
library(tidyverse)
surveys <-  read_csv("data_raw/portal_data_joined.csv")


#---------------------------------- Basics looking at dataframe 

head(surveys)#show us the top of the file, #tibble is a table

view(surveys) #will open the data in a new window. 

str(surveys)

dim(surveys) #this will give the dimensions of the table 
nrow(surveys) #this will give you the number of rows.
ncol(surveys) #this will give you the number of columns

tail(surveys) #this will give you the end / last data rows 

names(surveys) #this will give us the column names, because dataframe seels everything column wise

colnames(surveys) #is the same as names above
rownames(surveys) #row names

summary(surveys) #it is going to compute summary stats for each column 
  #summary will remove NAs from calculations 


#---------------------------------- Indexing and Subsetting Tables / Dataframes
surveys[1, 6] #going to 1st row, 6th column 
  #row, column 

#extract the whole first row
surveys[1, ] #leave column space blank
surveys[ , 1]

#extrac multiple values
surveys[c(1,2,3), c(5,6)] 
surveys[1:3, 5:6] 

#can use the - sign to remove a column to view 
surveys[ , -1]

surveys[ , "sex"]
surveys["sex"] #because it reasons column wise it will still give the same result - give subset as a tibble
surveys$plot_id #the answer you get back is a vector if you use the $ sign - give subset as vector 


# Challenge 4 ----------
surveys_200 <- surveys[ 200, ]
         # rows ,  columns
surveys[ 34786, ]
surveys[nrow(surveys), ] #start inside the parathenthesis so it would find last number first. 
tail(surveys)

survey_last <- nrow(surveys)

surveys[nrow(surveys)/2, ] 


# List practice 
my_list <- list(names = c("nora", " lisanna", "fran"), money = c(1,4,6,8,9,12,34))

str(my_list)
#access an element in the list
my_list[[1]]
my_list$names


surveys[[3]] #using terms for list will give you a vector. a list has 1D but is very flexible.
