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



#---------------------------------- Factors
str(surveys)
#some classes are categories they can only be a certain set of values not anything.
# for example the 'species' 
# once we create a factor It can only contain a pre-defined set of values.
#R stores factors in an efficient way - it will assume there are only these set levels,
# it will transform categories as numbers e.g. cat = 1, dog = 3, rat = 4 etc... 

surveys$sex <- factor(surveys$sex)

levels(surveys$sex) #tell you what the levels are
nlevels(surveys$sex) #tell you the number of levels. 

# ---- Force a factor to make levels how I want
sex <- factor(c("male", "female", "female", "male"))
sex <- factor(sex, levels = c("male", "female"))


# Challenge 5 ----------

surveys$taxa <- factor(surveys$taxa)
surveys$genus <- factor(surveys$genus)

# rabbit genus - Oryctolagus
sum(surveys$taxa == "Rabbit" ) #give me the number of rabbits = 75 
summary(surveys$taxa) #this will give you the information of all the taxa 

nlevels(surveys$genus)
#26 different genera in the table

levels(surveys$taxa) #rabbit is in taxa stored as "Rabbit"

# convert the factor to a character
as.character(sex)


year_fct <- factor(c(1990, 1983, 1977, 1997))
as.numeric(year_fct) #this will convert from a factor to a numeric value 
# but R will give you its internal coding of the values not the actual numbers. 


#quick way to get the years back from factors. You can convert it to a character.
as.numeric(as.character(year_fct)) # 1st years will be characters, 
  #2nd because they are numbers, R can then also convert back to numbers. 

#proper way to convert back to numbers
as.numeric((levels(year_fct))[year_fct]) 
 #middle bracket obtains the levels, extracts 
 # then convert the levels to numeric with 1st brackets - numeric version of the levels, but this is not the vector
 # then use the indexing [], year_fct with encoded levels, to organise the numbers as they were originally in the data.

#re naming factors
plot(surveys$sex) # R did not plot NAs first 
summary(surveys$sex) 
sex <-  surveys$sex
levels(sex)
sex <- addNA(sex) # this will add the NAs back in 
levels(sex) # now we see 3 labels 

levels(sex)[3] <- "undetermined"
levels(sex)
sex

plot(sex)


#seeing if R will plot a vector not a factor like sex above
plot(c("a", "a", "b", "b", "a"))
# it cant plot this as it cannot count the occurances of characters
# can force it to make it a factor 
plot(factor(c("a", "a", "b", "b", "a")))


# changing the names of the levels in the factor
levels(sex)[1:2] <- c("female", "male")     
plot(sex)     

sex <- factor(sex, levels = c("undetermined", "female", "male"))
plot(sex)
