library(tidyverse)

# tibble is similar to a dataframe, but it is different because it wont convert characters to factors.
str(surveys)
      # ( data,  column names, column names etc...)
select(surveys, plot_id, species_id, weight)
# select will pull out the few columns you want to quickly see

# if there are some columns you dont want to see. 
select(surveys, -record_id, -species_id)

# we can select the rows that we are interested in
filter(surveys, year == 1995, sex == "M")
# default is an AND unless you specify and | or 

#filter both columns and rows
surveys2 <- filter(surveys, weight < 5)
surveys_sml <- select(surveys2, species_id, sex, weight)
surveys_sml


surveys_sml2 <-  select(filter(surveys, weight < 5), species_id, sex, weight)
# this will filter first, and choose rows under 5, then it will find the columns I gave it

# %>% # anything left of the pipe is used for the functions on the right
  
  surveys %>% 
  filter(weight < 5) %>% 
  select(species_id, sex, weight)
  
  
  # challenge 6 
  
  surveys %>% 
    filter(year <1995) %>% 
    select(year, sex, weight)
  
  
  
# learn mutate 
  # if I want to convert the weight in kg to lbs, and add another column 
  
  surveys %>% 
    filter(!is.na(weight)) %>% #this will remove the na values from the table
    mutate(weight_kg = weight/1000, weight_lb = weight_kg *2.2) %>% 
    head()
    view() #will show you the intermediate files 
  
    # function (name of new column = where values come from / 1000)
    
# ------------------------------------- Means 
  # going to calculate the mean of the weights by sex
    # split, apply, re-combine
surveys %>% 
  filter(!is.na(sex)) %>% #removes the NA from the column 
  group_by(sex) %>% # groups by sex
  summarise(mean_weight = mean(weight, na.rm = T)) #calculates the mean weight by sex removes NA values
  
    
surveys %>% 
  filter(!is.na(sex)) %>%
  group_by(sex) %>%
  mean(mean_weight = weight, na.rm = T)
#need to use summarise because, mean understands a numeric or logical input, 
 # whereas the summarise function can use grouped data. 

surveys %>% 
  filter(!is.na(weight)) %>% 
  filter(!is.na(sex)) %>% 
  group_by(sex, species_id) %>% 
  summarise(mean_weight = mean(weight, na.rm = T)) %>% 
head()

surveys %>% 
  filter(!is.na(weight), !is.na(sex)) %>%  #better to filter first before you then use group_by, can have multiple filters
  filter(!is.na(sex)) %>% 
  group_by(sex, species_id) %>% 
  summarise(mean_weight = mean(weight, na.rm = T), min_weight = min(weight), max_weight = max(weight), median_weight = median(weight)) %>% # if you don't filter the NAs first you need to use na.rm for all of the categtories later on 
  arrange(desc(min_weight)) %>% #default ascending order can change to descending
  print(n =15) # this will print 15 lines of the code 


count_survey <-  surveys %>% 
  count(sex, species) %>%  #using the count command 
  arrange(species, desc(n))

# Challenege 7 
# how many animals were caught in each plot_type?

surveys %>% 
  count(plot_type, taxa) 
surveys %>% 
  count(plot_type) # this was the answr it wasnt by animal 
 
  
# find the mean, median, min and max hindfoot length for each species? Add the number of observations use ?n 
surveys %>% 
  filter(!is.na(hindfoot_length), !is.na(species_id)) %>%  #better to filter first before you then use group_by, can have multiple filters
  #filter(!is.na(sex)) %>% 
  group_by(species_id) %>% 
  summarise(
    mean_hf = mean(hindfoot_length), 
    min_hf = min(hindfoot_length), 
    max_hf = max(hindfoot_length), 
    median_hf = median(hindfoot_length),
    n = n()) %>%  # the n function tells you how many observations were used to calculate these mean/median values 
  #arrange(desc(mean_hf)) %>% #default ascending order can change to descending
  print(n =15) 


#find the heaviest animal weighed in each year
 # my attempt
surveys %>%
  filter(!is.na(year), !is.na(genus), !is.na(species_id), !is.na(weight)) %>% 
  group_by(year, genus, species_id, weight) %>% 
  summarise(max_weights = max(weight)) %>% 
  select(year, genus, species_id, max_weights) %>% 
  arrange(desc(year))

# solution
surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(year) %>% 
  filter(weight == max(weight)) %>% #this will only keep the maximum weights
  select(year, genus, species_id, weight) %>% 
  arrange(year) %>% 
  unique() # this will remove the duplicates 
  
  
