library(tidyverse)
library(ggplot2)
library(dplyr)

# Create a "data" directory
dir.create("data_rnaseq")

# Download the data provided by your collaborator
# using a for loop to automate this step
for(i in c("counts_raw.csv", "counts_transformed.csv", "sample_info.csv", "test_result.csv")){
  download.file(
    url = paste0("https://github.com/tavareshugo/data-carpentry-rnaseq/blob/master/data/", i, "?raw=true"),
    destfile = paste0("data_rnaseq/", i)
  )
}


#Create 4 objects with the files, read in the files and assign them to these objects

raw_cts <- read.csv("data_rnaseq/counts_raw.csv")
trans_cts <- read.csv("data_rnaseq/counts_transformed.csv")
sample_info <- read.csv("data_rnaseq/sample_info.csv")
test_result <- read.csv("data_rnaseq/test_result.csv")

#create a plot that will show the distribution of the counts across the sameples. 
  #need to convert the transformed counts table from a wide table to a long table. 

trans_cts_long <-  trans_cts %>% 
  pivot_longer(names_to = "sample",
               values_to = "cts",
               cols = wt_0_r1:mut_180_r3) #could use the column indexes here - better to use column names

#we want to make a plot that splits the samples based on WT/mutant, and timepoint. 
#   we do not have this information in the table but it is in the sample info file. 
#     so we can join the 2 tables as they have the same column called sample. 

trans_cts_long <- full_join(trans_cts_long, sample_info, by = "sample")
# function ( table 1, table 2, column to join by = )


# making the hisotgram - distribution of the counts data

trans_cts_long %>% 
  ggplot(aes(x = cts))+
  geom_freqpoly() 
#hisotgram shows how many times a value on the x axis appears. 
# the Freq poly shows this by the line

trans_cts_long %>% 
  ggplot(aes(x = cts, colour = replicate))+
  geom_freqpoly(binwidth = 1) +
  facet_grid(rows = vars(strain) , cols = vars(minute))
  # bin width - default is the groups between the x values i.e. binwidth = 1
#  this means if you look at cts 5, that value is somewhere between 4 and 5 cts 

# Challenge - make the same plot as above but with the raw counts

#make the raw counts table long
raw_cts_long <-  raw_cts %>% 
  pivot_longer(names_to = "sample",
               values_to = "cts",
               cols = wt_0_r1:mut_180_r3)

#make the joined table 
raw_cts_long <- full_join(raw_cts_long, sample_info, by = "sample")

# make the histogram plot
raw_cts_long %>% 
  ggplot(aes(x = cts, colour = replicate))+
  geom_freqpoly() +
  scale_x_log10()+
  facet_grid(rows = vars(strain) , cols = vars(minute))

#logging the values inside ggplot
raw_cts_long %>% 
  ggplot( aes(x = log10(cts), colour = replicate)) +
  geom_freqpoly() +
  facet_grid(rows = vars(strain), cols = vars(minute))
# warning message: Removed 645 rows containing non-finite values (`stat_bin()`). 
#   this means: that wherever it found a 0, it could not plot log10(0) as -Infinity
#     it then removed the rows that had 0, because they contained infinite values
#       it will not plot these. 
#         When NAs are the problem the warning will say "remove X rows with missing values"

log10(0)

# we can force R to still plot the 0 values by changing them to 1, this is because log10(1) is zero.
#   we can arbitrarily add 1 to the whole column because the other values are so large that adding 1 and then 
#     doing log10 will not affect the result we see but will fix the  zeros, for e.g. 10,000,001.

#logging the values inside ggplot and adding 1 to fix the zeros.
raw_cts_long %>% 
  ggplot( aes(x = log10(cts + 1), colour = replicate)) +
  geom_freqpoly(binwidth =1 ) +
  facet_grid(rows = vars(strain), cols = vars(minute))

#when we specify binwidth of 1, the bins have a difference of 1, but log transformed 
# is now 10, 100, 1000, width. 

#instead of the frquency polygon we can to make a box plot. 
raw_cts_long %>% 
  ggplot(aes(x = factor(minute)  , y = log10(cts + 1), fill=replicate)) +
  geom_boxplot()+
  facet_grid(cols = vars(strain))
  
  
  
  
  
  
  
trans_cts_long %>% 
  ggplot(aes(x = factor(minute)  , y = log10(cts + 1), fill= factor(minute))) +
  geom_boxplot()+
  facet_grid(cols = vars(replicate), rows = vars(strain))+
  ggtitle("Counts Transformed RNASeq Yeast")+
  theme(
    legend.position = "bottom", #puts the legend at the bottom, change to "none" to remove
    aspect.ratio = 1, # force the plot to be a square
    axis.text.x = element_text(angle = 45, #rotate x axis labels 45
                               hjust = 1,
                               vjust = 1), # horizontally justify them so they dont go behind the plot
    panel.grid = element_blank(), # remove the grid behind the plot
    plot.title = element_text(hjust = 0.5),
    strip.background = element_blank())
  
  
#Correlation between wild type sample at T0 mins and T30 mins
trans_cts
trans_cts_long

# going to use the wide table because the axis we want with one replicate they are the column names

trans_cts %>% 
  ggplot(aes(x = wt_0_r1, y = wt_30_r1)) +
  geom_point()+
  geom_abline(colour = "brown") + #this adds the line of best fit 
  ggtitle("Correlation between WT at 0 and 30 mins")+
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# make a different correlation between replicates - good correlation points are close together

trans_cts %>% 
  ggplot(aes(x = wt_0_r1, y = wt_0_r2)) +
  geom_point()+
  geom_abline(colour = "brown") + #this adds the line of best fit 
  ggtitle("Correlation between WT replicates 0 and 30 mins")+
  theme(
    plot.title = element_text(hjust = 0.5)
  )

#showing all correlations between all samples in one plot
# To look at the correlation of count data across all samples in our experiment
# 
#     we want to remove the gene names, because we can only do calculations with numeric values
#     function that calcs correlation between all possible values
trans_cts_corr <- trans_cts %>% 
  select(-gene) %>% 
  cor(method = "spearman") # if data is not normal you use spearman, pearson for normal
#corr has made an output that is a matrix, this is a base R function. 
# It contains the pairwise correlation results between all pairs 36 rows, 36 columns, with
#   spearman co-efficient 

#Going to make a heatmap of the spearman co-efficient values
# load the package first
library(corrr)

rplot(trans_cts_corr)
#size of the bubbles the more positive the bigger the bubble.

rplot(trans_cts_corr)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
