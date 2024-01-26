# Principle component analysis 
library(tidyverse)

#Create 4 objects with the files, read in the files and assign them to these objects

trans_cts <- read.csv("data_rnaseq/counts_transformed.csv")
sample_info <- read.csv("data_rnaseq/sample_info.csv")

#need to re-structure the data because PCA does not work well with dataframes 
# so we need to convert our data into the matrix format. Then transpose the matrix
#   a matrix can only have one type of data - normally numerical. 

pca_matrix <-  trans_cts %>% 
  #this means the column name gene should be used as a row name
  column_to_rownames("gene")  %>% 
  #converts the dataframe to a matrix
  as.matrix() %>% 
  #transposes the matrix take X and Y and swap them.
  t()


# PCA function is built into R
sample_pca <- prcomp(pca_matrix)

str(sample_pca) # scale has logic, characters of PCs, or gene names
class(sample_pca) #show you that its a prcomp class specific to this function
print(sample_pca) # this shows you there are 36 principal components
tail(sample_pca) 
head(sample_pca)
summary(sample_pca) # it will show you that the standard deviation is decreasing as
# the number of Principal components are increasing, this highlights how it is choosing
# the component that is contributing the most variance. 
# The proportion of variance should equal 1 (which is 100%)
# 
# 
#  The matrix a 2D object we can specify what we want by index 

pca_matrix[1:10, 1:5] # a way to look at the pca_matrix 

#convert the pca_matrix back to a tibble
as_tibble(pca_matrix, rownames="sample") #to make sure we do not lose information from row to column

# getting the eigen values - find out what these are? 
pc_eigenvalues <- sample_pca$sdev^2  

#to get any data out of it we need to convert this to a tibble so we can plot it
pc_eigenvalues <- tibble(PC = factor(1:length(pc_eigenvalues)), 
                         variance = pc_eigenvalues) %>% 
  #add a column of % vairance
  mutate(pct = variance/sum(variance)*100) %>% # % per component
  #making a column for the 
  mutate (pct_cum = cumsum(pct))


#plotting - pareto plot/chart - combination of cumulative line and bar plot Need 2 layers min
# what components are contributing the most? This plot answers that question 
pc_eigenvalues %>% 
  ggplot(aes(x = PC)) +
  geom_col(aes(y = pct)) +
  geom_line(aes(y = pct_cum, group = 1)) +
  geom_point(aes(y = pct_cum)) +
  # frequent question whuch components are responsible for X % of variance
  geom_hline(yintercept = 90, colour = "brown")+ 
  labs(x = "Principal Component", y = "Fraction of variance explained")
  

# visualise the principal components
#   create principal component values/scores

pc_scores <- sample_pca$x
 # to visualise this we need to transform to a tibble
pc_scores <- sample_pca$x %>% 
  as_tibble(rownames = "sample") #we want to use the components/columns as axis


#making the PCA plot

pc_scores %>% 
  ggplot(aes(x = PC1, y=PC2)) +
  geom_point()

pca_plot <- pc_scores %>% 
  full_join(sample_info, by= "sample") %>% 
  ggplot(aes(x = PC1, y=PC2, 
             colour = factor(minute),
             shape = strain)) +
  geom_point()

#having a go at looking at different PCs
pca_plot2 <- pc_scores %>% 
  full_join(sample_info, by= "sample") %>% 
  ggplot(aes(x = PC2, y=PC3, 
             colour = factor(minute),
             shape = strain)) +
  geom_point()

pca_plot3 <- pc_scores %>% 
  full_join(sample_info, by= "sample") %>% 
  ggplot(aes(x = PC1, y=PC3, 
             colour = factor(minute),
             shape = strain)) +
  geom_point()


# keeps the names of the genes from the rotation valie
pc_loadings <- sample_pca$rotation %>% 
  as_tibble(rownames = "gene")

#getting the top genes 
top_genes <- pc_loadings %>% 
  select(gene, PC1,PC2) %>% 
  pivot_longer(matches("PC"), names_to = "PC", values_to = "loading") %>% 
  group_by(PC) %>% 
  arrange(desc(abs(loading))) %>% 
  slice(1:10) %>% 
  pull(gene) %>% 
  unique()
top_genes  

# only keeping the loadings for the top 10 genes
top_loadings <- pc_loadings %>% 
  filter(gene %in%  top_genes)


# visualise how the genes look
loadings_plot <- ggplot(data = top_loadings) +
  # geom_segment is drawing the vectors of the loadings
  geom_segment(aes(x = 0, y = 0, xend=PC1, yend =PC2), # x is PC1, Y = PC2
                arrow= arrow(length = unit(0.1, "in")),
                colour = "brown") +
  # this is adding the gene name to each arrow
        geom_text(aes(x = PC1, y = PC2, label = gene), 
                           nudge_y = 0.005, size =3) +
  # normalise the scale so we can see all of the arrows
                 scale_x_continuous(expand = c(0.02, 0.02))

#the longer the arrow - the longer the vector - draw a lines from where PC1 is to PC2 is the longer the line the further apart the points,
#this gives us a direction. We can reason the contribution of the genes. 
# x and y are set to 0 because the end of the arrow is 2 numbers, to view it as a vector 
# you need to set the start of the arrow as 0. 


#---------------------------------- PCA cont. 26.01.24
# Joining 2 or more PCA plots together - pca_plot & loadings_plot

library(patchwork)

#put plots side by side
(pca_plot | loadings_plot)

#puts plots on top of each other
(pca_plot / loadings_plot)

#if we have 3 combinations of different plots, can put them together
(pca_plot | pca_plot3 |pca_plot2)/ loadings_plot + 
  plot_annotation(tag_levels = "A")


#---------------------------------- PCA cont.

#shortcuts to plots of PCA - ggfortify 
library(ggfortify)
autoplot(sample_pca) #adds % variance 

#strain as shapes, time as colour
autoplot(sample_pca, data = sample_info, 
         colour = "minute", shape = "strain")

#same as above but the minutes are changed to a factor to colour them
autoplot(sample_pca,
         data = sample_info %>% 
           mutate(minute = as.factor(minute)),
         colour = "minute",
         shape = "strain")

  # this looks sort of like a circle, starts at one point changes and ends 
  # back at the same place.

# Broom
library(broom)
tidy(sample_pca, matrix = "eigenvalues" )

# ^ this is the same as the code shortcut above 
# pc_eigenvalues <- tibble(PC = factor(1:length(pc_eigenvalues)), 
#variance = pc_eigenvalues) %>% 
#  #add a column of % vairance
#  mutate(pct = variance/sum(variance)*100) %>% # % per component
#  #making a column for the 
#  mutate (pct_cum = cumsum(pct))

tidy(sample_pca, matrix = "loadings" )
  #same as this code - pc_loadings <- sample_pca$rotation %>% 
#               as_tibble(rownames = "gene")


# we want to use the test result table.

test_result
  # wheny you do deseq function it will normalise data and give you this table
# columns:
# gene -> gene name
# basemean -> normalised expression level of the gene. (table result of deseq)
# log2foldchange -> amount of change between 2 conditions, change between 2 time points. 0mins is ref level.  
# lfcse -> standard error of the log2 fold change value
# stat -> stats value computed as log2foldchange/ lfcSE then compared to standard normal distribution
# pvalue -> pvalue associated with change (how likely is it this change is there by chance)area under the curve 
# padj -> pvalue corrected for multiple hypotheses testing. Bon-ferroni correction pvalue/number of observations
# comparison -> tells us the timepoint, comparison group


# MA plot 
#   want to plot the basemean - x, and log2foldchange - y.

# Challenge to generate an MA plot. (baseMean vs Log2foldChange)
#   Oragnise the panels by comparison (time point)
#   Hint: consider log transform baseMean

test_result %>% 
  ggplot(aes(x = log10(baseMean) , y = log2FoldChange  )) + # colour = as.factor(comparison)
  geom_point(alpha = 0.1) +
  facet_wrap(facets = vars(comparison))  
  #scale_x_continuous(trans = "log2") 
  #scale_colour_manual("goldenrod", "deepskyblue2", "grey30", "purple3", "green4")+
 
# say that the gene is involved if it has a lower pvalue/ less than of 0.05, can do 0.01 
#   we can highlight these points in the plot.

test_result %>% 
  #turner operator, takes 3 arguments, test, then true statement, then other option
  mutate(sig = ifelse(padj <0.01, log2FoldChange, NA )) %>%  #test T of F, if T this will happen, if F this will happen
  ggplot(aes(x = log10(baseMean), y = log2FoldChange)) +
  geom_point(alpha = 0.1) +
  geom_point(aes(y = sig), colour = "goldenrod2", alpha = 0.4)+ 
  geom_hline(yintercept = 0, colour = "deepskyblue2") +
  facet_wrap(facets = vars(comparison)) 
  

#more significant points at time 30 mins than at other time points
#  MA plots are normally cone shaped wide to the left, this is because genes with 
#  higher base mean change less.
#  Volcano plot shows relationship between pvalue and log2foldchange, the pvalue
#   is a straight line, anything above is sig.  
#  MA plot - sig points are curved shows the expression level of the genes.
