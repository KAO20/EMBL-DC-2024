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

pc_scores %>% 
  full_join(sample_info, by= "sample") %>% 
  ggplot(aes(x = PC1, y=PC2, 
             colour = factor(minute),
             shape = strain)) +
  geom_point()

#having a go at looking at different PCs
pc_scores %>% 
  full_join(sample_info, by= "sample") %>% 
  ggplot(aes(x = PC2, y=PC4, 
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
ggplot(data = top_loadings) +
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