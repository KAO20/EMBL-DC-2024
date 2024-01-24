library(ggplot2)
library(dplyr)

# ggplot is layers, you can make an object and then you keep adding to it
plt <- ggplot(
  data = surveys_complete,
  mapping = aes(x = weight , y = hindfoot_length)
)
plt
str(plt) # str is structure 

plt + 
  geom_point()


plt + 
  geom_point()+
  ggtitle("My First Plot")

#---------------------------------- Making a plot 
plt <- ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length))+
         geom_point()
plt +
  ggtitle("Weight vs Hindfoot length")


# making a plot where the dense areas are coloured by density split into hexagonal bins
install.packages("hexbin")
library(hexbin)
ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length))+
  geom_hex()

# generate basic scatter plot 
ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length))+
  geom_point(alpha = 0.1, colour = "blue") #alpha controls transparency 


ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length))+
  geom_point(alpha = 0.4, aes(colour = species_id)) 



ggplot(
  data = surveys_complete,
  mapping = aes(x = weight, y = hindfoot_length, colour = species_id)+
    geom_point(alpha = 0.25)
)


#challenge
#scatterplot weight vs species_id colour by plot type

ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight, colour = plot_type ))+
  geom_point()

ggplot(data = surveys_complete, 
       mapping = aes
       (x = species_id, 
         y = weight, 
         colour = plot_type ))+
  geom_point()

# going to try a box plot

ggplot(data = surveys_complete, 
       mapping = aes
       (x = species_id, 
         y = weight, 
         ))+
  geom_boxplot()
# box plot gives you lower 25th quartile, median and the upper 75th quartile.


# can overlay two different things 
ggplot(data = surveys_complete, 
       mapping = aes
       (x = species_id, 
         y = weight, 
       ))+
  geom_boxplot(outlier.shape = NA) + # set box plot to not display the outliers. 
  geom_jitter(alpha = 0.3, colour = "salmon") #jittering adding a little value for each X co-ord. 


# can overlay two different things - swith the order of the points
ggplot(data = surveys_complete, 
       mapping = aes
       (x = species_id, 
         y = weight, 
       ))+
  geom_jitter(alpha = 0.3, colour = "salmon")+
  geom_boxplot(outlier.shape = NA, fill = NA)+
  theme_bw()

#challenge to make a violin plot
ggplot(data = surveys_complete, 
       mapping = aes
       (x = species_id, 
         y = weight,
       ))+
  geom_violin(trim = FALSE)+
  scale_fill_brewer(palette="Blues")+
  theme_bw()

# this looks bad so we add scale layer to log the weight values
ggplot(data = surveys_complete, 
       mapping = aes
       (x = species_id, 
         y = weight,
       ))+
  geom_violin(trim = FALSE)+
  scale_y_log10()+
  ylab("Weight (log10)")+
  scale_fill_brewer(palette="Blues")+
  theme_bw()

# challenge - create a box plot + jittered scatterplot or hindfoot_length by species_id,
#the box plot needs to be infront of the dots and filled with white

ggplot(data = surveys_complete, 
       mapping = aes
       (x = species_id, 
        y = hindfoot_length
       ))+
  geom_jitter(alpha = 0.3, aes(colour = factor(plot_id))) + #colour = "purple")+
  geom_boxplot(outlier.shape = NA, fill = "white")+
  theme_bw()


# to count how many genera are in each year 
 yearly_count <- surveys_complete %>% 
   count(year, genus)

 ggplot(data = yearly_count, mapping = aes(x = year, y = n) + geom_line())

 
 ggplot(data = yearly_count, 
        mapping = aes(x=year, 
                      y=n, 
                      colour = genus))+ # can use group = genus, this will group the values but not colour them
   geom_line()+
   theme_bw()+
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
 
 
# can use 6 shapes in points. 
 
 yearly_count %>% 
   ggplot(mapping = aes(x=year, y =n, colour = genus)) +
   geom_line()

 yearly_count_graph <- surveys_complete %>% 
   count(year, genus) %>% 
   ggplot(mapping = aes(x = year, y=n, colour = genus))+
   geom_line()
 
 # facetting & facet_wrap - making a matrix of plots

surveys_complete %>% 
  count(year, genus, sex)
 
surveys_complete %>% 
  count(year, genus, sex) %>% 
 ggplot(data = yearly_count, 
        mapping = 
          aes(x= year, 
              y = n,
              colour = sex))+
   ylab("Count of species")+
   xlab("Years")+
   geom_line()+
   facet_wrap(facets = vars(genus))  


surveys_complete %>% 
  count(year, genus, sex) %>% 
  ggplot(mapping = 
           aes(x= year, 
               y = n,
               colour = sex))+
  ylab("Count of species")+
  xlab("Years")+
  geom_line()+
  facet_wrap(facets = vars(genus))+
  theme_bw()+
  theme(panel.grid.major = element_blank())


#organise the subplots into standardised way, genuses over the columns with the
# sex over the rows, and then only one geom_line over the plot. 
surveys_complete %>% 
  count(year, genus, sex) %>% 
  ggplot(mapping = 
           aes(x= year, 
               y = n,
               colour = sex))+
  ylab("Count of species")+
  xlab("Years")+
  geom_line()+
  facet_grid(
    rows = vars(sex),
    cols = vars(genus)
  )

# this will facet but use colour rather than 
surveys_complete %>% 
  count(year, genus, sex) %>% 
  ggplot(mapping = 
           aes(x= year, 
               y = n,
               colour = sex))+
  ylab("Count of species")+
  xlab("Years")+
  geom_line()+
  facet_grid(
    cols = vars(genus)
  )

# Themes 

plot <- surveys_complete %>% 
  count(year, genus, sex) %>% 
  ggplot(mapping = 
           aes(x= year, 
               y = n,
               colour = sex))+
  geom_line()+
  facet_wrap(facets = vars(genus))+ # can use scales = "free_y"/"free_x", "free"
  theme_bw(base_size = 14)+
  scale_color_manual(values= c("goldenrod2", "deepskyblue"),
                     labels = c("Female", "Male"),
                     name = "Sex")+

  ylab("Number of individuals")+
  xlab("Year of Observation")+
  ggtitle("Observed genera over time")+
  theme(
    legend.position = "bottom", #puts the legend at the bottom, change to "none" to remove
    aspect.ratio = 1, # force the plot to be a square
    axis.text.x = element_text(angle = 45, #rotate x axis labels 45
                               hjust = 1,
                               vjust = 1), # horizontally justify them so they dont go behind the plot
    panel.grid = element_blank(), # remove the grid behind the plot
    plot.title = element_text(hjust = 0.5),
    strip.background = element_blank())
 #  annotate(text(), x = 40, y = 50 , "some text")
  # use ggpubr to add stats over the top, it works with ggplot. 
plot

ggsave(filename = "data/plot.pdf",
       plot = plot,
       width = 10,
       height = 10,
       units = "in")

