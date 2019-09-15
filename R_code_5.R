###-----------------------------------------###
##
## Class Four: Visualization
## For: Institute of Advanced Analytics 
## Instructor: Aaron Baker 
## Email: ambaker31991@gmail.com
##
###-----------------------------------------### 

##----------------------------##
## Setup
##----------------------------##

# Import libraries
library(data.table)
library(dplyr)
library(stringr)

# Set working directory
setwd("C:\\Users\\bakea091\\Documents\\GitHub\\code_library\\IAA_R_Class\\Lessons_Class\\Data_Inputs")

# Upload data
df_auto <- fread("Auto_MPG.csv", stringsAsFactors = FALSE)
colnames(df_auto)
dim(df_auto)
summary(df_auto)

# Fix horsepower
df_auto$Horsepower <- as.numeric(df_auto$Horsepower)

# Get Car Make variable from previous class
df_auto <- df_auto %>% 
  group_by(Car_Name) %>%
  mutate(Car_Make=str_split(Car_Name," ")[[1]][1]) 

View(df_auto)

##----------------------------##
## Base R Plotting
##----------------------------##

# Base R has a plotting built in 

#Scatter one variable
plot(x=df_auto$MPG)

#Scatter two variables
plot(x=df_auto$MPG, y=df_auto$Weight)

#Line Plot
plot(x=df_auto$MPG, type="l")

#Histogram
hist(x=df_auto$MPG)

help(hist)

hist(x=df_auto$MPG, breaks=20)

#If you want to use Base graphics, that's fine. However, I typically use the ggplot2 package 

##----------------------------##
## Plotting - ggplot2
##----------------------------##  

# ggplot2 is another "grammar" package that aims to make plotting easier and more efficient 
# it's based on building a graphic composed of different layers
# every plot starts with setting up the data, and telling what type of graph you want

#install.packages("ggplot2")
library(ggplot2)

# simple scatter plot
ggplot(df_auto, aes(x=MPG, y=Weight)) +
  geom_point()

##----------------------------##
## ggplot2 - grammar
##----------------------------##  

# note it starts with ggplot - where you define the data and variables 
# aes = aesthetics. "how variables in the data are mapped to visual properties (aesthetics) of geoms"
# the + operator adds additional layers to the graph 
# anything "geom_" is the type of graph you want

# if you just do ggplot without a geom, you end up with an empty graph:
ggplot(df_auto, aes(x=MPG))

# simple bar chart 
ggplot(df_auto, aes(x=Cylinders)) +
  geom_bar()

# note that ggplot expects a data frame object and will error if not provided
ggplot(aes(x=df_auto$Cylinders))

# if you want to plot a set of vectors, you can use a blank ggplot and provide the aes in the geom
ggplot() +
  geom_bar(aes(x=df_auto$Cylinders))


##----------------------------##
## ggplot2 - simple graphs
##----------------------------## 

# simple scatter plot
ggplot(df_auto, aes(x=MPG, y=Weight)) +
  geom_point()

# simple line plot
ggplot(df_auto, aes(x=MPG, y=Weight)) +
  geom_line()

# simple line plot by order of index
ggplot(df_auto, aes(x=c(1:nrow(df_auto)), y=Weight)) +
  geom_line()

# simple bar chart 
ggplot(df_auto, aes(x=Cylinders)) +
  geom_bar()

# if you want to re-order, treat as a factor and use the reorder function
ggplot(df_auto, aes(x=reorder(Cylinders,-Cylinders))) +
  geom_bar()

# simple histogram 
ggplot(df_auto, aes(x=MPG)) +
  geom_histogram()

# density
ggplot(df_auto, aes(x=MPG)) +
  geom_density()

# boxplot 
ggplot(df_auto, aes(x=as.factor(Cylinders), y=MPG)) +
  geom_boxplot() 

# violen plot
ggplot(df_auto, aes(x=as.factor(Cylinders), y=MPG)) +
  geom_violin() 


##----------------------------##
## ggplot2 - graph attributes
##----------------------------## 

# all of the previous examples are just the defaults, but you can add a lot of customization to the graphs 

# Add some titles to the scatter plot
ggplot(df_auto, aes(x=MPG, y=Weight)) +
  geom_point() +
  labs(title="Scatter Plot of MGP and Weight", x="Miles per Gallon (MPG)", y="Weight (lbs)") 

# Note, the general look of the graph is still kind of ugly... 
# I could customize every element, or I can use a premade theme
ggplot(df_auto, aes(x=MPG, y=Weight)) +
  geom_point() +
  labs(title="Scatter Plot of MGP and Weight", x="Miles per Gallon (MPG)", y="Weight (lbs)") +
  theme_minimal()

ggplot(df_auto, aes(x=MPG, y=Weight)) +
  geom_point() +
  labs(title="Scatter Plot of MGP and Weight", x="Miles per Gallon (MPG)", y="Weight (lbs)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill='lavenderblush'))

# We may want to layer in additional information, such coloring the points by the Car Make
ggplot(df_auto, aes(x=MPG, y=Weight)) +
  geom_point(aes(colour=Car_Make)) +
  labs(title="Scatter Plot of MGP and Weight", x="Miles per Gallon (MPG)", y="Weight (lbs)") +
  theme_minimal() +
  theme(legend.position="none")


##----------------------------##
## ggplot2 - layering geoms
##----------------------------## 

# In addition to single plots, you can stack plots and plot elements together. 
# This is done by adding more geom_ components 

# For example, here we'll add a linear trendline using the geom_smooth
ggplot(df_auto, aes(x=MPG, y=Weight)) +
  geom_point() + 
  geom_smooth(method="lm", se=FALSE) +
  geom_hline(yintercept=mean(df_auto$Weight), color='red') +
  geom_vline(xintercept=mean(df_auto$MPG), color='green') +
  labs(title="Scatter Plot of MGP and Weight", x="Miles per Gallon (MPG)", y="Weight (lbs)") +
  theme_minimal() 

# Boxplot with a jitter
ggplot(df_auto, aes(x=Cylinders, y=MPG, group=Cylinders)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_minimal()

# Histogram with Density
ggplot(df_auto, aes(x=MPG)) +
  geom_histogram(aes(y=..density..), color='blue', fill='white') +
  geom_density(aes(y=..density..), color='purple', size=1) +
  theme_minimal()


##----------------------------##
## ggplot2 - grouping
##----------------------------## 

#Some times it may be beneficial to split your plot by group
ggplot(df_auto, aes(x=MPG, group=Cylinders, fill=as.factor(Cylinders))) +
  geom_density(alpha=.4) +
  theme_minimal()


##----------------------------##
## ggplot2 - facets
##----------------------------## 

# Additionally, I might want to make a plot grid based on a discrete variable
# This is done with facetting in ggplot

ggplot(filter(df_auto, Model_Year %in% c(70:75)), aes(x=MPG, y=Weight)) +
  geom_point() +
  facet_wrap(~Model_Year, scales = "free", ncol=3) +
  theme_minimal()

ggplot(filter(df_auto), aes(x=MPG, y=Weight)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  facet_wrap(~Cylinders, scales = "free", ncol=3) +
  theme_minimal()


##----------------------------##
## ggplot2 - saving plots
##----------------------------## 

# If I want to save this plot
ggsave("Grid_Plot.png", 
       plot=last_plot(), 
       width = 10, height = 8, dpi = 300, units = "in", 
       device='png'
)

# I can also assign the plot to a variable
p <- ggplot(df_auto, aes(x=MPG, y=Weight)) + 
  geom_point()

class(p)

#Once assigned to a variable, you continue to add additional elements 
p <- p + theme_minimal()


##----------------------------##
## Plotting - plotly
##----------------------------##  

# plotly is another visualization package that extends R to create interactive visualizations

#install.packages("plotly")
library(plotly)

# Basic 2D scatter plot
plot_ly(data=df_auto, x=~Weight, y=~MPG, type="scatter", mode="markers", color=~Model_Year)

# 3D scatter plot
plot_ly(data=df_auto, x=~Weight, y=~MPG, z=~Acceleration, type="scatter3d", mode="markers", color=~Model_Year)

# You can check out more here: https://plot.ly/r/

##----------------------------##
## Extras 
##----------------------------## 

#Visnetwork - network/graph, trees, sankey diagrams
#https://datastorm-open.github.io/visNetwork/

#Corrplot - quickly create correlation cross tab plots
#https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html

#Leaflet - interactive maps 
#https://rstudio.github.io/leaflet/

#Lattice - create tiled panels of graphs, generally for multivariate analysis
#https://www.statmethods.net/advgraphs/trellis.html

#googleVis - interactive charts, similar to plotly 
#https://cran.r-project.org/web/packages/googleVis/vignettes/googleVis_examples.html

#dygraphs - interactive time series charts 
#https://rstudio.github.io/dygraphs/

#https://www.r-graph-gallery.com/
