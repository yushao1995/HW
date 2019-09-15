## Class 5 Work 

# Import the following libaries:

# Import libraries


#1 Prepare by:
#1.1 Import the required libraries:

library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)

#1.2 Set your working directory and upload the "Auto_MPG.csv" data. Ensure stringAsFactors=FALSE.
setwd("/Users/shao/Desktop/HW")
df_auto <- fread("Auto_MPG.csv", stringsAsFactors = FALSE)
#1.3 Create a column names "Car_Make" from the first element of the "Car_Name" variable
df_auto <- df_auto %>% 
  group_by(Car_Name) %>%
  mutate(Car_Make=str_split(Car_Name," ")[[1]][1]) %>%
  ungroup(Car_Name)
#1.4 Update "horsepower" to be type numeric
df_auto$Horsepower <- as.numeric(df_auto$Horsepower)

#1.4 How many unique values of "Car_Make" are there?
length(unique(df_auto$Car_Make))
#37

#We are going to create a function that provides am univariate analysis on a provided variable vector

#2 Change the type of "Cylinders", "Model_Year", and "Origin" from numeric to character.
# How many variables in "df_auto" are of type character?
colnames(df_auto)[8] <- "Origin"
df_auto$Cylinders =as.character(df_auto$Cylinders)
df_auto$Model_Year =as.character(df_auto$Model_Year)
df_auto$Origin =as.character(df_auto$Origin)
summary(df_auto)
#5

#3 Create a function "univariate_analysis" that:
#3.1 Calculates the number of observations, mean, standard deviation, and quartiles of a provided vector. 
#3.1 Ensure the na.rm argument is TRUE
#3.2 Have the function return the results in a list with named elements "obs", "mean", "stdev", and "quartiles"
#Using the function created, what is the 2nd quartile of "MPG"?


univariate_analysis = function(v){
  n=length(v)
  mean=mean(v,na.rm=TRUE)
  std=sd(v,na.rm=TRUE)
  percentiles  <- quantile(v, probs=c(0.25,0.5, 0.75),na.rm=TRUE)
  l1 <- list(n=n,mean=mean, std=std,q1=percentiles[1],q2=percentiles[2],q3=percentiles[3])
  return(l1)
}
univariate_analysis(df_auto$MPG)$q2
#23

#4 To the "univariate_analysis" function, create a histogram plot named "histogram". Add the plot to the return list with the element named "histogram". 
#Note, you will be providing your function a vector, but ggplot only accepts data frames. You will need to convert your vector to a data frame with one column.
#What is the default number of bins in the histogram for "MPG"?
univariate_analysis = function(v){
  n=length(v)
  mean=mean(v,na.rm=TRUE)
  std=sd(v,na.rm=TRUE)
  percentiles  <- quantile(v, probs=c(0.25,0.5, 0.75),na.rm=TRUE)
  l1 <- list(n=n,mean=mean, std=std,q1=percentiles[1],q2=percentiles[2],q3=percentiles[3])
  
  dt1 <- data.frame(
    v1 =  c(v),
    stringsAsFactors = FALSE)
  histogram=ggplot(dt1, aes(x=v1)) +
    geom_histogram() 
  
  return(list(l1,histogram))

}

univariate_analysis(df_auto$MPG)
#30


#5 Update the histogram in the "univariate_analysis" to:
# 5.1 Change the number of bins to 20
# 5.2 Add vertical lines at the quartiles of the data. 
# 5.3 Change the colors of the quartile vertical lines so they are red, blue, and green respectively
# 5.4 Using geom_text, add a label next to each vertical line that corresponds to the value of of the quartile:
# 5.4.1 Set the horizontal starting position to 5
# 5.4.2 Offset the label's x position by 1.2
# 5.4.3 Change the text color to match the line color 
# 5.4.4 Change the angle to 90 
# 5.4.5 Change the text size to 12
# 5.5 Change the theme to minimal
#What x-axis tick label does the green line fall nearest for "MPG"?

univariate_analysis = function(v) {
  n=length(v)
  mean=mean(v,na.rm=TRUE)
  std=sd(v,na.rm=TRUE)
  percentiles  <- quantile(v, probs=c(0.25,0.5, 0.75),na.rm=TRUE)
  q1=percentiles[1]
  q2=percentiles[2]
  q3=percentiles[3]
  
  l1 <- list(n=n,mean=mean, std=std,q1=q1,q2=q2,q3=q3)
  
  
  dt1 <- data.frame(
    v1 =  c(v),
    stringsAsFactors = FALSE)

  histogram=ggplot(dt1, aes(x=v1)) +
    geom_histogram(bins = 20) +
    
    geom_vline(xintercept=q1,color = "red")+
    geom_text(aes(x=q1-1.2, label=toString(q1), y=5),colour="red", angle=90,size = 12)+
    
    geom_vline(xintercept=q2,color = "blue")+
    geom_text(aes(x=q2-1.2, label=toString(q2), y=5),colour="blue", angle=90,size = 12)+
    
    geom_vline(xintercept=q3,color = "green")+
    geom_text(aes(x=q3-1.2, label=toString(q3), y=5),colour="green", angle=90,size = 12)+
    
    theme_minimal()
  return(list(l1,histogram))
}

univariate_analysis(df_auto$MPG)
#30



# 6 Thus far, we've assumed the variable provided will be numeric. However, we should account for categorical variables. 
# Update the "univariate_analysis" function to:
#6.1 If the provided vector is numeric, execute everything created in the function thus far
#6.2 If the provided vector is character:
#6.2.1 Store the number of elements in the vector in a variable named "obs"
#6.2.2 Store the unique values of the vector in a variable named "unique_levels" 
#6.2.1 Store a bar chart in a variable "bar_chart" that plots the number of observations by the unqiue levels of the variable 
#6.3 Returns the previous variables in a list with the elements named "obs", "unique_levels", and "bar_chart")
#Using the function created, what "Model_Year" has the greatest number of observations?
univariate_analysis = function(v) {
   dt1 <- data.frame(
     v1 =  c(v),
     stringsAsFactors = FALSE)

  if(is.numeric(v)==FALSE){
    obs=length(v)
    unique_levels=length(unique(v))
    bar_chart=ggplot(dt1, aes(v1)) + 
      geom_bar()
    return(list(obs,unique_levels,bar_chart))
  }
  
  else{
  n=length(v)
  mean=mean(v,na.rm=TRUE)
  std=sd(v,na.rm=TRUE)
  percentiles  <- quantile(v, probs=c(0.25,0.5, 0.75),na.rm=TRUE)
  q1=percentiles[1]
  q2=percentiles[2]
  q3=percentiles[3]
  
  l1 <- list(n=n,mean=mean, std=std,q1=q1,q2=q2,q3=q3)
  
  histogram=ggplot(dt1, aes(x=v1)) +
    geom_histogram(bins = 20) +
    
    geom_vline(xintercept=q1,color = "red")+
    geom_text(aes(x=q1-1.2, label=toString(q1), y=5),colour="red", angle=90,size = 12)+
    
    geom_vline(xintercept=q2,color = "blue")+
    geom_text(aes(x=q2-1.2, label=toString(q2), y=5),colour="blue", angle=90,size = 12)+
    
    geom_vline(xintercept=q3,color = "green")+
    geom_text(aes(x=q3-1.2, label=toString(q3), y=5),colour="green", angle=90,size = 12)+
    
    theme_minimal()
  return(list(l1,histogram))}
}

univariate_analysis(df_auto$Model_Year)
#73

#7 Using the "univariate_analysis" function, what is the standard deviation of "Horsepower"  
univariate_analysis(df_auto$Horsepower)
#38.49116


#8 Using the "univariate_analysis" function, what is the median of "Displacement" 
univariate_analysis(df_auto$Displacement)
#148.5

#9 Using the "univariate_analysis" function, what is the most common number of cylinders for cars in the data set? 
univariate_analysis(df_auto$Cylinders)
#4


#10 Using the "univariate_analysis" function, what is the mean of Weight? 
univariate_analysis(df_auto$Weight)
#2970.425