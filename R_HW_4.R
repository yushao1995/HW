###------------------------------------### 
#
# Class 4 Work:
#
###------------------------------------### 

# Import libraries
library(data.table)
library(dplyr)
library(stringr)

# Download the Auto_MPG.csv from the class website 
# Set the working directory to the location of the data
setwd("/Users/shao/Desktop/HW")

#1 Load the Auto_MPG.csv into a data frame named "df_auto". How many columns are in "df_auto"?
df_auto <- fread("Auto_MPG.csv", stringsAsFactors = FALSE)
#9

#2 Convert "Horsepower" to numeric. How many NAs are in "df_auto" after converting "Horsepower"?
df_auto$Horsepower <- as.numeric(df_auto$Horsepower)
summary(df_auto$Horsepower)
#6

#3 Using dplyr, add to "df_auto":
# 3.1 Variable named "Car_Make" that is the first word from "Car_Name"
# 3.2 Variable named "Car_Model" that is the remaining words (not first) from "Car_Name" 
# How many observations of "Car_Make" "ford" are there? 
df_auto_hold <- df_auto %>% 
  group_by(Car_Name) %>%
  mutate(Car_Make=str_split(Car_Name," ")[[1]][1]) %>%
  ungroup(Car_Name) %>%
  filter(Car_Make=="ford")
#51
  

#4 Using dplyr, how many cars are there for model year 73? 
df_auto_hold <- df_auto %>%
  filter(Model_Year==73)
#40


#5 Using dplyr, what "Car_Make" "ford" car ("Car_Name") has the highest "Weight_per_Horsepower" (Weight/Horsepower) ratio? Ignore NAs. 
df_auto_hold <- df_auto %>% 
  group_by(Car_Name) %>%
  mutate(Car_Make=str_split(Car_Name," ")[[1]][1]) %>%
  ungroup(Car_Name) %>%
  filter(Car_Make=="ford") %>%
  mutate(Weight_per_Horsepower=(Weight/Horsepower)) %>%
  arrange(desc(Weight_per_Horsepower))
#ford granada ghia



#6 Using dplyr, create a ranking variable, "MPG_Rank", that ranks, using the row_number() function, "MPG" by "Model_Year" in descending order. 
# What is the third highest "MPG" "Car_Name" for "Model_Year" 1972?
df_auto_hold <- df_auto %>%
  group_by(Model_Year)%>%
  arrange(desc(MPG)) %>%
  mutate(row_number=row_number()) %>%
  ungroup(Model_Year) %>%
  filter(Model_Year=="72")
#toyota corolla 1600 (sw)


#7 Using dplyr and stringr, update "df_auto" so all values of "Car_Make" that contain "chev" say "chevrolet".
# How many observations have "Car_Make" "chevrolet"
df_auto_hold <- df_auto %>% 
  group_by(Car_Name) %>%
  mutate(Car_Make=str_split(Car_Name," ")[[1]][1]) %>%
  ungroup(Car_Name)

df_auto_hold$Car_Make = str_replace(df_auto_hold$Car_Make,"chev.*","chevrolet")

df_auto_hold <- df_auto_hold %>%
  filter(Car_Make=="chevrolet")
#47

#8 What is the average MPG for "chevrolet" cars?  
df_auto_hold <- df_auto %>% 
  group_by(Car_Name) %>%
  mutate(Car_Make=str_split(Car_Name," ")[[1]][1]) %>%
  ungroup(Car_Name) %>%
  mutate(Car_Make = str_replace(Car_Make, "chevy", "chevrolet")) %>%
  filter(Car_Make=="chevrolet")

mean(df_auto_hold$MPG)
#20.31


#Below is a data frame, "df_auto_start", that contains the first production year for some of the auto makes.

Car_Make <- c("amc", "audi", "bmw", "buick", "chevrolet", "datsun", "dodge", "ford")
First_Year <- c(1954, 1910, 1916, 1903, 1911, 1931, 1900, 1903)
df_auto_start <- data.frame(Car_Make=Car_Make, First_Year=First_Year, stringsAsFactors = FALSE)

# Use the data to answer the following questions:

#9 Using dplyr joins, what is the average "MPG" for car makes where "First_Year" exists? 
df_auto_hold <- df_auto %>% 
  group_by(Car_Name) %>%
  mutate(Car_Make=str_split(Car_Name," ")[[1]][1]) %>%
  ungroup(Car_Name) %>%
  inner_join(df_auto_start, by="Car_Make")

mean(df_auto_hold$MPG)
#21.55528

#10 Using dplyr joins, how many cars exist in the data set where "First_Year" is null? 
df_auto_hold <- df_auto %>% 
  group_by(Car_Name) %>%
  mutate(Car_Make=str_split(Car_Name," ")[[1]][1]) %>%
  ungroup(Car_Name)

df_auto_hold$Car_Make = str_replace(df_auto_hold$Car_Make,"chev.*","chevrolet")

df_auto_hold <- df_auto_hold %>%
  left_join(df_auto_start, by="Car_Make") %>%
  filter(is.na(First_Year))

#195

