#Test 1
#Author:Megan Vokes
#Date:20/04/2021


library(tidyverse)
library(ggplot2)
library(dplyr) 
library(dslabs)
library(ggrepel)
library(readr)
library(plotly)
library(ggpubr)
library(e1071)




# Question1 ---------------------------------------------------------------

#the various data classes are :
#Numerical Data-Refers to that data which is qualitative and as such  can be measured or counted 
#              - Can be discrete_such that the values are whole and are usually counted 
#             _Can be continuous_such that the values are rational and are usually measured 
#Qualitative Data-Refers to that data which features categories 
#               _ Categorical data_data is in categories and can be counted per category 
#               - Ordinal Data_ features Categorized data and the categories are ranked/ordered 
#BinaryData- Refers to that data in which only two outcomes ( mutually exclusive)are possible .eg. Yes or No 
#Character Data_ Refers to that data which is not numerical ( words )
#Missing Data_Refers to that data which is missing from a dataset but must be represented by a place holder (NA)


View()
str()
head()
tail ()
names()
nrow()
ncol()
View()
slice()
glimpse()



# Skewness , refers to the position of the median relative to the mean , 
#a positively skewed dataset will have a median that is < the mean ( also right skewed )
#a negatively skewed dataset will have a median that is > the mean ( also left skewed )

# This is different from Kurtosis which refers to the state of the tail section of distributed 
#data, Normally distributed data will have a kurtosis = 0 , if the kurtosis is<0 then it is platykurtic 
#if the Kurtosis is >0 then it is leptokurtic 



# Question2 ---------------------------------------------------------------

Orange <- datasets::Orange
str(Orange)
# The Orange Dataset has Ordinal  and Numerical data

head(Orange)
tail (Orange)
names(Orange)
summary(Orange)


#mean , median ,sd for each of tree

Orange%>% 
  group_by(Tree) %>% 
  summarise(Orange = mean(age), Orange_sd = sd(age),Orange_m = median(age),
            Orange_c = mean(circumference), Orange_sdc = sd(circumference),Orange_mc = median(circumference))


kurtosis(Orange$circumference)# Kurtosis =  -1.239487
skewness(Orange$circumference)# Skewness = 1.655431e-05

Orange%>% 
  group_by(Tree) %>% 
summarise(mean_c = sum(circumference) / n(),sd_c = sd(circumference)
          ,min_c = min(circumference),
          qrt1_c = quantile(circumference, p = 0.25),
          med_c = median(circumference),
          qrt3_c = quantile(circumference, p = 0.75),
          max_c = max(circumference)) 
  


ggplot(data = Orange, aes(x = age, y = circumference, colour = Tree)) +
  geom_line() +
  labs(y = "Circumference", x = "Age") +
  theme(legend.position = "none") + 
  scale_x_continuous(breaks = c(118,484,664,1004,1231,1372,11582))








# Question3 ---------------------------------------------------------------

mutate()#Creates new coloums
select()#
group_by()#Groups data together by specified 
filter()#Keeps the filtered data and removes the others 
seperate()



