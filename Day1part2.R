# Date:19 April 2021
#Author:Megan Vokes 
# Day1 part2 

#calculate sample size 
library(tidyverse)
chicks <- as_tibble(ChickWeight)
nrow(chicks)#doesnt give n in this case , gives number of obs rather 
unique(chicks$Chick)# gives smaple size in this case , because counts each unique thing ( not repetitions)
#note the distinction betweeen nrow and the true sample size
chicks %>% 
filter(Time == "20" ) %>%
  group_by(Diet) %>% 
  summarise(chicks = mean(weight)) #mean weight of chicks  for each diets 

chicks %>% 
  filter(Time == "20" ) %>%
  summarise(chicks = mean(weight), chicks = sd(weight))# mean and sd of chicks acroos all diets 


chicks %>% 
  group_by(Diet) %>% 
  filter(Time == "20" ) %>%
  summarise(chicks = mean(weight), chicks = sd(weight))#mean and sd for eeach diet 


chicks %>% 
  group_by(Diet) %>% 
  filter(Time == "20" ) %>%
  summarise(chicks = mean(weight), chicks_sd = sd(weight),chicks_m = median(weight))#mean ,sd and median for each diet 

chicks %>% 
  filter(Time == "20" ) %>%
  summarise(chicks = mean(weight), chicks = sd(weight),chicks_m = median(weight))  
  kurtosis(chicks$weight)#mean ,sd and median across all diets 

install.packages("e1071")
library("e1071")


chicks %>% 
  filter(Time == "20" ) %>%
  summarise(mean_wt = sum(weight) / n(),sd_wt = sd(weight))# easy way to do mean and sd 

chicks %>% 
  group_by(Diet) %>% 
  filter(Time == "20" ) %>%
  summarise(mean_wt = sum(weight) / n(),sd_wt = sd(weight)
             ,min_wt = min(weight),
            qrt1_wt = quantile(weight, p = 0.25),
            med_wt = median(weight),
            qrt3_wt = quantile(weight, p = 0.75),
            max_wt = max(weight))#how to calculate the 25th and 75th percentiles,a

range(chicks$weight)#how to get just range


chicks %>% 
  filter(Time == "20" ) %>%
  summarise(mean_wt = sum(weight) / n(),sd_wt = sd(weight),lower_wt = range(weight)[1],
                                                            upper_wt = range(weight)[2])# range overall
       

chicks %>%
  group_by(Diet) %>%
 filter(Time == "20" ) %>%
  summarise(mean_wt = sum(weight) / n(),sd_wt = sd(weight),lower_wt = range(weight)[1],
            upper_wt = range(weight)[2])#range per diet



dat1 <- c(NA, 12, 76, 34, 23)
mean(dat1)
mean(dat1, na.rm = TRUE)#how to find the mean( or others ) with missing values 



























