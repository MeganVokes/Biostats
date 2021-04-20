#Biostatistic and Intro_R recap assignment: Data Manipulation, Anlyses and Visualisation
#Due date: 20 April 2021 
#Author:Megan Vokes 

library(tidyverse)
library(ggplot2)
library(ggrepel)
# Section 1 ------------------------------------------------------------
BOD <- BOD

C

# Section 2 ---------------------------------------------------------------
install.packages("dplyr")
install.packages("dslabs")

library(plotly)
library(ggpubr)
library(dplyr) 
library(dslabs)
library(lubridate)
options(scipen = 100)
data(murders)

glimpse(murders)
head(murders)
tail (murders)
names(murders)
nrow(murders)
ncol(murders)
View(murders)
slice(murders)
str(murders)


# The murders datasets shows the amount of murders committed in a state that forms part of a region (also shown )
# the data set also show the population of the state within which the murder was committed . 
#There is data present for 51 states (therefore there are 51 rows ) and the data appears in 5 coloumns  

#
Murders <- murders %>% 
  select(state,population)

#
Murder1 <- murders %>% 
  filter(state != "Florida")
#
no_south <- murders %>%
  filter(region != "South")
  unique(no_south)
  
#  
south <- murders %>% 
  filter(region == "South")
  nrow(south)
# this question was a bit difficult understand , so ill answer both interpretations,
#the number of states after removing those from the south is 34 , the number of states 
#in the south region is 17.

  Combopop <- murders %>%
  filter(region %in% c( "South","West")) %>% 
    arrange(region) %>% 
  summarise(pop_s = sum(population[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]),
          pop_w = sum(population[c(18,19,20,21,22,23,24,25,26,27,28,29,30)]))

#the population in the south region is 115 674 434 .
#the population in the west region is 71 945 553 .

  
#
NE <- murders %>% 
  select(region,population) %>% 
   filter(region == "Northeast")

#
NC <- murders %>% 
  filter(region == "North Central")
  ggplot(data = NC , aes(x = population, y = total)) +
  geom_text_repel(aes(label = state), size = 2.5) +
  geom_point(aes(colour = state)) +
  geom_smooth(method = "lm") +
  labs(x = "Population size", y = "Total gun murders", title = "Gun murders per North Central State") +
  theme(axis.text.x = element_text(angle = 90))
  #This graph shows the relationship between the total murders commited in the north central state
  #and the populations of the north central states . there is a line of regression present which shows 
  #increased population also have more murders .
  #


North <- murders %>% 
  select(region,total,state) %>% 
  filter(region %in% c("North Central","Northeast")) %>% 
  arrange(region) 
  ggplot(data=North, aes(x = state, y = total,fill = region)) +
    geom_bar(stat="identity", position=position_dodge()) +
    scale_fill_manual(values=c("mediumpurple 2", "seagreen 2")) +
    
    theme(axis.text.x = element_text(angle = 90,hjust = 1)) +
    labs(x = "States", y = "Total Number of Murders ",
         title = "Total Murders in The Northern Regions in 2010")
  
  #this graph displays the total number of murders across all nothern regions 
  #by state . this gives a clear veiw of the amount of murders happening in the 
  #states and the north regions as a whole 

  
   #comparison of South and West pop 

  SandW <- murders %>% 
    select(region,population) %>% 
    filter(region %in% c("South","West")) %>% 
    group_by(region) %>% 
    summarise(mean_population = mean(population)) 
  
  
  ggplot(data = SandW,aes(x = region, y =mean_population ,fill = region)) +
    scale_fill_manual(values=c("dark orchid", "cyan")) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme(axis.text.x = element_text(angle = 90,hjust = 1), legend.position = "none") +
    labs(x = "Regions", y = "Mean Population ",
         title = "Comparison of Mean Number of Murders in The South and West Region")
  # this graph was created using mean population because i couldnt figure out how to compare the data as is 
 
  # 
  new_data <- murders %>%
    filter(total %in% (20:100)) %>% #filer data >20 and <100
    arrange(total) #arrange it numerically
  head(new_data)
  tail(new_data)
  
  #
  slc <- murders %>%
    slice(10:24, 26) #index rows 10 to 24 and row 26.
 
   #
  murders_tibble <- as_tibble(murders) 
  
  #
  murders_tibble %>% 
    group_by(region) %>% 
arrange(region)  
  
  
  

# Section3 ----------------------------------------------------------------
?heights
  data(heights)
   #the data presented in the the dataset heights refers to those self-reported 
  #heights in inches for males and females ,it features 2 columns and 1050 rows
  #the data is categorical and numerical 

#
  glimpse(heights)
  head(heights)
  tail (heights)
  names(heights)
  nrow(heights)
  ncol(heights)
  View(heights)
  slice(heights)
  str(heights)
  
 #
  
  heights %>% 
    group_by(sex) %>% 
    summarise(H_mean = mean(height), H_sd = sd(height),H_m = median(height),
              min_H = min(height),max_H = max(height))
#Females : Mean= 64.9,sd= 3.76, min = 51 , max = 79 , median = 65
 #males : Mean= 69.3,sd= 3.61, min = 50 , max = 82.7 , median = 69
  
  
  

# Section4 ----------------------------------------------------------------
#
  x <- c( 1, 6, 21, 19 , NA, 73, NA) 
  y <- c(NA, NA, 3, NA, 13, 24, NA)
  sum(is.na(x))
  sum(is.na(y))
  
  
  
  meg_fun <- function(x){
    x <- (sum(is.na(x)))
return(x)
    
    
    
    }
    meg_fun(x)
    
    
    #
    Car <- c(1,2,NA,2,4,6,4,8,NA)
    Models<- c(21.22,NA,15,NA,108,127,113,NA)
    Colours <- c(17,NA,1953,NA ,2,9,NA,93 ,NA)
    meg_fun(Car)
    meg_fun(Models)
    meg_fun(Colours)
    
    
    
    
    
    

# section5 ----------------------------------------------------------------
#
    Seasonal_data <- data.frame(year = c(2015, 2016, 2017, 2018), 
                                winter = c(41, 39, 47, 40), 
                                spring = c(41, 46, 57, 45), 
                                summer = c(75, 52, 85, 66), 
                                Autumn = c(57, 66, 52, 56)) 
    #Hypothesis = Summer is conssitantly getting hotter 
    
  Season <-  Seasonal_data %>% 
    gather(winter,summer,spring,Autumn,key = Seasons, value = temp ) %>% 
    group_by(y)

    
    
    
    #
    cats_data<- tibble(cats = c("A", "B", "C"), 
                       position = c("1-2-3", "3-1-2", "2-3-1"), 
                       minutes = c(3, 3, 3), 
                       seconds = c(12, 44, 15)) 
    cats_data 
    
    cats_data_tidy <- cats_data %>%
      separate(col = position, into = c("first_place", "second_place", "third_place"), sep = "-") %>% 
      unite(minutes, seconds, col = "total_time", sep = "-")
    view(cats_data_tidy)
    

# section6 ----------------------------------------------------------------
    data(package = .packages(all.available = TRUE))
    
    
   Titanic <- datasets::Titanic
    view(Titanic)
    
    Titanic %>% 
     gather()
    spread() 
    seperate() 
    joining() 
    arrange() 
    select() 
    group_by() 
    mutate() 
    
    