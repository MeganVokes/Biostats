#Biostatistics
#Data Manipulation, Anlyses and Visualisation
#Quiz_2
#Author:Megan Vokes
#Date:22/04/2021



# Question1 ---------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(ggplot2)
library(Rmisc)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(dslabs)
library(readr)
library(ggpubr)
library("reshape")                                                # Load reshape package 
library(plotly)
library(plyr)
library(rcompanion)

#Orange
Orange <- datasets::Orange %>% 
group_by(Tree)


ggplot(data = Orange, aes(x = age,y =circumference,fill = Tree)) +
  geom_col(position = "dodge")+
  geom_density(aes(y = 1*..count..,fill = Tree), alpha = 0.4) +
  labs(x = "value")+
  scale_x_continuous(breaks = c(118,484,664,1004,1231,1372,1582))


Orange %>% 
  group_by(Tree) %>% #this will test normality for A and B seperatly
  summarise(norm_O = as.numeric(shapiro.test(Orange$circumference)[2])) #

Orange%>%
  group_by(Tree) %>%
  summarise(O_var = var(circumference))
#he variance of the samples we are com




Toothgrowth <- datasets::ToothGrowth %>% 
group_by(supp)

ggplot(data = Toothgrowth, aes(x = dose, y = len, fill = supp)) +
  geom_boxplot() +
  scale_fill_manual(values = c("springgreen3","cadetblue4"))+
  labs(title = "Tooth Growth in Guinea Pigs",
       subtitle = "Based on different Supplements ",
       x = "Dose", y = "Length")

#


Warpbreaks <- datasets::warpbreaks %>% 
group_by(wool)


ggplot(data = Warpbreaks, aes(x = tension, y = breaks ,fill = wool)) +
  scale_fill_manual(values=c("orchid","cadetblue"))+
  #scale_fill_brewer(palette = "set3")
  geom_bar(stat="identity", position=position_dodge())+
  labs(x = "Tension", y = "Breaks", title = "Graph of Wool Tension ") +
  theme(axis.text.x = element_text(angle = 90))

#



# Question 2 ---------------------------------------------------------

SACTN <- SACTN_daily_v4.2 %>% 
  separate(col = index, into = c("site", "src"), sep = "/") %>% 
group_by(site,src)


#sep = "", what the words are seperated as
#into + what we seperating into
#col = ..., the name of the column we want to seperate















