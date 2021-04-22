#Day4 
#Author :Megan Vokes
#Date :22/04/2021

install.packages("rcompanion")

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

Input <- ("
Student  Sex     Teacher  Steps  Rating
a        female  Jacob    8000   7
b        female  Jacob    9000  10
c        female  Jacob   10000   9
d        female  Jacob    7000   5
e        female  Jacob    6000   4
f        female  Jacob    8000   8
g        male    Jacob    7000   6
h        male    Jacob    5000   5
i        male    Jacob    9000  10
j        male    Jacob    7000   8
k        female  Sadam    8000   7
l        female  Sadam    9000   8
m        female  Sadam    9000   8
n        female  Sadam    8000   9
o        male    Sadam    6000   5
p        male    Sadam    8000   9
q        male    Sadam    7000   6
r        female  Donald   10000  10
s        female  Donald    9000  10
t        female  Donald    8000   8
u        female  Donald    8000   7
v        female  Donald    6000   7
w        male    Donald    6000   8
x        male    Donald    8000  10
y        male    Donald    7000   7
z        male    Donald    7000   7
")


Input <- ("
Student  Sex     Teacher  Steps  Rating
a        female  Jacob    8000   7
b        female  Jacob    9000  10
c        female  Jacob   10000   9
d        female  Jacob    7000   5
e        female  Jacob    6000   4
f        female  Jacob    8000   8
g        male    Jacob    7000   6
h        male    Jacob    5000   5
i        male    Jacob    9000  10
j        male    Jacob    7000   8
k        female  Sadam    8000   7
l        female  Sadam    9000   8
m        female  Sadam    9000   8
n        female  Sadam    8000   9
o        male    Sadam    6000   5
p        male    Sadam    8000   9
q        male    Sadam    7000   6
r        female  Donald   10000  10
s        female  Donald    9000  10
t        female  Donald    8000   8
u        female  Donald    8000   7
v        female  Donald    6000   7
w        male    Donald    6000   8
x        male    Donald    8000  10
y        male    Donald    7000   7
z        male    Donald    7000   7
")



data <- read.table(textConnection(Input),header = TRUE)
summary(data)



library(rcompanion)
# ungrouped data is indicated with a 1 on the right side of the formula, or the group = NULL argument.
groupwiseMean(Steps ~ 1,data = data, conf = 0.95, digits = 3)

data2 <- groupwiseMean(Steps ~ Sex, data = data, conf = 0.95,digits = 3)# one-way data
#the mean value 7690 is half was between lower confidence level and upper confidence levels          
              
 # plot of one way data

data2 <- groupwiseMean(Steps ~ Sex, data = data, conf = 0.95,digits = 3)

ggplot(data2 ) + # creates the plot
  geom_col(aes(x = Sex, y = Mean, fill = Sex)) +
  geom_errorbar(aes(ymin = Trad.lower,
                    ymax = Trad.upper,
                    x = Sex),
                col = "black",
                width = 0.2) + 
  labs(x = "Sex", y ="Steps", title = "The relationship between the amount of steps the students take" )+
scale_fill_manual(values = c("springgreen","cyan"))+
  theme(axis.text.x = element_text(angle = 45,hjust = 1),
                                   panel.background = element_rect(fill = "lemonchiffon",color = "black")) 
 
  #outlier.colour = "blue", outlier.shape = 8, # specifies the details of how the plot will appear
               #) +          #  outlier colour, outlier shape (1 is a dot),fill box colour, col line colour  
  #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +    # determines heading and axis information
  #xlab("Site") + ylab("Total Length of Laminaria (cm)") # lables 

#Two way data

data3 <- groupwiseMean(Steps ~ Teacher + Sex, data = data, conf = 0.95,digits = 3)

  
  ggplot(data3 ,aes(x = Sex, y = Mean),fill = Sex) + # creates the plot
  geom_col(fill = "cyan2",col = "cadetblue3") +
  geom_errorbar(aes(ymin = Trad.lower,
                    ymax = Trad.upper,
                    x = Sex),
                col = "black",
                width = 0.2) +
    labs(title = "Steps Per Teacher Data",
         subtitle = "Distingushing Between Male and Female ",
         x = "Sex", y = "Steps")+
    facet_wrap(~ Teacher,ncol=3)+
  theme(axis.text.x = element_text(angle = 45,hjust = 1),
        legend.position = "left", panel.background = element_rect(fill = "darkseagreen2",color = "black")) 
    
  

 # xlab("Sex") + ylab("Steps")       
  
  #Bootstraping- repeats the test, and takes random sample , to check how often it falls within or out of the interval 
  #One Way 
  groupwiseMean(Steps ~ Sex,
                data = data,
                conf = 0.95,
                digits = 3,
                R = 10000,
                boot = TRUE,
                traditional = FALSE,
                normal = FALSE,
                basic = FALSE,
                percentile = FALSE,
                bca = TRUE)
  
  #2Way 
  groupwiseMean(Steps ~ Teacher + Sex,
                data = data,
                conf = 0.95,
                digits = 3,
                R = 10000,
                boot = TRUE,
                traditional = FALSE,
                normal = FALSE,
                basic = FALSE,
                percentile = FALSE,
                bca = TRUE)
  
  #ANOVA
  
  anova <- aov(Steps~Sex*Teacher,data = data)
  summary(anova)
  
  anova_Tukey <- TukeyHSD(anova)
  plot(anova_Tukey, col = "springgreen2")
    #theme(legend.position = "left",panel.background = element_rect(fill = "honeydew",color = "black"))
  
  