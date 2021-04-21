#Snakes
#Date:21/04/2021
#Author:Megan


library(tidyverse)
library(ggplot2)
library(ggrepel)
library(plotly)
library(ggpubr)
library(dplyr) 
library(dslabs)
library(lubridate)
library(Rmisc)
options(scipen = 100)


snakes <- read_csv("data/snakes.csv")
snakes$day = as.factor(snakes$day)

snakes.summary <- snakes %>% 
  group_by(day, snake) %>% 
  summarise(mean_openings = mean(openings),
            sd_openings = sd(openings)) %>% 
  ungroup()
snakes.summary


snakes.summary <- snakes %>% 
  group_by(day) %>% 
  summarise(mean_openings = mean(openings),
            sd_openings = sd(openings)) %>% 
  ungroup()
snakes.summary


snakes.summary2 <- summarySE(data = snakes, measurevar = "openings", groupvars = c("day"))


ggplot(data = snakes, aes(x = day, y = openings)) +
  geom_segment(data = snakes.summary2, aes(x = day, xend = day, y = openings - ci, yend = openings + ci, colour = day),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)


par(mfrow = c(2, 2))
snakes.aov <- aov(openings ~ day + snake, data = snakes)
summary(snakes.aov)
snakes.res <- residuals(snakes.aov)
hist(snakes.res, col = "red")
plot(fitted(snakes.aov), residuals(snakes.aov), col = "red")
snakes.tukey <- TukeyHSD(snakes.aov, which = "day", conf.level = 0.90)
plot(snakes.tukey, las = 1, col = "red")



Snakes_1 <- snakes %>% 
  group_by(day)

ggplot(data = Snakes_1, aes(x = snake, y = openings ,fill = snake)) +
  scale_fill_manual(values=c("dark orchid", "cyan","darkolivegreen","violet","Springgreen3","powderblue"))+
  #scale_fill_brewer(palette = "set3")
  geom_bar(stat="identity", position=position_dodge())+
  labs(x = "Days", y = "Number of Openings", title = "Graph of snake Habituation ") +
  theme(axis.text.x = element_text(angle = 90))



ggplot(data = Snakes_1, aes(x = day,y = openings,fill = snake)) +
         geom_col(position = "Dodge")+
  labs(x = "Days", y = "Number of Openings", title = "Graph of snake Habituation ")

  



  


