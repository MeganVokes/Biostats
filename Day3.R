##Day_3_biostats
#Simple linear regressions
#Date:21 APril 2021
#Author:Megan Vokes
install.packages("Rmisc")
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
install.packages("reshape")                                       # Install reshape package
library("reshape")                                                # Load reshape package
library(plotly)
install.packages("plyr")
library(plyr)


#comparing the influence of one variable to another
data(faithful)
head(faithful)

eruption.lm <- lm(eruptions ~ waiting, data = faithful)
summary(eruption.lm)

slope <- round(eruption.lm$coef[2], 3)
# p.val <- round(coefficients(summary(eruption.lm))[2, 4], 3) # it approx. 0, so...
p.val = 0.001
r2 <- round(summary(eruption.lm)$r.squared, 3)

ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_point() +
  annotate("text", x = 45, y = 5, label = paste0("slope == ", slope, "~(min/min)"), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.75, label = paste0("italic(p) < ", p.val), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.5, label = paste0("italic(r)^2 == ", r2), parse = TRUE, hjust = 0) +
  stat_smooth(method = "lm", colour = "seagreen2") +
  labs(title = "Old Faithful eruption data",
       subtitle = "Linear regression",
       x = "Waiting time (minutes)",
       y = "Eruption duration (minutes)")
#if you can use anova or t-test you can linear regression


# Correlation -------------------------------------------------------------

install.packages("corrplot")
library(corrplot)

ecklonia <- read_csv("data/ecklonia.csv")

ecklonia_sub <- ecklonia %>% 
  select(-species, - site, - ID)


#pearson
cor.test(x = ecklonia$stipe_length, ecklonia$frond_length,
         use = "everything", method = "pearson")# corr value closer to 1 = stronger correlation

ecklonia_pearson <- cor(ecklonia_sub)
View(ecklonia_pearson)


#spearman
cor.test(x = ecklonia$stipe_length, ecklonia$frond_length,
         use = "everything", method = "spearman")


#kendall rank
ecklonia_norm <- ecklonia_sub %>% 
  gather(key = "variable") %>% 
  group_by(variable) %>% 
  summarise(variable_norm = as.numeric(shapiro.test(value)[2]))
ecklonia_norm

cor.test(ecklonia$primary_blade_length, ecklonia$primary_blade_width, method = "kendall")

r_print <- paste0("r = ", 
                  round(cor(x = ecklonia$stipe_length, ecklonia$frond_length),2))#stores the answer in a object


#single panel 
ggplot(data = ecklonia, aes(x = stipe_length, y = frond_length)) +
  geom_smooth(method = "lm", colour = "darkorchid4", se = F) +
  geom_point(colour = "darkmagenta") +
  geom_label(x = 300, y = 240, label = r_print) +# can now use the rprint ( alternative ,annotate)
  labs(x = "Stipe length (cm)", y = "Frond length (cm)",title = "Graph showing reltionship between Stipe Length and Frond Length") +
  theme_pubclean()

#multipanel visual
corrplot(ecklonia_pearson, method = "circle")


# Exercise ----------------------------------------------------------------
  #Method 1                    
heatmap(ecklonia_pearson) #heatmap with lines

heatmap(ecklonia_pearson,Rowv = NA, Colv = NA ) #Heatmap original 


my_colors <- colorRampPalette(c("cadetblue", "springgreen"))  
heatmap(ecklonia_pearson, col = my_colors(100),Rowv = NA, Colv = NA) #changed colour heatmap 

#Method 2

Ecklonia_melt <- melt(ecklonia_pearson)                                           # Reorder data
head(Ecklonia_melt)                                                   # First six rows of data
ggp <- ggplot(Ecklonia_melt, aes(X1, X2)) +                           # Create heatmap with ggplot2
  geom_tile(aes(fill = value))  #base graph

ggp + scale_fill_gradient(low = "green", high = "chocolate") +
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) +
labs(x = "",y = "",title = "Graph showing reltionship between Ecklonia Variables")#base graph + colour change  +title ,hjust etc.

#Method #
plot_ly(z = ecklonia_pearson, type = "heatmap")    #base graph                            # Apply plot_ly function

plot_ly(z = ecklonia_pearson, colorscale = "Blues", type = "heatmap")         # Manual colors




#dlply
Base <- baseball
?dlply()


linmod <- function(df) {
  lm(rbi ~ year, data = mutate(df, year = year - min(year)))
}
models <- dlply(Base, .(id), linmod)
models[[1]]


