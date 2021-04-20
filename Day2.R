#Day 2
#Author:Megan Vokes
#ANova tests and inferences
#Date:20 April 2021

library(tidyverse)
library(readr)
library(ggplot2)
install.packages("plotly")
library(plotly)
library(ggpubr)

set.seed(666)
r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)), #setting the variables of data frame
                    sample = c(rep("A", 1000), rep("B", 1000)))

# Create histogram
#its good to plot data before looking at normality or homoscedastic
h <- ggplot(data = r_dat, aes(x = dat, fill = sample)) +
  geom_histogram(position = "dodge", binwidth = 1, alpha = 0.8) +
  geom_density(aes(y = 1*..count.., fill = sample), colour = NA, alpha = 0.4) +
  labs(x = "value")
h
#when plotting, you have to statistically prove that it is normal.

shapiro.test(r_dat$dat)
#this is showing that data is not normal. But we proved that it was.
#This happend because it took the enitre normality of the dataset instead of
#a and b seperatly.



r_dat %>%
  group_by(sample) %>% #this will test normality for A and B seperatly
  summarise(norm_dat = as.numeric(shapiro.test(r_dat$dat)[2])) #gives normality test in a new column
#summarise is when you use a built in function (mean, mediun, etc.) to create a column.

#  Homoscedasticity -------------------------------------------------------

# to test that the variance of things being compared are homogeneous

r_dat %>%
  group_by(sample) %>%
  summarise(sample_var = var(dat))
#he variance of the samples we are comparing should not be more than two to four
#times greater than one another
#there this data us homoscedastic


# creating a function -----------------------------------------------------

two_assum <- function(x) {
  x_var <- var(x)
  x_norm <- as.numeric(shapiro.test(x)[2])
  result <- c(x_var, x_norm)
  return(result)
}

r_dat %>%
  group_by(sample) %>%
  summarise(sample_var = two_assum(dat)[1],
            sample_norm = two_assum(dat)[2])


# one sample t-test -------------------------------------------------------

#comparing sample to mean
#in this case p<0.05 for the data to be normal

# create a single sample of random normal data
set.seed(666)
r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5),
                    sample = "A")

# check normality
shapiro.test(r_one$dat) #testing normality

# compare random data against a population mean of 20
t.test(r_one$dat, mu = 20)
#mu = 20, this is the value given to the mean MU=mean

# compare random data against a population mean of 30
t.test(r_one$dat, mu = 30)
#p>0.05 so its no significantly different.


# two-sample t-test -------------------------------------------------------

#will use this the most and anova
#most common t test used

set.seed(666)
r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    
                    sample = c(rep("A", 20), rep("B", 20)))
#creates a dataset named r_two. With A and B
r_two
t.test(dat ~ sample, data = r_two, var.equal = TRUE)
#p<0.05, therefore there is a significant difference between the samples

#compare means is not a common method. You have to specify you're testing t-tests
#compare_means(dat ~ sample, data = r_two, method = "t.test", var.equal = TRUE)
r_dat %>%
  group_by(sample) %>% #this will test normality for A and B seperatly
  summarise(norm_dat = as.numeric(shapiro.test(r_dat$dat)[2])) #gives normality test in a new column
#summarise is when you use a built in function (mean, mediun, etc.) to create a column.

#  Homoscedasticity -------------------------------------------------------

# to test that the variance of things being compared are homogeneous

r_dat %>%
  group_by(sample) %>%
  summarise(sample_var = var(dat))
#he variance of the samples we are comparing should not be more than two to four
#times greater than one another
#there this data us homoscedastic


# creating a function -----------------------------------------------------

two_assum <- function(x) {
  x_var <- var(x)
  x_norm <- as.numeric(shapiro.test(x)[2])
  result <- c(x_var, x_norm)
  return(result)
}

r_dat %>%
  group_by(sample) %>%
  summarise(sample_var = two_assum(dat)[1],
            sample_norm = two_assum(dat)[2])


# one sample t-test -------------------------------------------------------

#comparing sample to mean
#in this case p<0.05 for the data to be normal

# create a single sample of random normal data
set.seed(666)
r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5),
                    sample = "A")

# check normality
shapiro.test(r_one$dat) #testing normality

# compare random data against a population mean of 20
t.test(r_one$dat, mu = 20)
#mu = 20, this is the value given to the mean MU=mean

# compare random data against a population mean of 30
t.test(r_one$dat, mu = 30)
#p>0.05 so its no significantly different.


# two-sample t-test -------------------------------------------------------

#will use this the most and anova
#most common t test used

set.seed(666)
r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    
                    sample = c(rep("A", 20), rep("B", 20)))
#creates a dataset named r_two. With A and B
r_two
t.test(dat ~ sample, data = r_two, var.equal = TRUE)
#p<0.05, therefore there is a significant difference between the samples

#compare means is not a common method. You have to specify you're testing t-tests
#compare_means(dat ~ sample, data = r_two, method = "t.test", var.equal = TRUE)
     


# Practice ---------------------------------------------------------- 
ecklonia <- read.csv("data/ecklonia.csv") %>% 
  gather(key = "variable", value = "value", -species, -site, -ID)

ggplot(data = ecklonia, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip()



ecklonia_sub <- ecklonia %>% 
  filter(variable == "stipe_mass")
ggplot(data = ecklonia_sub, aes(x = variable, y = value, fill = site)) +
  scale_fill_manual(values = c("Boulders Beach" = "Powder Blue",
                               "Batsata Rock" = "Light Green"))+
  geom_boxplot() +
  coord_flip() +
  labs(x = "", y = "Stipe Mass (kg)", title = "Stipe Masses At Two Beach Locations")+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


two_assum <- function(x) {
  x_var <- var(x)
  x_norm <- as.numeric(shapiro.test(x)[2])# does normality and homo together needed to do tests 
  result <- c(x_var, x_norm)
  return(result)
}


ecklonia_sub %>% 
  group_by(site) %>% 
  summarise(stipe_mass_var = two_assum(value)[1],
            stipe_mass_norm = two_assum(value)[2])#if p>0.5 then normal 

t.test(value ~ site, data = ecklonia_sub, var.equal = TRUE, alternative = "greater")# if p <0.5 then significant difference 



# ANOVA -------------------------------------------------------------------
#there is a way to make your data normal
#research on google for this method

#if you do not want to transform your date there are calculations that you can use
#one factor anova - comaprng one sample
#two factor anova - comparing two samples

chicks <- as_tibble(ChickWeight)

chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 21)#filtering for diet 1 and 2 at time 21
compare_means(weight ~ Diet, data = chicks_sub, method = "t.test")#comparing diet to weight

#there is no significant influence as the p>0.05.

chicks.aov1 <- aov(weight ~ Diet, data = filter(chicks, Time == 21))
summary(chicks.aov1)#single factor anova

#allows you to compare two variables at the same time while maintain reliability
#of your data
summary(chicks.aov1)


# multiple factor ANOVA ---------------------------------------------------

#when runing multiple anova's
#changing a factor(e.g. time) messes with the reliabilit if the data
#with this funciton you have reduce this effect
chicks.aov2 <- aov(weight ~ as.factor(Time), data = filter(chicks, Time %in% c(0, 2, 10, 21)))
summary(chicks.aov2)
#decreases chances of a type 1 error.

# alternatives to ANOVA ---------------------------------------------------
#when you trnasform data specify that it was transfored in your methods and materials

#if you don't want to trnasform you can use:
#wilcox test

#can use this when assumptions are not met.  







