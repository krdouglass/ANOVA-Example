##################################################################
# The purpose of this R script is to demonstrate an ANOVA 
# statistical analysis. This was made mostly for my own reference and a way to 
# practice these functions so it is far from perfect or comprehensive,  
# but I will share it for others who may find it helpful.
#
# Script by: Kim Fake
#
#
###################################################################


# To Clear working environment
rm(list=ls())
graphics.off()

# load packages
library(dplyr)
library(lme4)
library(car)
library(emmeans)

# Load made up data --------------------------------------------------------

#load data
data <- read.csv (
  'C:/Users/kfake/Dropbox/Kim Fake/R Resources/Statistical Analyses Examples/ANOVA-Example/Biology 101 Data CLEAN.csv', 
  stringsAsFactors = FALSE, 
  fileEncoding = 'UTF-8-BOM'
)

# subset data by class
class_1 <- data %>%
  filter(Class == 1)
class_2 <- data %>%
  filter(Class == 2)
class_3 <- data %>%
  filter(Class == 3)

# Shapiro-Wilk Test for normality
# ANOVA assumes data of each group is normally distributed
# p value must be > 0.05 to pass
shapiro.test(class_1$Percent_Grade)
shapiro.test(class_2$Percent_Grade)
shapiro.test(class_3$Percent_Grade)

# bartlett test for equal variance
# t-test assumes equal variances of groups
# p value must be > 0.05 to pass
bartlett.test(data$Percent_Grade, data$Class)

#One-way ANOVA
model <- lm(Percent_Grade ~  Class , data = data)
car::Anova(model)

emmeans(model, list(Percent_Grade ~  Class), adjust = "tukey")
