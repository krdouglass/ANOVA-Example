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


# load packages
library(dplyr) # pretty much always load for filtering and manipulating data
library(Rmisc) # helpful function (summarySE) for summarizing data


# Load made up data --------------------------------------------------------

#load data
data <- read.csv (
  'C:/Users/kfake/Dropbox/Kim Fake/R Resources/Statistical Analyses Examples/ANOVA-Example/Biology 101 Data CLEAN.csv', 
  stringsAsFactors = FALSE, 
  fileEncoding = 'UTF-8-BOM'
)

#set class and sex as factors
data$Class <- as.factor(data$Class)
data$Sex <- as.factor(data$Sex)


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
# ANOVA assumes equal variances of groups
# p value must be > 0.05 to pass
bartlett.test(data$Percent_Grade, data$Class)

# helpful summary of data
data_summary <- summarySE(data, measurevar = "Percent_Grade", groupvars = "Class")
data_summary

#One-way ANOVA
OW_model <- aov(Percent_Grade ~  Class , data = data)
summary(OW_model)

OW_tukey<-TukeyHSD(OW_model)                        

OW_tukey

# This is a quicker rough way to check if
# the model fits the homoscedasticity assumption
# (assumption of equal or similar variances in different groups being compared)
par(mfrow=c(2,2))
plot(OW_model)
par(mfrow=c(1,1))

# Each plot gives a specific piece of information about the model fit, 
# but it's enough to know that the red line, representing the mean of the residuals, 
# should be horizontal and centered on zero (or on one, in the scale-location plot),
# meaning that there are no large outliers that would cause bias in the model.

# The normal Q-Q plot plots a regression between the theoretical residuals of 
# a perfectly-homoscedastic model and the actual residuals of your model, 
# so the closer to a slope of 1 this is the better. 
# This Q-Q plot is very close, with only a bit of deviation.

# summarize two-way data
# helpful summary of data
tw_data_summary <- summarySE(data, measurevar = "Percent_Grade", groupvars = c("Class", "Sex"))
tw_data_summary

#Two-way ANOVA
TW_model <- aov(Percent_Grade ~  Class + Sex , data = data)
summary(TW_model)

TW_tukey<-TukeyHSD(TW_model)                        

TW_tukey

# interaction model
# Sometimes you have reason to think that two of your independent variables
# have an interaction effect rather than an additive effect.

# To test whether two variables have an interaction effect in ANOVA, 
# simply use an asterisk instead of a plus-sign in the model
i_model <- aov(Percent_Grade ~  Class*Sex , data = data)

summary(i_model)


