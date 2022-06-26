# Assignment: American Community Survey
# Name: Adams, Kimberly
# Date: 2022-06-23

## Load the ggplot2 package
library(ggplot2)
theme_set(theme_minimal())

## Set the working directory
setwd("/Users/kimberlyadams/Documents/GitHub/DSC520-Statistics-and-R")

## Load the downloaded data from csv file
survey_results <- read.csv("data/acs-14-1yr-s0201.csv")

## Inspect the dataset
str(survey_results)
nrow(survey_results)
ncol(survey_results)

## Create a Histogram of the 'HSDegree' variable
x <- survey_results$HSDegree

hist.HSdegree <- ggplot(survey_results, aes(x)) + geom_histogram(aes(y = ..density..)) + ggtitle('Prevalence of High School Degrees within Counties') + xlab('Percentage of County Population with HS Degree') + ylab('Percentage of Counties')
hist.HSdegree

## Calculate data summary values
mean(x)
sd(x)
min(x)
max(x)

## Create a Histogram of the 'HSDegree' variable
hist.HSdegree_norm <- hist.HSdegree + stat_function(fun = dnorm, args = list(mean = mean(survey_results$HSDegree, na.rm = TRUE), sd = sd(survey_results$HSDegree, na.rm = TRUE)), col = 'red', size = 1)
hist.HSdegree_norm

## Q-Q plot of the 'HSDegree' variable
qqplot.HSdegree <- qplot(sample = survey_results$HSDegree, stat = "qq") 
qqplot.HSdegree

## Calculate descriptive statistics of the 'HSDegree' variable
library(pastecs)
stat.desc(survey_results$HSDegree, basic = TRUE, desc = TRUE, norm = TRUE, p = 0.95)
