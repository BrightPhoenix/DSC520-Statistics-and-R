# Assignment: 4.2
# Name: Adams, Kimberly
# Date: 2022-07-01

## Load the ggplot2 package
library(ggplot2)
theme_set(theme_minimal())

## Set the working directory
setwd("/Users/kimberlyadams/Documents/GitHub/DSC520-Statistics-and-R")

## Load the downloaded data from csv file
score_results <- read.csv("data/scores.csv")
score_results$total_points <- score_results$Count * score_results$Score
score_results

## Separate out the two sections and calculate average
sports_section <- subset(score_results, score_results$Section == 'Sports')
sports_section
total_sports_students <- sum(sports_section$Count)
sports_points <- sum(sports_section$total_points)
sports_average <- sports_points / total_sports_students
sports_average

regular_section <- subset(score_results, score_results$Section == 'Regular')
regular_section
total_regular_students = sum(regular_section$Count)
regular_points <- sum(regular_section$total_points)
regular_average <- regular_points / total_regular_students
regular_average

## Graph sports section data
plot(sports_section$Score, sports_section$Count, main = 'Distribution of Scores in the Sports Section', xlab = 'Score', ylab = 'Students', xlim = c(150, 400))

## Graph regular section data
plot(regular_section$Score, regular_section$Count, main = 'Distribution of Scores in the Regular Section', xlab = 'Score', ylab = 'Students', xlim = c(150, 400))



## Housing data set cleanup
## Prevent scientific notation
options(scipen = 999)

## Load libraries
library(readxl)
library(formattable)
library(plyr)

## Load the downloaded data from file
housing_data = read_excel('data/week-7-housing.xlsx', sheet = 'Sheet2', .name_repair = 'universal')

## Show data frame column names
colnames(housing_data)

## Find mean value for useful columns
apply(housing_data[, c(2, 14, 15, 19, 20, 22)], 2, mean)

## Find mean sale price of houses by zip code
zip_mean_price <- aggregate(housing_data$Sale.Price, list(housing_data$zip5), mean)
colnames(zip_mean_price) <- c('ZipCode', 'MeanPrice')
zip_mean_price$MeanPrice <- currency(zip_mean_price$MeanPrice, digits = 0L)
zip_mean_price

## Find number of houses for sale in each zip code
zip_sales <- aggregate(housing_data$Sale.Price, by = list(housing_data$zip5), FUN = length)
colnames(zip_sales) <- c('ZipCode', 'ForSale')
zip_sales

## Find cheapest house price in each zip code
zip_min_price <- ddply(housing_data, ~ zip5, summarise, MinPrice = min(Sale.Price))
zip_min_price$MinPrice <- currency(zip_min_price$MinPrice, digits = 0L)
zip_min_price



## Check data distributions
## Sale Price
ggplot(housing_data, aes(Sale.Price)) + geom_histogram()

## Sq living space
ggplot(housing_data, aes(square_feet_total_living)) + geom_histogram()

boxplot(housing_data$square_feet_total_living, ylab = "Living Space Sq Ft")

## Year built
ggplot(housing_data, aes(year_built)) + geom_histogram()

## Year Renovated
ggplot(housing_data, aes(year_renovated)) + geom_histogram()

renovated <- subset(housing_data, housing_data$year_renovated != 0)
renovated
ggplot(renovated, aes(year_renovated)) + geom_histogram()

## Property Sq Footage
ggplot(housing_data, aes(sq_ft_lot)) + geom_histogram()


## Add new variables
housing_data$PricePerLivSqFt <- housing_data$Sale.Price / housing_data$square_feet_total_living

housing_data$PricePerPropSqFt <- housing_data$Sale.Price / housing_data$sq_ft_lot

housing_data$YearsSinceRenovation <- 2022 - housing_data$year_renovated

housing_data$HouseAge <- 2022 - housing_data$year_built

colnames(housing_data)
