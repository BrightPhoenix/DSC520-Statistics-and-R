# Assignment: 4.2
# Name: Adams, Kimberly
# Date: 2022-07-01


## Prevent scientific notation
options(scipen = 999)

## Load libraries
library(readxl)
library(dplyr)

## Load the downloaded data from file
housing_data = read_excel('data/week-7-housing.xlsx', sheet = 'Sheet2', .name_repair = 'universal')

## Show data frame column names
colnames(housing_data)


## Using the dplyr package, use the 6 different operations to analyze/transform the data

## GroupBy + Summarize
grouped_data <- housing_data %>% group_by(zip5) %>% summarize(avg_price = mean(Sale.Price), avg_livSqFt = mean(square_feet_total_living), avg_LotSize = mean(sq_ft_lot), oldest = min(year_built), youngest = max(year_built))
View(grouped_data)

## Mutate
housing_data_added <- mutate(housing_data, PricePerSqFtLiving = Sale.Price / square_feet_total_living)
View(housing_data_added)

## Filter
FullBath3Under200k <- filter(housing_data_added, bath_full_count == 3 & Sale.Price < 200000)
View(FullBath3Under200k)

## Select
useful_columns <- housing_data %>% select(Sale.Price, square_feet_total_living, bedrooms, bath_full_count)
View(useful_columns)

## Arrange
Bedrooms_Price <- arrange(housing_data, desc(bedrooms), Sale.Price)

Bedrooms_Price_only <- Bedrooms_Price %>% select(bedrooms, Sale.Price)
View(Bedrooms_Price_only)



## Using the purrr package – perform 2 functions on your dataset. ## You could use zip_n, keep, discard, compact, etc.
library(purrr)

renovated <- discard(housing_data$year_renovated, housing_data$year_renovated == 0)
renovated

keep(housing_data$year_renovated, function(x) x > 0)

price_per_sqft <- function(a, b){return(a / b)}
housing_data$price_per_sqft_lot <- map2(housing_data$Sale.Price, housing_data$sq_ft_lot, price_per_sqft)
View(housing_data)

## Use the cbind and rbind function on your dataset
Price_YearBuilt <- cbind(housing_data$Sale.Price, housing_data$year_built)
head(Price_YearBuilt)

NewRow <- c(1, 2, 3)
NewRow2 <- c(4, 5, 6)
AddedRow <- rbind(NewRow, NewRow2)
AddedRow

## Split a string, then concatenate the results back together
sky <- "The sky is blue"
splitted <- strsplit(sky,split = " ")
splitted

sapply(splitted, paste, collapse = " ")

