# LogitModel.R -------------------------------------------------------------
# author: Ujjwol Paudel (ASU)
# assignment: ECN 753 (Industrial Organization) Assignment 1, Part I
# description: Estimating homogeneous-consumer logit model

# load relevant libraries
library(readxl) # reading excel data
library(dplyr) # grammar of data manipulation. Might first need to "install.packages("dplyr")" in the console
library(tidyverse) # transformation and better presentation of data

# read excel data
product_data = read_excel("Dropbox (ASU)/Semesters/Spr23/ECN753/Assignments/A1/product_data.xlsx")

# summary statistics
summary(product_data$product_ids)

# frequency distribution
table(product_data$market_ids)

## Checking values of a column based on another column
# Here, we check the sum of shares for each product/product_id
product_shares = product_data %>% # %>% is a mechanism for chaining commands with a new forward-pipe operator, %>%. This operator will forward a value, or the result of an expression, into the next function call/expression
  group_by(product_ids) %>% 
  summarise(num = n(), # num = n() gives the number of repetitions of each product_id
            totalShares = sum(shares))# gives the sum of shares for each product_id
print(product_shares, n=24)

# 