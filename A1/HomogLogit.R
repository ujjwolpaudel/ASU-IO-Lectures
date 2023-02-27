# Homogeneous Consumer Logit Estimation -------------------------------------------------------------
# author: Ujjwol Paudel (ASU)
# assignment: ECN 753 (Industrial Organization) Assignment 1, Part I

# load relevant libraries
library(readxl) # reading excel data
library(dplyr) # grammar of data manipulation. Might first need to "install.packages("dplyr")" in the console
library(tidyverse) # transformation and better presentation of data
library(stargazer) # for results
library(AER) # for 2sls

# read excel data
product_data = read_excel("Dropbox (ASU)/Semesters/Spr23/ECN753/Assignments/A1/product_data.xlsx")
agent_data = read_excel("Dropbox (ASU)/Semesters/Spr23/ECN753/Assignments/A1/agent_data.xlsx")

# summary statistics
summary(product_data$product_ids)

# frequency distribution
table(product_data$market_ids)

## Checking values of a column based on another column
# Here, we check the sum of shares for each product/product_id in each market
# %>% is a mechanism for chaining commands with a new forward-pipe operator, %>%. This operator will forward a value, or the result of an expression, into the next function call/expression
product_shares = product_data %>%
  group_by(market_ids) %>%
  summarise(insideShares = sum(shares), # gives the sum of product shares of inside goods for each market_id
            outsideShares = 1-insideShares, # Share of the outside option in each market
            logOutsideShares = log(outsideShares)) # log of shares of outside option in each market

# merging two dataframes by market_ids
product_data <- merge(product_data, product_shares, by = "market_ids")


## checking that agent weights sum to one in each market
agent_shares = agent_data %>%
  group_by(market_ids) %>% 
  summarise(num = n(), # num = n() gives the number of repetitions of each product_id
            totalShares = sum(weights))# gives the sum of shares for each product_id

###### 1.a. Model estimation using OLS ######
product_data$logInsideShares = log(product_data$shares) # log of market share of individual products
model_ols <- lm(logInsideShares-logOutsideShares~prices + sugar, data=product_data)
stargazer(model_ols, 
          digits = 3,
          header = FALSE,
          type = "text", # type = "latex" to generate the latex code of the table 
          title = "Estimated Parameters using OLS",
          model.numbers = FALSE,
          column.labels = c("(1)"))

###### 1.b. Model estimation using 2SLS based on 19 demand_instruments ######
model_2sls <- ivreg(logInsideShares-logOutsideShares ~ 
                      sugar + prices | 
                      sugar + 
                      demand_instruments0 + 
                      demand_instruments1 + 
                      demand_instruments2 + 
                      demand_instruments3 + 
                      demand_instruments4 + 
                      demand_instruments5 + 
                      demand_instruments6 + 
                      demand_instruments7 + 
                      demand_instruments8 + 
                      demand_instruments9 + 
                      demand_instruments10 + 
                      demand_instruments11 + 
                      demand_instruments12 + 
                      demand_instruments12 + 
                      demand_instruments13 + 
                      demand_instruments14 + 
                      demand_instruments15 + 
                      demand_instruments16 + 
                      demand_instruments17 + 
                      demand_instruments18 + 
                      demand_instruments19, data = product_data)
stargazer(model_2sls, 
          digits = 3,
          header = FALSE,
          type = "text", # type = "latex" to generate the latex code of the table 
          title = "Estimated Parameters using 2SLS",
          model.numbers = FALSE,
          column.labels = c("(1)"))


##### 1.c. Own-price elasticities for each product in the market ‘C01Q1’

# Keeping only the observations for  ‘C01Q1’ and only the columns market_ids, product_ids, shares, and prices
df_C01Q1 <- product_data[product_data$market_ids == 'C01Q1', c("market_ids", "product_ids", "shares", "prices")]

# Computing the own price elasticities for each product in the market
df_C01Q1$ownPriceElast = coef(model_2sls)["prices"] * df_C01Q1$prices * (1-df_C01Q1$shares)

# Make scatter plot
plot(df_C01Q1$prices, df_C01Q1$ownPriceElast, main = "Scatter plot of prices and own-price elasticities", xlab = "Prices", ylab = "Own-price elasticities")  

"We observe a that own-price elasticities for products in market 'C01Q1' is a downward sloping function of prices, and that
more expensive products are more price elastic."