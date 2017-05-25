rm(list=ls())
setwd('~/Code/Anastasiia')

# Read data
data <- read.csv('gdp.csv', header=F)
len = length(data[,])

# Read regressors
inflation <- read.csv('inflation.csv', header=F)
gas_price <- read.csv('gas_price.csv', header=F)
index_of_industrial_production <- read.csv('index_of_industrial_production.csv', header=F)
tax_revenue <- read.csv('tax_revenue.csv', header=F)


# Find minimal dimension
min_shape = list(length(data[,1]),
                  length(inflation[,1]),
                  length(gas_price[,1]),
                  length(index_of_industrial_production[,1]),
                  length(tax_revenue[,1]))

lapply(min_shape, min)
min_dimension <- min(unlist(min_shape))

# Fix data
train <- data[1:min_dimension,]

inflation <- inflation[1:min_dimension,]
gas_price <- gas_price[1:min_dimension,]
index_of_industrial_production <- index_of_industrial_production[1:min_dimension,]
tax_revenue <- tax_revenue[1:min_dimension,]
regressors <- cbind(inflation, gas_price, index_of_industrial_production, tax_revenue)

require('stats')

# ARMAX
fit <- arima(train, order=c(1, 0, 1), xreg=regressors)
show(fit)

durbin_watson <- function(model) {
  d = sum((model$residuals - lag(model$residuals))^2, na.rm = TRUE) /
    sum(model$residuals^2, na.rm = TRUE)
  return(d)
}

require('forecast')
r_square <- function(model, actual_data) {
  predicted_data <- fitted(model)
  return(cor(predicted_data, actual_data)^2)
}

# The same as r_square but self-written
r_square_alternative <- function(model, actual_data) {
  predicted_data <- fitted(model)
  SSR <- sum((predicted_data - actual_data)^2)
  SST <- sum((actual_data - rep(mean(actual_data), length(actual_data)))^2)
  return(1 - SSR/SST)
}

sse <- function(model, actual_data) {
  predicted_data <- fitted(model)
  return(sum((predicted_data - actual_data)^2))
}

# TODO Print in pritier way
show_model_metrics <- function(model, actual_data, model_title) {
  durbin_watson_test <- durbin_watson(model)
  r_square_test <- r_square(model, actual_data)
  sse_test <- sse(model, actual_data)
  
  metrics <- list("durbin_watson" = durbin_watson_test, "r_square" = r_square_test, "sse" = sse_test)
  show(metrics)
  #return(metrics)
}


# Model metrics
show_model_metrics(fit, train, "ARMAX")


# Add several more coefficients

data <- read.csv('Out.csv', header=F, sep=";")
train <- data[,1]


# AR
fit <- arima(log(train), order=c(1, 0, 0))
show(fit)

show_model_metrics(fit, train, "AR simple")


# Example of training AR
updated_fit <- auto.arima(train, max.p = 5, max.q = 0, max.d = 0)
show(updated_fit)

show_model_metrics(updated_fit, train, "AR tuned")


# ARMA
fit <- arima(train, order=c(1, 0, 1), method="CSS")
show(fit)

show_model_metrics(fit, train, "ARMA")

updated_fit <- auto.arima(train, max.p = 5, max.q = 5, max.d = 0)
show(updated_fit)

show_model_metrics(updated_fit, train, "ARMA tuned")