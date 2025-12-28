# # when you run this program for the first time, you have to install thire packages
# install.packages("tictoc")
# install.packages("doParallel")
# install.packages("foreach")
# install.packages("forecast")

# Load necessary libraries
library(doParallel)
library(foreach)
library(tictoc)
library(forecast)

rm(list = ls())
periodic_based_prediction_path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/src/WseBasedPrediction.R")
source(periodic_based_prediction_path)

# Load data
data <- load_data(data_path = "/example/exampleDS.txt")
dt <- "none"
threshold_name <- "ldt"
threshold_mode <- "h"
index <- 3
init_threshold_value <- 1
prediction_percentage <- 0.5

periodic_result <- periodic_based_prediction(data, dt, threshold_name, threshold_mode, index, init_threshold_value, prediction_percentage)

quatratic_result <- quatratic_based_prediction(data, dt, threshold_name, threshold_mode, index, init_threshold_value, prediction_percentage)

arima_result <- arima_based_prediction(data, dt, threshold_name, threshold_mode, index, init_threshold_value, prediction_percentage)
