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

periodicBasedPrediction_Path = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/src/wseBasedPrediction.R")
source(periodicBasedPrediction_Path)

# Load data
data = loadData(dataPath = "/example/exampleDS.txt")
dt = "none"
thresholdName = "ldt"
thresholdMode = "h"
index = 3
initThresholdvalue = 1
predictionPercentage = 0.5

PeriodicResult = PeriodicBasedPrediction(data, dt, thresholdName, thresholdMode, index, initThresholdvalue, predictionPercentage, term)

QuatraticResult = QuatraticBasedPrediction(data, dt, thresholdName, thresholdMode, index, initThresholdvalue, predictionPercentage, term)