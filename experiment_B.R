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
library(ggplot2)
library(keras)
library(prophet)

rm(list = ls())
periodicBasedPrediction_Path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/src/WseBasedPrediction.R")
source(periodicBasedPrediction_Path)
WaveletShrinkageEstimation_Path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/src/WaveletShrinkageEstimation.R")
source(WaveletShrinkageEstimation_Path)
evaluator_Path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/src/Evaluator.R")
source(evaluator_Path)

# # Load data
# data = loadData(dataPath = "/example/exampleDS.txt")
# dt = "none"
# thresholdName = "ldt"
# thresholdMode = "h"
# index = 3
# initThresholdvalue = 1
# training_percentage = 0.7

dataset_name_list <- list("DS1", "DS2", "DS3", "DS4", "DS5", "DS6")
training_percentage_list <- list(0.3, 0.5, 0.7)

for (i in seq(1, length(dataset_name_list), by = 1)) {
  for (j in seq(1, length(training_percentage_list), by = 1)) {
    dataPath <- paste0("/DS/", dataset_name_list[[i]], ".txt")
    data <- loadData(dataPath)
    training_percentage <- training_percentage_list[[j]]
    training_data <- data[1:ceiling(length(data) * training_percentage)]
    max_resolution_level <- floor(log2(length(training_data))) + 1
    pmae_result_each_resolution <- data.frame(matrix(nrow = max_resolution_level, ncol = 3))
    for (k in seq(1, max_resolution_level, by = 1)) {
      dataset_with_prediction_percentage <- paste0(dataset_name_list[[i]], "_", training_percentage_list[[j]])
      name <- paste0("./output/", dataset_with_prediction_percentage, "/", "resolution_", k, "/")
      resolution_level <- k
      regression_model <- "prophet"
      if (!dir.exists(name)) {
        dir.create(name, recursive = TRUE)
      }
      print(name)
      wavelet_decomposition_prediciton_result <-  tryCatch(
      {
        WaveletDecomposePrediction(
          data,
          training_percentage,
          resolution_level,
          name,
          regression_model
        )
      },
      error = function(e) {
        warning(paste("LSTM回帰失敗:", e$message))
        list(
          prediction_data = rep(NA, predictionTerm),
          execute_time = list(callback_msg = NA)
          )
      }
    )
      predictionTerm <- floor((1 - training_percentage) * length(data))
      pmae_wavelet <- pmae(wavelet_decomposition_prediciton_result$prediction_data, tail(data, predictionTerm))
      pmae_result_each_resolution[k, 1] <- k
      pmae_result_each_resolution[k, 2] <- pmae_wavelet
      pmae_result_each_resolution[k, 3] <- wavelet_decomposition_prediciton_result$execute_time$callback_msg
    }
    name <- paste0("./output/", dataset_name_list[[i]], "_", training_percentage_list[[j]], "/")
    write.table(pmae_result_each_resolution, file = paste0(name, dataset_with_prediction_percentage,"_summary.txt"), sep = "\t", row.names = FALSE, col.names = c("resolution", "pmae", "execution_time"))
  }
}
