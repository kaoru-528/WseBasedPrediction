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
periodic_based_prediction_path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/src/WseBasedPrediction.R")
source(periodic_based_prediction_path)
wavelet_shrinkage_estimation_path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/src/WaveletShrinkageEstimation.R")
source(wavelet_shrinkage_estimation_path)
evaluator_path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/src/Evaluator.R")
source(evaluator_path)

# # Load data
# data = load_data(data_path = "/example/exampleDS.txt")
# dt = "none"
# threshold_name = "ldt"
# threshold_mode = "h"
# index = 3
# init_threshold_value = 1
# training_percentage = 0.7

dataset_name_list <- list("DS1", "DS2", "DS3", "DS4", "DS5", "DS6")
training_percentage_list <- list(0.3, 0.5, 0.7)

for (i in seq(1, length(dataset_name_list), by = 1)) {
  for (j in seq(1, length(training_percentage_list), by = 1)) {
    data_path <- paste0("/DS/", dataset_name_list[[i]], ".txt")
    data <- load_data(data_path)
    training_percentage <- training_percentage_list[[j]]
    training_data <- data[1:ceiling(length(data) * training_percentage)]
    max_resolution_level <- floor(log2(length(training_data))) + 1
    pmae_result_each_resolution <- data.frame(matrix(nrow = max_resolution_level, ncol = 3))
    for (k in seq(1, max_resolution_level, by = 1)) {
      dataset_with_prediction_percentage <- paste0(dataset_name_list[[i]], "_", training_percentage_list[[j]])
      name <- paste0("./output/", dataset_with_prediction_percentage, "/", "resolution_", k, "/")
      resolution_level <- k
      regression_model <- "arima"  # "ARIMA", "LSTM"
      if (!dir.exists(name)) {
        dir.create(name, recursive = TRUE)
      }
      print(name)
      wavelet_decomposition_prediciton_result <-  tryCatch(
      {
        multi_resolution_wavelet_prediction(
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
          prediction_data = rep(NA, prediction_term),
          execute_time = list(callback_msg = NA)
          )
      }
    )
      prediction_term <- floor((1 - training_percentage) * length(data))
      pmae_wavelet <- pmae(wavelet_decomposition_prediciton_result$prediction_data, tail(data, prediction_term))
      pmae_result_each_resolution[k, 1] <- k
      pmae_result_each_resolution[k, 2] <- pmae_wavelet
      pmae_result_each_resolution[k, 3] <- wavelet_decomposition_prediciton_result$execute_time$callback_msg

      prediction_data_filename <- paste0(name, "prediction_data.txt")
      write.table(
        data.frame(prediction_data = unlist(wavelet_decomposition_prediciton_result$prediction_data)),
        file = prediction_data_filename,
        row.names = FALSE,
        col.names = FALSE,
        sep = "\t"
      )
    }
    name <- paste0("./output/", dataset_name_list[[i]], "_", training_percentage_list[[j]], "/")
    write.table(pmae_result_each_resolution, file = paste0(name, dataset_with_prediction_percentage,"_summary.txt"), sep = "\t", row.names = FALSE, col.names = c("resolution", "pmae", "execution_time"))
  }
}
