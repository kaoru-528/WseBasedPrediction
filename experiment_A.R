#  when you run this program for the first time, you have to install thire packages
# install.packages("tictoc")
# install.packages("doParallel")
# install.packages("foreach")
# install.packages("forecast")
# install.packages("keras")
# install.packages("prophet")
# install.packages("ggplot2")

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

dataset_name_list <- list("DS1", "DS2", "DS3", "DS4", "DS5", "DS6")
training_percentage_list <- list(0.3, 0.5, 0.7)
dt_threshold_name_pair <- list(c("none", "ldt"), c("A1", "ut"))

for (i in seq(1, length(dataset_name_list), by = 1)) {
  for (j in seq(1, length(dt_threshold_name_pair), by = 1)) {
    for (l in seq(1, length(training_percentage_list), by = 1)) {
      data_path <- paste0("/DS/", dataset_name_list[[i]], ".txt")
      data <- load_data(data_path)
      dt <- dt_threshold_name_pair[[j]][1]
      threshold_name <- dt_threshold_name_pair[[j]][2]
      index <- 3
      init_threshold_value <- 1
      training_percentage <- training_percentage_list[[l]]
      var <- 1
      dataset_with_prediction_percentage <- paste0(dataset_name_list[[i]], "_", dt_threshold_name_pair[[j]][1], "_", dt_threshold_name_pair[[j]][2], "_", training_percentage_list[[l]])
      name <- paste0("./output/", dataset_with_prediction_percentage, "/")
      if (!dir.exists(name)) {
        dir.create(name, recursive = TRUE)
      }
      print(name)

      # 回帰モデルの指定
      prediction_result_soft = sliding_window_wavelet_shrinkage_prediction(data, dt, threshold_name, threshold_mode = "soft", index, init_threshold_value, training_percentage, name, regression_model = "arima")
      prediction_result_hard = sliding_window_wavelet_shrinkage_prediction(data, dt, threshold_name, threshold_mode = "hard", index, init_threshold_value, training_percentage, name, regression_model = "arima")

      term <- length(data)
      prediction_term <- floor((1 - training_percentage) * term)

      pmae_soft <- pmae(prediction_result_soft$prediction_data, tail(data, prediction_term))
      pmae_hard <- pmae(prediction_result_hard$prediction_data, tail(data, prediction_term))

      output_data <- data.frame(
        pmae_soft = pmae_soft,
        prediction_result_soft = prediction_result_soft$prediction_data,
        execute_time_soft = prediction_result_soft$execute_time$callback_msg,
        pmae_hard = pmae_hard,
        prediction_result_hard = prediction_result_hard$prediction_data,
        execute_time_hard = prediction_result_hard$execute_time$callback_msg
      )

      print(paste0("pmae_soft: ", pmae_soft, ",実行時間: ", prediction_result_soft$execute_time$callback_msg))
      print(paste0("pmae_hard: ", pmae_hard, ",実行時間: ", prediction_result_hard$execute_time$callback_msg))
      summary_data_name <- paste0(name, dataset_with_prediction_percentage, "_", "summary.txt")

      # 1つのファイルに書き出す
      write.table(output_data, file = summary_data_name, row.names = FALSE, col.names = TRUE, quote = FALSE)

      # prediction_result_soft と prediction_result_hard を1つのファイルに保存
      prediction_results_filename <- paste0(name, dataset_with_prediction_percentage, "_", "prediction_results.txt")
      prediction_results_data <- data.frame(
        soft_prediction = prediction_result_soft$prediction_data,
        hard_prediction = prediction_result_hard$prediction_data
      )
      write.table(
        prediction_results_data,
        file = prediction_results_filename,
        col.names = TRUE,
        row.names = FALSE,
        sep = "\t"
      )
    }
  }
}
