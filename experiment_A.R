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
dt_thresholdName_pair <- list(c("none", "ldt"), c("A1", "ut"))

# PeriodicResult = PeriodicBasedPrediction(data, dt, thresholdName, thresholdMode, index, initThresholdvalue, training_percentage)

# QuatraticResult = QuatraticBasedPrediction(data, dt, thresholdName, thresholdMode, index, initThresholdvalue, training_percentage)\

# prediction_result_soft = ArimaBasedPrediction(data, dt, thresholdName, thresholdMode = "soft", index, initThresholdvalue, training_percentage)
for (i in seq(1, length(dataset_name_list), by = 1)) {
  for (j in seq(1, length(dt_thresholdName_pair), by = 1)) {
    for (l in seq(1, length(training_percentage_list), by = 1)) {
      dataPath <- paste0("/DS/", dataset_name_list[[i]], ".txt")
      data <- loadData(dataPath)
      dt <- dt_thresholdName_pair[[j]][1]
      thresholdName <- dt_thresholdName_pair[[j]][2]
      index <- 3
      initThresholdvalue <- 1
      training_percentage <- training_percentage_list[[l]]
      var <- 1
      dataset_with_prediction_percentage <- paste0(dataset_name_list[[i]], "_", dt_thresholdName_pair[[j]][1], "_", dt_thresholdName_pair[[j]][2], "_", training_percentage_list[[l]])
      name <- paste0("./output/", dataset_with_prediction_percentage, "/")
      if (!dir.exists(name)) {
        dir.create(name, recursive = TRUE)
      }
      print(name)
      prediction_result_soft = WaveletSlidingWindowPrediction(data, dt, thresholdName, thresholdMode = "soft", index, initThresholdvalue, training_percentage, name, regression_model = "periodic")
      prediction_result_hard = WaveletSlidingWindowPrediction(data, dt, thresholdName, thresholdMode = "hard", index, initThresholdvalue, training_percentage, name, regression_model = "periodic")

      term <- length(data)
      predictionTerm <- floor((1 - training_percentage) * term)

      pmae_soft <- pmae(prediction_result_soft$predictionData, tail(data, predictionTerm))
      pmae_hard <- pmae(prediction_result_hard$predictionData, tail(data, predictionTerm))

      output_data <- data.frame(
        pmae_soft = pmae_soft,
        prediction_result_soft = prediction_result_soft$predictionData,
        execute_time_soft = prediction_result_soft$execute_time$callback_msg,
        pmae_hard = pmae_hard,
        prediction_result_hard = prediction_result_hard$predictionData,
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
        soft_prediction = prediction_result_soft$predictionData,
        hard_prediction = prediction_result_hard$predictionData
      )
      write.table(
        prediction_results_data,
        file = prediction_results_filename,
        row.names = FALSE,
        col.names = TRUE,
        sep = "\t"
      )
    }
  }
}