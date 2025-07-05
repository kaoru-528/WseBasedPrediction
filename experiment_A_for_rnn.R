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

dataset_name_list <- list("DS1", "DS2", "DS3", "DS4", "DS5", "DS6")
training_percentage_list <- list(0.3, 0.5, 0.7)
dt_thresholdName_pair <- list(c("none", "ldt"), c("A1", "ut"))
NUM_EXPERIMENTS <- 100
top_percentage <- 10  # 上位10%の平均を計算

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

      # 100回実行の結果を格納
      pmae_soft_results <- numeric(NUM_EXPERIMENTS)
      pmae_hard_results <- numeric(NUM_EXPERIMENTS)
      execution_times_soft <- character(NUM_EXPERIMENTS)
      execution_times_hard <- character(NUM_EXPERIMENTS)
      
      # NUM_EXPERIMENTS回実行
      for(exp in seq(1, NUM_EXPERIMENTS, by = 1)) {
        print(paste("Experiment", exp, "of", NUM_EXPERIMENTS))
        
        ArimaResult_soft = LstmBasedPrediction(data, dt, thresholdName, thresholdMode = "soft", index, initThresholdvalue, training_percentage, name)
        ArimaResult_hard = LstmBasedPrediction(data, dt, thresholdName, thresholdMode = "hard", index, initThresholdvalue, training_percentage, name)

        term <- length(data)
        predictionTerm <- floor((1 - training_percentage) * term)

        pmae_soft <- pmae(ArimaResult_soft$predictionData, tail(data, predictionTerm))
        pmae_hard <- pmae(ArimaResult_hard$predictionData, tail(data, predictionTerm))

        pmae_soft_results[exp] <- pmae_soft
        pmae_hard_results[exp] <- pmae_hard
        execution_times_soft[exp] <- ArimaResult_soft$execute_time$callback_msg
        execution_times_hard[exp] <- ArimaResult_hard$execute_time$callback_msg
      }

      # Soft thresholding の統計計算
      sorted_indices_soft <- order(pmae_soft_results)
      sorted_pmae_soft <- pmae_soft_results[sorted_indices_soft]
      top_count <- max(1, ceiling(NUM_EXPERIMENTS * top_percentage / 100))
      
      best_pmae_soft <- min(pmae_soft_results)
      top_x_percent_mean_soft <- mean(sorted_pmae_soft[1:top_count])
      mean_all_soft <- mean(pmae_soft_results)

      # Hard thresholding の統計計算
      sorted_indices_hard <- order(pmae_hard_results)
      sorted_pmae_hard <- pmae_hard_results[sorted_indices_hard]
      
      best_pmae_hard <- min(pmae_hard_results)
      top_x_percent_mean_hard <- mean(sorted_pmae_hard[1:top_count])
      mean_all_hard <- mean(pmae_hard_results)

      # サマリーデータを作成
      summary_data <- data.frame(
        threshold_mode = c("soft", "hard"),
        best_pmae = c(best_pmae_soft, best_pmae_hard),
        top_x_percent_mean = c(top_x_percent_mean_soft, top_x_percent_mean_hard),
        mean_all = c(mean_all_soft, mean_all_hard),
        top_percentage_used = c(top_percentage, top_percentage)
      )

      # カラム名を動的に設定
      colnames(summary_data) <- c("threshold_mode", "best_pmae", 
                                 paste0("top", top_percentage, "%_pmae_mean"), 
                                 paste0("mean_all_", NUM_EXPERIMENTS, "runs"),
                                 "top_percentage_used")

      print(paste0("Soft - Best: ", best_pmae_soft, ", Top", top_percentage, "%: ", top_x_percent_mean_soft, ", Mean: ", mean_all_soft))
      print(paste0("Hard - Best: ", best_pmae_hard, ", Top", top_percentage, "%: ", top_x_percent_mean_hard, ", Mean: ", mean_all_hard))

      summary_data_name <- paste0(name, dataset_with_prediction_percentage, "_", "summary.txt")

      # サマリーファイルに書き出し
      write.table(summary_data, file = summary_data_name, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

      # 詳細結果も出力（オプション）
      detailed_results_soft <- data.frame(
        run_number = 1:NUM_EXPERIMENTS,
        pmae = sorted_pmae_soft,
        execution_time = execution_times_soft[sorted_indices_soft],
        rank = 1:NUM_EXPERIMENTS
      )
      
      detailed_results_hard <- data.frame(
        run_number = 1:NUM_EXPERIMENTS,
        pmae = sorted_pmae_hard,
        execution_time = execution_times_hard[sorted_indices_hard],
        rank = 1:NUM_EXPERIMENTS
      )

      detailed_filename_soft <- paste0(name, "detailed_", NUM_EXPERIMENTS, "runs_soft.txt")
      detailed_filename_hard <- paste0(name, "detailed_", NUM_EXPERIMENTS, "runs_hard.txt")
      
      write.table(detailed_results_soft, file = detailed_filename_soft, sep = "\t", row.names = FALSE, col.names = TRUE)
      write.table(detailed_results_hard, file = detailed_filename_hard, sep = "\t", row.names = FALSE, col.names = TRUE)
    }
  }
}
