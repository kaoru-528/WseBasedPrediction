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
num_experiments <- 100
top_percentage <- 10  # 上位10%の平均を計算

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

      # 100回実行の結果を格納
      pmae_soft_results <- numeric(num_experiments)
      pmae_hard_results <- numeric(num_experiments)
      execution_times_soft <- character(num_experiments)
      execution_times_hard <- character(num_experiments)
      prediction_data_soft_all <- list()
      prediction_data_hard_all <- list()

      # num_experiments回実行
      for(exp in seq(1, num_experiments, by = 1)) {
        print(paste("Experiment", exp, "of", num_experiments))

        rnn_result = sliding_window_wavelet_shrinkage_prediction(data, dt, threshold_name, threshold_mode = "double", index, init_threshold_value, training_percentage, name, regression_model = "rnn")

        rnn_result_soft = rnn_result$soft
        rnn_result_hard = rnn_result$hard

        term <- length(data)
        prediction_term <- floor((1 - training_percentage) * term)

        pmae_soft <- pmae(rnn_result_soft$prediction_data, tail(data, prediction_term))
        pmae_hard <- pmae(rnn_result_hard$prediction_data, tail(data, prediction_term))

        pmae_soft_results[exp] <- pmae_soft
        pmae_hard_results[exp] <- pmae_hard
        execution_times_soft[exp] <- rnn_result_soft$execute_time$callback_msg
        execution_times_hard[exp] <- rnn_result_hard$execute_time$callback_msg
        prediction_data_soft_all[[exp]] <- rnn_result_soft$prediction_data
        prediction_data_hard_all[[exp]] <- rnn_result_hard$prediction_data
      }

      # Soft thresholding の統計計算
      sorted_indices_soft <- order(pmae_soft_results)
      sorted_pmae_soft <- pmae_soft_results[sorted_indices_soft]
      top_count <- max(1, ceiling(num_experiments * top_percentage / 100))

      best_pmae_soft <- min(pmae_soft_results)
      best_index_soft <- which.min(pmae_soft_results)
      top_x_percent_mean_soft <- mean(sorted_pmae_soft[1:top_count])
      mean_all_soft <- mean(pmae_soft_results)

      # Hard thresholding の統計計算
      sorted_indices_hard <- order(pmae_hard_results)
      sorted_pmae_hard <- pmae_hard_results[sorted_indices_hard]

      best_pmae_hard <- min(pmae_hard_results)
      best_index_hard <- which.min(pmae_hard_results)
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
                                 paste0("mean_all_", num_experiments, "runs"),
                                 "top_percentage_used")

      print(paste0("Soft - Best: ", best_pmae_soft, ", Top", top_percentage, "%: ", top_x_percent_mean_soft, ", Mean: ", mean_all_soft))
      print(paste0("Hard - Best: ", best_pmae_hard, ", Top", top_percentage, "%: ", top_x_percent_mean_hard, ", Mean: ", mean_all_hard))

      summary_data_name <- paste0(name, dataset_with_prediction_percentage, "_", "summary.txt")

      # サマリーファイルに書き出し
      write.table(summary_data, file = summary_data_name, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

      # 詳細結果も出力（オプション）
      detailed_results_soft <- data.frame(
        run_number = 1:num_experiments,
        pmae = sorted_pmae_soft,
        execution_time = execution_times_soft[sorted_indices_soft],
        rank = 1:num_experiments
      )

      detailed_results_hard <- data.frame(
        run_number = 1:num_experiments,
        pmae = sorted_pmae_hard,
        execution_time = execution_times_hard[sorted_indices_hard],
        rank = 1:num_experiments
      )

      detailed_filename_soft <- paste0(name, "detailed_", num_experiments, "runs_soft.txt")
      detailed_filename_hard <- paste0(name, "detailed_", num_experiments, "runs_hard.txt")

      write.table(detailed_results_soft, file = detailed_filename_soft, sep = "\t", row.names = FALSE, col.names = TRUE)
      write.table(detailed_results_hard, file = detailed_filename_hard, sep = "\t", row.names = FALSE, col.names = TRUE)

      # 最も低いpmaeの予測結果を保存
      best_prediction_soft_filename <- paste0(name, "best_prediction_result_soft.txt")
      best_prediction_hard_filename <- paste0(name, "best_prediction_result_hard.txt")

      write.table(
        data.frame(prediction_data = unlist(prediction_data_soft_all[[best_index_soft]])),
        file = best_prediction_soft_filename,
        row.names = FALSE,
        col.names = FALSE,
        sep = "\t"
      )

      write.table(
        data.frame(prediction_data = unlist(prediction_data_hard_all[[best_index_hard]])),
        file = best_prediction_hard_filename,
        row.names = FALSE,
        col.names = FALSE,
        sep = "\t"
      )
    }
  }
}
