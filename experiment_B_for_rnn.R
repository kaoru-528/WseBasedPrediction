rm(list = ls())
library(doParallel)
library(foreach)
library(tictoc)
library(forecast)
library(ggplot2)
library(keras)

periodic_based_prediction_path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/src/WseBasedPrediction.R")
source(periodic_based_prediction_path)
wavelet_shrinkage_estimation_path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/src/WaveletShrinkageEstimation.R")
source(wavelet_shrinkage_estimation_path)
evaluator_path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/src/Evaluator.R")
source(evaluator_path)

dataset_name_list <- list("DS1", "DS2", "DS3", "DS4", "DS5", "DS6")
training_percentage_list <- list(0.3, 0.5, 0.7)
num_experiments <- 100

# 上位何%の平均を取るかを指定
top_percentage <- 10  # 上位10%の平均を計算

for (i in seq(1, length(dataset_name_list), by = 1)) {
  for (j in seq(1, length(training_percentage_list), by = 1)) {
    data_path <- paste0("/DS/", dataset_name_list[[i]], ".txt")
    data <- load_data(data_path)
    training_percentage <- training_percentage_list[[j]]
    training_data <- data[1:ceiling(length(data) * training_percentage)]
    max_resolution_level <- floor(log2(length(training_data))) + 1
    pmae_result_each_resolution <- data.frame(matrix(nrow = max_resolution_level, ncol = 4))
    
    for (k in seq(1, max_resolution_level, by = 1)) {
      dataset_with_prediction_percentage <- paste0(dataset_name_list[[i]], "_", training_percentage_list[[j]])
      name <- paste0("./output/", dataset_with_prediction_percentage, "/", "resolution_", k, "/")
      resolution_level <- k
      regression_model <- "rnn"
      if (!dir.exists(name)) {
        dir.create(name, recursive = TRUE)
      }
      print(name)
      
      # 50回実行の結果を格納
      pmae_results_50runs <- numeric(num_experiments)
      execution_times_50runs <- character(num_experiments)
      prediction_data_all <- list()
      
      for(l in seq(1, num_experiments, by = 1)) {
        wavelet_decomposition_prediciton_result <-  multi_resolution_wavelet_prediction(
            data,
            training_percentage,
            resolution_level,
            name,
            regression_model
        )

        prediction_term <- floor((1 - training_percentage) * length(data))
        pmae_50run <- pmae(wavelet_decomposition_prediciton_result$prediction_data, tail(data, prediction_term))

        pmae_results_50runs[l] <- pmae_50run
        execution_times_50runs[l] <- wavelet_decomposition_prediciton_result$execute_time$callback_msg
        prediction_data_all[[l]] <- wavelet_decomposition_prediciton_result$prediction_data
      }

      # PMEAを小さい順にソート
      sorted_indices <- order(pmae_results_50runs)
      sorted_pmae <- pmae_results_50runs[sorted_indices]
      sorted_times <- execution_times_50runs[sorted_indices]
      
      # 最も低いpmaeのインデックスを取得
      best_index <- which.min(pmae_results_50runs)

      # 上位x%の平均を計算
      top_count <- max(1, ceiling(num_experiments * top_percentage / 100))
      top_x_percent_mean <- mean(sorted_pmae[1:top_count])

      pmae_result_each_resolution[k, 1] <- k
      pmae_result_each_resolution[k, 2] <- sorted_pmae[1]
      pmae_result_each_resolution[k, 3] <- top_x_percent_mean
      pmae_result_each_resolution[k, 4] <- mean(pmae_results_50runs)

      # 50回の詳細結果をファイルに出力
      detailed_results <- data.frame(
        run_number = 1:num_experiments,
        pmae = sorted_pmae,
        execution_time = sorted_times,
        rank = 1:num_experiments
      )

      detailed_filename <- paste0(name, "detailed_", num_experiments, "runs_resolution_", k, ".txt")
      write.table(detailed_results, file = detailed_filename, sep = "\t", row.names = FALSE, col.names = TRUE)

      # 統計サマリーも出力
      summary_stats <- data.frame(
        resolution = k,
        mean_all = mean(pmae_results_50runs),
        median_all = median(pmae_results_50runs),
        min_pmae = min(pmae_results_50runs),
        max_pmae = max(pmae_results_50runs),
        top_x_percent_mean = top_x_percent_mean,
        top_percentage_used = top_percentage,
        sd = sd(pmae_results_50runs)
      )
      
      stats_filename <- paste0(name, "statistics_summary_resolution_", k, ".txt")
      write.table(summary_stats, file = stats_filename, sep = "\t", row.names = FALSE, col.names = TRUE)
      
      # 最も低いpmaeの予測結果を保存
      best_prediction_filename <- paste0(name, "best_prediction_result.txt")
      write.table(
        data.frame(prediction_data = unlist(prediction_data_all[[best_index]])),
        file = best_prediction_filename,
        row.names = FALSE,
        col.names = FALSE,
        sep = "\t"
      )
    }
    
    name <- paste0("./output/", dataset_name_list[[i]], "_", training_percentage_list[[j]], "/")
    write.table(pmae_result_each_resolution, file = paste0(name, dataset_with_prediction_percentage,"_summary.txt"), sep = "\t", row.names = FALSE, col.names = c("resolution", "best_pmae",paste0("top", top_percentage, "%_pmae_mean"), paste0("mean_all_",num_experiments,"runs")))
  }
}
