rm(list = ls())
periodicBasedPrediction_Path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/src/WseBasedPrediction.R")
source(periodicBasedPrediction_Path)
WaveletShrinkageEstimation_Path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/src/WaveletShrinkageEstimation.R")
source(WaveletShrinkageEstimation_Path)
evaluator_Path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/src/Evaluator.R")
source(evaluator_Path)

dataset_name_list <- list("DS1", "DS2", "DS3", "DS4", "DS5", "DS6")
training_percentage_list <- list(0.3, 0.5, 0.7)

# 上位何%の平均を取るかを指定
top_percentage <- 10  # 上位10%の平均を計算

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
      regression_model <- "rnn"
      if (!dir.exists(name)) {
        dir.create(name, recursive = TRUE)
      }
      print(name)
      
      # 50回実行の結果を格納
      pmae_results_50runs <- numeric(50)
      execution_times_50runs <- character(50)
      
      for(l in seq(1, 5, by = 1)) {
        wavelet_decomposition_prediciton_result <-  WaveletDecomposePrediction(
            data,
            training_percentage,
            resolution_level,
            name,
            regression_model
        )
        
        predictionTerm <- floor((1 - training_percentage) * length(data))
        pmae_50run <- pmae(wavelet_decomposition_prediciton_result$prediction_data, tail(data, predictionTerm))
        
        pmae_results_50runs[l] <- pmae_50run
        execution_times_50runs[l] <- wavelet_decomposition_prediciton_result$execute_time$callback_msg
      }
      
      # PMEAを小さい順にソート
      sorted_indices <- order(pmae_results_50runs)
      sorted_pmae <- pmae_results_50runs[sorted_indices]
      sorted_times <- execution_times_50runs[sorted_indices]
      
      # 上位x%の平均を計算
      top_count <- max(1, ceiling(50 * top_percentage / 100))
      top_x_percent_mean <- mean(sorted_pmae[1:top_count])
      
      pmae_result_each_resolution[k, 1] <- k
      pmae_result_each_resolution[k, 2] <- top_x_percent_mean
      pmae_result_each_resolution[k, 3] <- paste0("Top", top_percentage, "%_Mean")
      
      # 50回の詳細結果をファイルに出力
      detailed_results <- data.frame(
        run_number = 1:50,
        pmae = sorted_pmae,
        execution_time = sorted_times,
        rank = 1:50
      )
      
      detailed_filename <- paste0(name, "detailed_50runs_resolution_", k, ".txt")
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
    }
    
    name <- paste0("./output/", dataset_name_list[[i]], "_", training_percentage_list[[j]], "/")
    write.table(pmae_result_each_resolution, file = paste0(name, dataset_with_prediction_percentage,"_summary.txt"), sep = "\t", row.names = FALSE, col.names = c("resolution", paste0("top", top_percentage, "%_pmae_mean"), "note"))
  }
}