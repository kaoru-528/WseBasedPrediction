WaveletShrinkageEstimation_Path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/src/WaveletShrinkageEstimation.R")
source(WaveletShrinkageEstimation_Path)
Regression_Path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/src/RegressionModule.R")
source(Regression_Path)
CreateGraph_Path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/src/CreateGraph.R")
source(CreateGraph_Path)

# 周期関数を用いた予測
PeriodicBasedPrediction <- function(data, dt, thresholdName, thresholdMode, index, initThresholdvalue, training_percentage) {
    term <- length(data)
    # wseを実行する必要がないが、メソッドを切り出すのがめんどくさいのでcoefficientsを取得するために実行
    wsed_data <- wse(data = data, dt = dt, thresholdName = thresholdName, thresholdMode = thresholdMode, index = index, initThresholdvalue = initThresholdvalue)
    prediction_term <- floor((1 - training_percentage) * term)
    cs <- wsed_data$cs
    ds <- wsed_data$ds
    coeLength <- length(cs)
    coe <- prepare_data(cs, ds, prediction_term)

    tic()
    sorted_best_coe <- run_parallel_regression(coe, coeLength, prediction_term, "periodic")
    time <- toc()

    y <- c(1:coeLength)
    C_4_1 <- periodic_function(y, sorted_best_coe[[1]]$a[[1]], sorted_best_coe[[1]]$b[[1]], sorted_best_coe[[1]]$c[[1]], sorted_best_coe[[1]]$d[[1]])
    D_1_1 <- periodic_function(y, sorted_best_coe[[2]]$a[[1]], sorted_best_coe[[2]]$b[[1]], sorted_best_coe[[2]]$c[[1]], sorted_best_coe[[2]]$d[[1]])
    D_1_2 <- periodic_function(y, sorted_best_coe[[3]]$a[[1]], sorted_best_coe[[3]]$b[[1]], sorted_best_coe[[3]]$c[[1]], sorted_best_coe[[3]]$d[[1]])
    D_1_3 <- periodic_function(y, sorted_best_coe[[4]]$a[[1]], sorted_best_coe[[4]]$b[[1]], sorted_best_coe[[4]]$c[[1]], sorted_best_coe[[4]]$d[[1]])
    D_1_4 <- periodic_function(y, sorted_best_coe[[5]]$a[[1]], sorted_best_coe[[5]]$b[[1]], sorted_best_coe[[5]]$c[[1]], sorted_best_coe[[5]]$d[[1]])
    D_2_1 <- periodic_function(y, sorted_best_coe[[6]]$a[[1]], sorted_best_coe[[6]]$b[[1]], sorted_best_coe[[6]]$c[[1]], sorted_best_coe[[6]]$d[[1]])
    D_2_2 <- periodic_function(y, sorted_best_coe[[7]]$a[[1]], sorted_best_coe[[7]]$b[[1]], sorted_best_coe[[7]]$c[[1]], sorted_best_coe[[7]]$d[[1]])
    D_3_1 <- periodic_function(y, sorted_best_coe[[8]]$a[[1]], sorted_best_coe[[8]]$b[[1]], sorted_best_coe[[8]]$c[[1]], sorted_best_coe[[8]]$d[[1]])

    for (k in seq(coeLength - prediction_term + 1, coeLength, by = 1)) {
        Cs[[k]][[4]][1] <- C_4_1[k]

        Ds[[k]][[2]][1] <- D_1_1[[k]]
        Ds[[k]][[2]][2] <- D_1_2[[k]]
        Ds[[k]][[2]][3] <- D_1_3[[k]]
        Ds[[k]][[2]][4] <- D_1_4[[k]]
        Ds[[k]][[3]][1] <- D_2_1[[k]]
        Ds[[k]][[3]][2] <- D_2_2[[k]]
        Ds[[k]][[4]][1] <- D_3_1[[k]]
    }

    denoised_ds <- ThresholdForGroups(Ds = Ds, thresholdMode = thresholdMode, thresholdName = thresholdName, dt = dt, groups = 0, initThresholdvalue = 1)
    i_groups <- inverseHaarWaveletTransformForGroups(Cs, denoised_ds)
    i_groups <- lapply(i_groups, function(x) x * 8^0.5)
    all_data <- movingAverage(i_groups, term)
    all_data <- apply_inverse_transform(all_data, dt, var)
    all_data <- pmax(all_data, 0)

    best_coe <- data.frame(a = numeric(), b = numeric(), c = numeric(), d = numeric())
    for (m in seq(1, 8, by = 1)) {
        tmp_best_coe <- data.frame(a = sorted_best_coe[[m]]$a[[1]], b = sorted_best_coe[[m]]$b[[1]], c = sorted_best_coe[[m]]$c[[1]], d = sorted_best_coe[[m]]$d[[1]])
        best_coe <- rbind(best_coe, tmp_best_coe)
    }
    predictionData <- list(predictionData = tail(all_data, prediction_term), regressionCoefficient = best_coe)
    return(predictionData)
}

# ARIMAを用いた予測
ArimaBasedPrediction <- function(data, dt, thresholdName, thresholdMode, index, initThresholdvalue, training_percentage, name) {
    term <- length(data)
    wsed_data <- wse(data = data, dt = dt, thresholdName = thresholdName, thresholdMode = thresholdMode, index = index, initThresholdvalue = initThresholdvalue, var = 1)
    prediction_term <- floor((1 - training_percentage) * term)
    cs <- wsed_data$cs
    ds <- wsed_data$ds
    coeLength <- length(cs)
    all_coefficients_data <- get_all_coefficients_data(cs, ds)
    coefficients_data_for_training <- prepare_data(cs, ds, prediction_term)

    tic()
    prediction_result <- run_parallel_arima_regression(coefficients_data_for_training, prediction_term)
    time <- toc()
    CreateGraphForArimaBasedPrediction(prediction_result, all_coefficients_data, coeLength, prediction_term, name)

    y <- c(1:coeLength)
    C_4_1 <- c(unlist(coefficients_data_for_training[[1]]), prediction_result[[1]]$mean)
    D_1_1 <- c(unlist(coefficients_data_for_training[[2]]), prediction_result[[2]]$mean)
    D_1_2 <- c(unlist(coefficients_data_for_training[[3]]), prediction_result[[3]]$mean)
    D_1_3 <- c(unlist(coefficients_data_for_training[[4]]), prediction_result[[4]]$mean)
    D_1_4 <- c(unlist(coefficients_data_for_training[[5]]), prediction_result[[5]]$mean)
    D_2_1 <- c(unlist(coefficients_data_for_training[[6]]), prediction_result[[6]]$mean)
    D_2_2 <- c(unlist(coefficients_data_for_training[[7]]), prediction_result[[7]]$mean)
    D_3_1 <- c(unlist(coefficients_data_for_training[[8]]), prediction_result[[8]]$mean)

    for (k in seq(coeLength - prediction_term + 1, coeLength, by = 1)) {
        cs[[k]][[4]][1] <- C_4_1[k]

        ds[[k]][[2]][1] <- D_1_1[[k]]
        ds[[k]][[2]][2] <- D_1_2[[k]]
        ds[[k]][[2]][3] <- D_1_3[[k]]
        ds[[k]][[2]][4] <- D_1_4[[k]]
        ds[[k]][[3]][1] <- D_2_1[[k]]
        ds[[k]][[3]][2] <- D_2_2[[k]]
        ds[[k]][[4]][1] <- D_3_1[[k]]
    }


    denoised_ds <- ThresholdForGroups(Ds = ds, thresholdMode = thresholdMode, thresholdName = thresholdName, dt = dt, groups = 0, initThresholdvalue = 1)
    i_groups <- inverseHaarWaveletTransformForGroups(cs, denoised_ds)
    i_groups <- lapply(i_groups, function(x) x * 8^0.5)
    all_data <- movingAverage(i_groups, term)
    all_data <- apply_inverse_transform(all_data, dt, var)
    all_data <- pmax(all_data, 0)

    PredictionData <- list(predictionData = tail(all_data, prediction_term), execute_time = time)
    return(PredictionData)
}

LstmBasedPrediction <- function(data, dt, thresholdName, thresholdMode, index, initThresholdvalue, training_percentage, name) {
    term <- length(data)
    wsed_data <- wse(data = data, dt = dt, thresholdName = thresholdName, thresholdMode = thresholdMode, index = index, initThresholdvalue = initThresholdvalue)
    prediction_term <- floor((1 - training_percentage) * term)
    cs <- wsed_data$cs
    ds <- wsed_data$ds
    coeLength <- length(cs)
    all_coefficients_data <- get_all_coefficients_data(cs, ds)
    coefficients_data_for_training <- prepare_data(cs, ds, prediction_term)
    tic()
    prediction_result <- list()
    for (j in seq(1, 8, by = 1)) {
        training_data <- unlist(coefficients_data_for_training[[j]])
        prediction_result_each_resolution_level <- run_lstm_regression(training_data, prediction_term)
        prediction_result[[j]] <- data.frame(mean = prediction_result_each_resolution_level)
    }
    time <- toc()
    CreateGraphForArimaBasedPrediction(prediction_result, all_coefficients_data, coeLength, prediction_term)
    y <- c(1:coeLength)
    C_4_1 <- c(unlist(coefficients_data_for_training[[1]]), prediction_result[[1]]$mean)
    D_1_1 <- c(unlist(coefficients_data_for_training[[2]]), prediction_result[[2]]$mean)
    D_1_2 <- c(unlist(coefficients_data_for_training[[3]]), prediction_result[[3]]$mean)
    D_1_3 <- c(unlist(coefficients_data_for_training[[4]]), prediction_result[[4]]$mean)
    D_1_4 <- c(unlist(coefficients_data_for_training[[5]]), prediction_result[[5]]$mean)
    D_2_1 <- c(unlist(coefficients_data_for_training[[6]]), prediction_result[[6]]$mean)
    D_2_2 <- c(unlist(coefficients_data_for_training[[7]]), prediction_result[[7]]$mean)
    D_3_1 <- c(unlist(coefficients_data_for_training[[8]]), prediction_result[[8]]$mean)
    for (k in seq(coeLength - prediction_term + 1, coeLength, by = 1)) {
        cs[[k]][[4]][1] <- C_4_1[k]

        ds[[k]][[2]][1] <- D_1_1[[k]]
        ds[[k]][[2]][2] <- D_1_2[[k]]
        ds[[k]][[2]][3] <- D_1_3[[k]]
        ds[[k]][[2]][4] <- D_1_4[[k]]
        ds[[k]][[3]][1] <- D_2_1[[k]]
        ds[[k]][[3]][2] <- D_2_2[[k]]
        ds[[k]][[4]][1] <- D_3_1[[k]]
    }
    denoised_ds <- ThresholdForGroups(Ds = ds, thresholdMode = thresholdMode, thresholdName = thresholdName, dt = dt, groups = 0, initThresholdvalue = 1)
    i_groups <- inverseHaarWaveletTransformForGroups(cs, denoised_ds)
    i_groups <- lapply(i_groups, function(x) x * 8^0.5)
    all_data <- movingAverage(i_groups, term)
    all_data <- apply_inverse_transform(all_data, dt, var = 1)
    all_data <- pmax(all_data, 0)
    PredictionData <- list(predictionData = tail(all_data, prediction_term), execute_time = time)
    return(PredictionData)
}

# 二次関数を用いた予測
QuatraticBasedPrediction <- function(data, dt, thresholdName, thresholdMode, index, initThresholdvalue, training_percentage) {
    term <- length(data)
    data <- wse(data = data, dt = dt, thresholdName = thresholdName, thresholdMode = thresholdMode, index = index, initThresholdvalue = initThresholdvalue)
    prediction_term <- floor((1 - training_percentage) * term)
    Cs <- data$cs
    Ds <- data$ds
    dDs <- data$denoisedDs
    coeLength <- length(Cs)
    coe <- prepare_data(data$cs, data$ds, prediction_term)

    tic()
    sorted_best_coe <- run_parallel_regression(coe, coeLength, prediction_term, "quadratic")
    time <- toc()

    y <- c(1:(term%/%8 + 1))
    C_4_1 <- quadratic_function(y, sorted_best_coe[[1]]$a[[1]], sorted_best_coe[[1]]$b[[1]], sorted_best_coe[[1]]$c[[1]])
    D_1_1 <- quadratic_function(y, sorted_best_coe[[2]]$a[[1]], sorted_best_coe[[2]]$b[[1]], sorted_best_coe[[2]]$c[[1]])
    D_1_2 <- quadratic_function(y, sorted_best_coe[[3]]$a[[1]], sorted_best_coe[[3]]$b[[1]], sorted_best_coe[[3]]$c[[1]])
    D_1_3 <- quadratic_function(y, sorted_best_coe[[4]]$a[[1]], sorted_best_coe[[4]]$b[[1]], sorted_best_coe[[4]]$c[[1]])
    D_1_4 <- quadratic_function(y, sorted_best_coe[[5]]$a[[1]], sorted_best_coe[[5]]$b[[1]], sorted_best_coe[[5]]$c[[1]])
    D_2_1 <- quadratic_function(y, sorted_best_coe[[6]]$a[[1]], sorted_best_coe[[6]]$b[[1]], sorted_best_coe[[6]]$c[[1]])
    D_2_2 <- quadratic_function(y, sorted_best_coe[[7]]$a[[1]], sorted_best_coe[[7]]$b[[1]], sorted_best_coe[[7]]$c[[1]])
    D_3_1 <- quadratic_function(y, sorted_best_coe[[8]]$a[[1]], sorted_best_coe[[8]]$b[[1]], sorted_best_coe[[8]]$c[[1]])

    for (k in seq(1, term%/%8 + 1, by = 1)) {
        Cs[[k]][[4]][1] <- C_4_1[k]

        dDs[[k]][[2]][1] <- D_1_1[[k]]
        dDs[[k]][[2]][2] <- D_1_2[[k]]
        dDs[[k]][[2]][3] <- D_1_3[[k]]
        dDs[[k]][[2]][4] <- D_1_4[[k]]
        dDs[[k]][[3]][1] <- D_2_1[[k]]
        dDs[[k]][[3]][2] <- D_2_2[[k]]
        dDs[[k]][[4]][1] <- D_3_1[[k]]
    }
    i_groups <- inverseHaarWaveletTransformForGroups(Cs, dDs)
    i_groups <- lapply(i_groups, function(x) x * 8^0.5)
    all_data <- movingAverage(i_groups, term)
    all_data <- apply_inverse_transform(all_data, dt, var = 1)
    all_data <- pmax(all_data, 0)

    best_coe <- data.frame(a = numeric(), b = numeric(), c = numeric())
    for (m in seq(1, 8, by = 1)) {
        tmp_best_coe <- data.frame(a = sorted_best_coe[[m]]$a[[1]], b = sorted_best_coe[[m]]$b[[1]], c = sorted_best_coe[[m]]$c[[1]])
        best_coe <- rbind(best_coe, tmp_best_coe)
    }
    predictionData <- list(predictionData = tail(all_data, prediction_term), regressionCoefficient = best_coe, execute_time = time)
    return(predictionData)
}



WaveletDecomposePrediction <- function(data, training_percentage, resolution_level, name, regression_model) {
    training_data <- data[1:ceiling(length(data) * training_percentage)]
    prediction_term <- floor((1 - training_percentage) * length(data))

    coefficients <- calculate_wavelet_and_scaling_coefficients(training_data, resolution_level)
    wavelet_coefficients <- coefficients$wavelet_coefficients
    scaling_coefficients <- coefficients$scaling_coefficients

    all_coefficients <- calculate_wavelet_and_scaling_coefficients(data, resolution_level)
    all_wavelet_coefficients <- all_coefficients$wavelet_coefficients
    all_scaling_coefficients <- all_coefficients$scaling_coefficients

    # 学習データの作成 wavelet係数とスケーリング係数を結合
    trading_coefficients <- list()
    all_coefficients_data <- list()
    for (i in seq(1, resolution_level + 1)) {
        if (i == 1) {
            trading_coefficients[[i]] <- wavelet_coefficients[[resolution_level + 1]]
            all_coefficients_data[[i]] <- all_wavelet_coefficients[[resolution_level + 1]]
        } else {
            trading_coefficients[[i]] <- scaling_coefficients[[i]]
            all_coefficients_data[[i]] <- all_scaling_coefficients[[i]]
        }
    }

    # 予測期間を作成
    coefficients_prediction_term_list <- list()
    tmp_variant <- 2^ceiling(log2(prediction_term))
    if (2^(resolution_level + 1) > tmp_variant) {
        tmp_variant <- 2^(resolution_level + 1)
    }
    for (i in seq(1, resolution_level + 1)) {
        if (i != resolution_level + 1) {
            coefficients_prediction_term_list[[i + 1]] <- tmp_variant
        } else {
            coefficients_prediction_term_list[[1]] <- tmp_variant
        }
        tmp_variant <- tmp_variant/2
    }

    tic()
    if (regression_model == "arima") {
        num_cores <- detectCores()
        cl <- makeCluster(num_cores)
        registerDoParallel(cl)
        prediction_result <- foreach(j = seq(1, resolution_level + 1), .packages = c("forecast")) %dopar% {
            training_data <- unlist(trading_coefficients[[j]])

            # ARIMAモデルの適用
            fit <- auto.arima(training_data, stepwise = FALSE, approximation = FALSE, seasonal = FALSE, trace = TRUE)

            # 予測
            forecasted <- forecast(fit, h = coefficients_prediction_term_list[[j]])  # 予測
            return(forecasted)
        }
        stopCluster(cl)
    } else if (regression_model == "periodic") {
        num_cores <- detectCores()
        cl <- makeCluster(num_cores)
        registerDoParallel(cl)
        sorted_best_coe_list <- foreach(j = seq(1, resolution_level + 1), .packages = c("stats"), .export = c("run_regression_for_periodic_function", "periodic_function")) %dopar% {
            run_regression_for_periodic_function(j, trading_coefficients[[j]])
        }
        prediction_result <- list()
        for (j in seq(1, resolution_level + 1)) {
            y <- c(1:coefficients_prediction_term_list[[j]])
            prediction_result[[j]] <- data.frame(mean = periodic_function(y, sorted_best_coe_list[[j]]$a[[1]], sorted_best_coe_list[[j]]$b[[1]], sorted_best_coe_list[[j]]$c[[1]], sorted_best_coe_list[[j]]$d[[1]]))
        }
        stopCluster(cl)
    } else if (regression_model == "lstm") {
        prediction_result <- list()
        for (j in seq(1, resolution_level + 1)) {
            training_data <- unlist(trading_coefficients[[j]])
            prediction_result_each_resolution_level <- run_lstm_regression(training_data, coefficients_prediction_term_list[[j]])
            prediction_result[[j]] <- data.frame(mean = prediction_result_each_resolution_level)
        }
    }

    time <- toc()

    CreateGraphForWaveleDecomposePrediction(prediction_result, all_coefficients_data, length(data), prediction_term, name, resolution_level)

    prediction_wavelet <- list()
    prediction_scaling <- list()

    for (i in seq(1, resolution_level + 1)) {
        if (i == 1) {
            prediction_wavelet[[i]] <- as.numeric(prediction_result[[i]]$mean)
        } else {
            prediction_scaling[[i]] <- as.numeric(prediction_result[[i]]$mean)
        }
    }

    inverse_data <- unlist(prediction_wavelet)
    for (i in seq(resolution_level + 1, 2, by = -1)) {
        a <- list()
        for (j in seq(1, length(prediction_scaling[[i]]), by = 1)) {
            if (j == 1) {
                a <- inverse_data[ceiling(j/2)] + prediction_scaling[[i]][j]
            } else {
                a <- append(a, inverse_data[ceiling(j/2)] + prediction_scaling[[i]][j])
            }
        }
        inverse_data <- a
    }
    prediction_result <- list(prediction_data = inverse_data[1:prediction_term], execute_time = time)
    return(prediction_result)
}

# private

get_all_coefficients_data <- function(Cs, Ds) {
    tmp_Cs_4_1 <- list()
    tmp_Ds <- vector("list", 7)  # D[1][1] ~ D[3][1]

    for (j in seq(1, length(Ds), by = 1)) {
        tmp_Cs_4_1 <- c(tmp_Cs_4_1, Cs[[j]][[4]][1])
        for (i in 1:7) {
            tmp_Ds[[i]] <- c(tmp_Ds[[i]], Ds[[j]][[ceiling(i/4)]][(i - 1)%%4 + 1])
        }
    }

    coe <- c(list(tmp_Cs_4_1), tmp_Ds)
    return(coe)
}

prepare_data <- function(Cs, Ds, prediction_term) {
    tmp_Cs_4_1 <- list()
    tmp_Ds <- vector("list", 7)  # D[1][1] ~ D[3][1]

    for (j in seq(1, length(Ds) - prediction_term, by = 1)) {
        tmp_Cs_4_1 <- c(tmp_Cs_4_1, Cs[[j]][[4]][1])
        for (i in 1:7) {
            tmp_Ds[[i]] <- c(tmp_Ds[[i]], Ds[[j]][[ceiling(i/4)]][(i - 1)%%4 + 1])
        }
    }

    coe <- c(list(tmp_Cs_4_1), tmp_Ds)
    return(coe)
}

apply_inverse_transform <- function(all_data, dt, var) {
    if (dt == "A1") {
        return(inverseAnscombeTransformFromGroup(all_data, var))
    } else if (dt == "A2") {
        return(inverseAnscombeTransform2FromGroup(all_data, var))
    } else if (dt == "A3") {
        return(inverseAnscombeTransform3FromGroup(all_data, var))
    } else if (dt == "B1") {
        return(inverseBartlettTransformFromGroup(all_data, var))
    } else if (dt == "B2") {
        return(inverseBartlettTransform2FromGroup(all_data, var))
    } else if (dt == "Fr") {
        return(inverseFreemanTransformFromGroup(all_data, var))
    } else {
        return(all_data)
    }
}

calculate_wavelet_and_scaling_coefficients <- function(training_data, resolution_level) {
    wavelet_coefficients <- list()
    scaling_coefficients <- list()

    # 初期化
    wavelet_coefficients[[1]] <- training_data

    # Wavelet変換とスケーリング係数の計算
    for (i in 1:resolution_level) {
        if (length(wavelet_coefficients[[i]])%%2 != 0) {
            wavelet_coefficients[[i]][length(wavelet_coefficients[[i]]) + 1] <- wavelet_coefficients[[i]][length(wavelet_coefficients[[i]])]
        }
        for (j in seq(1, length(wavelet_coefficients[[i]]) - 1, by = 2)) {
            if (j == 1) {
                wavelet_coefficients[[i + 1]] <- (wavelet_coefficients[[i]][j] + wavelet_coefficients[[i]][j + 1])/2
                tmp <- (wavelet_coefficients[[i]][j] + wavelet_coefficients[[i]][j + 1])/2
                scaling_coefficients[[i + 1]] <- wavelet_coefficients[[i]][j] - tmp
                scaling_coefficients[[i + 1]] <- c(scaling_coefficients[[i + 1]], wavelet_coefficients[[i]][j + 1] - tmp)
            } else {
                wavelet_coefficients[[i + 1]] <- c(wavelet_coefficients[[i + 1]], (wavelet_coefficients[[i]][j] + wavelet_coefficients[[i]][j + 1])/2)
                tmp <- (wavelet_coefficients[[i]][j] + wavelet_coefficients[[i]][j + 1])/2
                scaling_coefficients[[i + 1]] <- c(scaling_coefficients[[i + 1]], wavelet_coefficients[[i]][j] - tmp)
                scaling_coefficients[[i + 1]] <- c(scaling_coefficients[[i + 1]], wavelet_coefficients[[i]][j + 1] - tmp)
            }
        }
    }

    return(list(wavelet_coefficients = wavelet_coefficients, scaling_coefficients = scaling_coefficients))
}
FALSE
