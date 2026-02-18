wavelet_shrinkage_estimation_path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/src/WaveletShrinkageEstimation.R")
source(wavelet_shrinkage_estimation_path)
regression_path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/src/RegressionModule.R")
source(regression_path)
create_graph_path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/src/CreateGraph.R")
source(create_graph_path)


sliding_window_wavelet_shrinkage_prediction <- function(data, dt, threshold_name, threshold_mode, index, init_threshold_value, training_percentage, name, regression_model, units = 4, epochs = 20) {
    if(regression_model == "periodic") {
        periodic_based_prediction(
            data = data,
            dt = dt,
            threshold_name = threshold_name,
            threshold_mode = threshold_mode,
            index = index,
            init_threshold_value = init_threshold_value,
            training_percentage = training_percentage
        )
    } else if (regression_model == "arima") {
       arima_based_prediction(
            data = data,
            dt = dt,
            threshold_name = threshold_name,
            threshold_mode = threshold_mode,
            index = index,
            init_threshold_value = init_threshold_value,
            training_percentage = training_percentage,
            name = name
       )
    } else if (regression_model == "rnn") {
        rnn_based_prediction(
            data = data,
            dt = dt,
            threshold_name = threshold_name,
            threshold_mode = threshold_mode,
            index = index,
            init_threshold_value = init_threshold_value,
            training_percentage = training_percentage,
            name = name,
            units = units,
            epochs = epochs
        )
    } 
    else {
       print("Unsupported regression model")
    }
}
# 周期関数を用いた予測
periodic_based_prediction <- function(data, dt, threshold_name, threshold_mode, index, init_threshold_value, training_percentage) {
    term <- length(data)
    # wseを実行する必要がないが、メソッドを切り出すのがめんどくさいのでcoefficientsを取得するために実行
    wsed_data <- wse(data = data, dt = dt, threshold_name = threshold_name, threshold_mode = threshold_mode, index = index, init_threshold_value = init_threshold_value)
    prediction_term <- floor((1 - training_percentage) * term)
    cs <- wsed_data$cs
    ds <- wsed_data$ds
    coe_length <- length(cs)
    coe <- prepare_data(cs, ds, prediction_term)

    tic()
    sorted_best_coe <- run_parallel_regression(coe, coe_length, prediction_term, "periodic")
    time <- toc()

    y <- c(1:coe_length)
    c_4_1 <- periodic_function(y, sorted_best_coe[[1]]$a[[1]], sorted_best_coe[[1]]$b[[1]], sorted_best_coe[[1]]$c[[1]], sorted_best_coe[[1]]$d[[1]])
    d_1_1 <- periodic_function(y, sorted_best_coe[[2]]$a[[1]], sorted_best_coe[[2]]$b[[1]], sorted_best_coe[[2]]$c[[1]], sorted_best_coe[[2]]$d[[1]])
    d_1_2 <- periodic_function(y, sorted_best_coe[[3]]$a[[1]], sorted_best_coe[[3]]$b[[1]], sorted_best_coe[[3]]$c[[1]], sorted_best_coe[[3]]$d[[1]])
    d_1_3 <- periodic_function(y, sorted_best_coe[[4]]$a[[1]], sorted_best_coe[[4]]$b[[1]], sorted_best_coe[[4]]$c[[1]], sorted_best_coe[[4]]$d[[1]])
    d_1_4 <- periodic_function(y, sorted_best_coe[[5]]$a[[1]], sorted_best_coe[[5]]$b[[1]], sorted_best_coe[[5]]$c[[1]], sorted_best_coe[[5]]$d[[1]])
    d_2_1 <- periodic_function(y, sorted_best_coe[[6]]$a[[1]], sorted_best_coe[[6]]$b[[1]], sorted_best_coe[[6]]$c[[1]], sorted_best_coe[[6]]$d[[1]])
    d_2_2 <- periodic_function(y, sorted_best_coe[[7]]$a[[1]], sorted_best_coe[[7]]$b[[1]], sorted_best_coe[[7]]$c[[1]], sorted_best_coe[[7]]$d[[1]])
    d_3_1 <- periodic_function(y, sorted_best_coe[[8]]$a[[1]], sorted_best_coe[[8]]$b[[1]], sorted_best_coe[[8]]$c[[1]], sorted_best_coe[[8]]$d[[1]])

    for (k in seq(coe_length - prediction_term + 1, coe_length, by = 1)) {
        cs[[k]][[4]][1] <- c_4_1[k]

        ds[[k]][[2]][1] <- d_1_1[[k]]
        ds[[k]][[2]][2] <- d_1_2[[k]]
        ds[[k]][[2]][3] <- d_1_3[[k]]
        ds[[k]][[2]][4] <- d_1_4[[k]]
        ds[[k]][[3]][1] <- d_2_1[[k]]
        ds[[k]][[3]][2] <- d_2_2[[k]]
        ds[[k]][[4]][1] <- d_3_1[[k]]
    }

    denoised_ds <- threshold_for_groups(ds = ds, threshold_mode = threshold_mode, threshold_name = threshold_name, dt = dt, groups = 0, init_threshold_value = 1)
    i_groups <- inverse_haar_wavelet_transform_for_groups(cs, denoised_ds)
    i_groups <- lapply(i_groups, function(x) x * 8^0.5)
    all_data <- moving_average(i_groups, term)
    all_data <- apply_inverse_transform(all_data, dt, var)
    all_data <- pmax(all_data, 0)

    best_coe <- data.frame(a = numeric(), b = numeric(), c = numeric(), d = numeric())
    for (m in seq(1, 8, by = 1)) {
        tmp_best_coe <- data.frame(a = sorted_best_coe[[m]]$a[[1]], b = sorted_best_coe[[m]]$b[[1]], c = sorted_best_coe[[m]]$c[[1]], d = sorted_best_coe[[m]]$d[[1]])
        best_coe <- rbind(best_coe, tmp_best_coe)
    }
    prediction_data <- list(prediction_data = tail(all_data, prediction_term), regression_coefficient = best_coe)
    return(prediction_data)
}

# ARIMAを用いた予測
arima_based_prediction <- function(data, dt, threshold_name, threshold_mode, index, init_threshold_value, training_percentage, name) {
    term <- length(data)
    wsed_data <- wse(data = data, dt = dt, threshold_name = threshold_name, threshold_mode = threshold_mode, index = index, init_threshold_value = init_threshold_value, var = 1)
    prediction_term <- floor((1 - training_percentage) * term)
    cs <- wsed_data$cs
    ds <- wsed_data$ds
    coe_length <- length(cs)
    all_coefficients_data <- get_all_coefficients_data(cs, ds)
    coefficients_data_for_training <- prepare_data(cs, ds, prediction_term)

    tic()
    prediction_result <- run_parallel_arima_regression(coefficients_data_for_training, prediction_term)
    time <- toc()
    create_graph_for_swwsp(prediction_result, all_coefficients_data, coe_length, prediction_term, name)

    y <- c(1:coe_length)
    c_4_1 <- c(unlist(coefficients_data_for_training[[1]]), prediction_result[[1]]$mean)
    d_1_1 <- c(unlist(coefficients_data_for_training[[2]]), prediction_result[[2]]$mean)
    d_1_2 <- c(unlist(coefficients_data_for_training[[3]]), prediction_result[[3]]$mean)
    d_1_3 <- c(unlist(coefficients_data_for_training[[4]]), prediction_result[[4]]$mean)
    d_1_4 <- c(unlist(coefficients_data_for_training[[5]]), prediction_result[[5]]$mean)
    d_2_1 <- c(unlist(coefficients_data_for_training[[6]]), prediction_result[[6]]$mean)
    d_2_2 <- c(unlist(coefficients_data_for_training[[7]]), prediction_result[[7]]$mean)
    d_3_1 <- c(unlist(coefficients_data_for_training[[8]]), prediction_result[[8]]$mean)

    for (k in seq(coe_length - prediction_term + 1, coe_length, by = 1)) {
        cs[[k]][[4]][1] <- c_4_1[k]

        ds[[k]][[2]][1] <- d_1_1[[k]]
        ds[[k]][[2]][2] <- d_1_2[[k]]
        ds[[k]][[2]][3] <- d_1_3[[k]]
        ds[[k]][[2]][4] <- d_1_4[[k]]
        ds[[k]][[3]][1] <- d_2_1[[k]]
        ds[[k]][[3]][2] <- d_2_2[[k]]
        ds[[k]][[4]][1] <- d_3_1[[k]]
    }


    denoised_ds <- threshold_for_groups(ds = ds, threshold_mode = threshold_mode, threshold_name = threshold_name, dt = dt, groups = 0, init_threshold_value = 1)
    i_groups <- inverse_haar_wavelet_transform_for_groups(cs, denoised_ds)
    i_groups <- lapply(i_groups, function(x) x * 8^0.5)
    all_data <- moving_average(i_groups, term)
    all_data <- apply_inverse_transform(all_data, dt, var)
    all_data <- pmax(all_data, 0)

    prediction_data <- list(prediction_data = tail(all_data, prediction_term), execute_time = time)
    return(prediction_data)
}

rnn_based_prediction <- function(data, dt, threshold_name, threshold_mode, index, init_threshold_value, training_percentage, name, units = 4, epochs = 20) {
    term <- length(data)
    wsed_data <- wse(data = data, dt = dt, threshold_name = threshold_name, threshold_mode = threshold_mode, index = index, init_threshold_value = init_threshold_value)
    prediction_term <- floor((1 - training_percentage) * term)
    cs <- wsed_data$cs
    ds <- wsed_data$ds
    coe_length <- length(cs)
    all_coefficients_data <- get_all_coefficients_data(cs, ds)
    coefficients_data_for_training <- prepare_data(cs, ds, prediction_term)
    tic()
    prediction_result <- list()
    for (j in seq(1, 8, by = 1)) {
        training_data <- unlist(coefficients_data_for_training[[j]])
        prediction_result_each_resolution_level <- run_rnn_regression(training_data, prediction_term, units, epochs)
        prediction_result[[j]] <- data.frame(mean = prediction_result_each_resolution_level)
    }
    time <- toc()
    create_graph_for_swwsp(prediction_result, all_coefficients_data, coe_length, prediction_term, name)
    y <- c(1:coe_length)
    c_4_1 <- c(unlist(coefficients_data_for_training[[1]]), prediction_result[[1]]$mean)
    d_1_1 <- c(unlist(coefficients_data_for_training[[2]]), prediction_result[[2]]$mean)
    d_1_2 <- c(unlist(coefficients_data_for_training[[3]]), prediction_result[[3]]$mean)
    d_1_3 <- c(unlist(coefficients_data_for_training[[4]]), prediction_result[[4]]$mean)
    d_1_4 <- c(unlist(coefficients_data_for_training[[5]]), prediction_result[[5]]$mean)
    d_2_1 <- c(unlist(coefficients_data_for_training[[6]]), prediction_result[[6]]$mean)
    d_2_2 <- c(unlist(coefficients_data_for_training[[7]]), prediction_result[[7]]$mean)
    d_3_1 <- c(unlist(coefficients_data_for_training[[8]]), prediction_result[[8]]$mean)
    for (k in seq(coe_length - prediction_term + 1, coe_length, by = 1)) {
        cs[[k]][[4]][1] <- c_4_1[k]

        ds[[k]][[2]][1] <- d_1_1[[k]]
        ds[[k]][[2]][2] <- d_1_2[[k]]
        ds[[k]][[2]][3] <- d_1_3[[k]]
        ds[[k]][[2]][4] <- d_1_4[[k]]
        ds[[k]][[3]][1] <- d_2_1[[k]]
        ds[[k]][[3]][2] <- d_2_2[[k]]
        ds[[k]][[4]][1] <- d_3_1[[k]]
    }
    if (threshold_mode == "soft" || threshold_mode == "hard") {
        denoised_ds <- threshold_for_groups(ds = ds, threshold_mode = threshold_mode, threshold_name = threshold_name, dt = dt, groups = 0, init_threshold_value = 1)
        i_groups <- inverse_haar_wavelet_transform_for_groups(cs, denoised_ds)
        i_groups <- lapply(i_groups, function(x) x * 8^0.5)
        all_data <- moving_average(i_groups, term)
        all_data <- apply_inverse_transform(all_data, dt, var = 1)
        all_data <- pmax(all_data, 0)
        prediction_data <- list(prediction_data = tail(all_data, prediction_term), execute_time = time)
    } else {
        # doubleを指定したとき
        denoised_ds_soft <- threshold_for_groups(ds = ds, threshold_mode = "soft", threshold_name = threshold_name, dt = dt, groups = 0, init_threshold_value = 1)
        denoised_ds_hard <- threshold_for_groups(ds = ds, threshold_mode = "hard", threshold_name = threshold_name, dt = dt, groups = 0, init_threshold_value = 1)
        i_groups_soft <- inverse_haar_wavelet_transform_for_groups(cs, denoised_ds_soft)
        i_groups_soft <- lapply(i_groups_soft, function(x) x * 8^0.5)
        all_data_soft <- moving_average(i_groups_soft, term)
        all_data_soft <- apply_inverse_transform(all_data_soft, dt, var = 1)
        all_data_soft <- pmax(all_data_soft, 0)
        i_groups_hard <- inverse_haar_wavelet_transform_for_groups(cs, denoised_ds_hard)
        i_groups_hard <- lapply(i_groups_hard, function(x) x * 8^0.5)
        all_data_hard <- moving_average(i_groups_hard, term)
        all_data_hard <- apply_inverse_transform(all_data_hard, dt, var = 1)
        all_data_hard <- pmax(all_data_hard, 0)
        prediction_data <- list(soft = list( prediction_data = tail(all_data_soft, prediction_term), execute_time = time), hard = list( prediction_data = tail(all_data_hard, prediction_term), execute_time = time))
    }
    return(prediction_data)
}

# 二次関数を用いた予測
quatratic_based_prediction <- function(data, dt, threshold_name, threshold_mode, index, init_threshold_value, training_percentage) {
    term <- length(data)
    data <- wse(data = data, dt = dt, threshold_name = threshold_name, threshold_mode = threshold_mode, index = index, init_threshold_value = init_threshold_value)
    prediction_term <- floor((1 - training_percentage) * term)
    cs <- data$cs
    ds <- data$ds
    d_ds <- data$denoised_ds
    coe_length <- length(cs)
    coe <- prepare_data(data$cs, data$ds, prediction_term)

    tic()
    sorted_best_coe <- run_parallel_regression(coe, coe_length, prediction_term, "quadratic")
    time <- toc()

    y <- c(1:(term%/%8 + 1))
    c_4_1 <- quadratic_function(y, sorted_best_coe[[1]]$a[[1]], sorted_best_coe[[1]]$b[[1]], sorted_best_coe[[1]]$c[[1]])
    d_1_1 <- quadratic_function(y, sorted_best_coe[[2]]$a[[1]], sorted_best_coe[[2]]$b[[1]], sorted_best_coe[[2]]$c[[1]])
    d_1_2 <- quadratic_function(y, sorted_best_coe[[3]]$a[[1]], sorted_best_coe[[3]]$b[[1]], sorted_best_coe[[3]]$c[[1]])
    d_1_3 <- quadratic_function(y, sorted_best_coe[[4]]$a[[1]], sorted_best_coe[[4]]$b[[1]], sorted_best_coe[[4]]$c[[1]])
    d_1_4 <- quadratic_function(y, sorted_best_coe[[5]]$a[[1]], sorted_best_coe[[5]]$b[[1]], sorted_best_coe[[5]]$c[[1]])
    d_2_1 <- quadratic_function(y, sorted_best_coe[[6]]$a[[1]], sorted_best_coe[[6]]$b[[1]], sorted_best_coe[[6]]$c[[1]])
    d_2_2 <- quadratic_function(y, sorted_best_coe[[7]]$a[[1]], sorted_best_coe[[7]]$b[[1]], sorted_best_coe[[7]]$c[[1]])
    d_3_1 <- quadratic_function(y, sorted_best_coe[[8]]$a[[1]], sorted_best_coe[[8]]$b[[1]], sorted_best_coe[[8]]$c[[1]])

    for (k in seq(1, term%/%8 + 1, by = 1)) {
        cs[[k]][[4]][1] <- c_4_1[k]

        d_ds[[k]][[2]][1] <- d_1_1[[k]]
        d_ds[[k]][[2]][2] <- d_1_2[[k]]
        d_ds[[k]][[2]][3] <- d_1_3[[k]]
        d_ds[[k]][[2]][4] <- d_1_4[[k]]
        d_ds[[k]][[3]][1] <- d_2_1[[k]]
        d_ds[[k]][[3]][2] <- d_2_2[[k]]
        d_ds[[k]][[4]][1] <- d_3_1[[k]]
    }
    i_groups <- inverse_haar_wavelet_transform_for_groups(cs, d_ds)
    i_groups <- lapply(i_groups, function(x) x * 8^0.5)
    all_data <- moving_average(i_groups, term)
    all_data <- apply_inverse_transform(all_data, dt, var = 1)
    all_data <- pmax(all_data, 0)

    best_coe <- data.frame(a = numeric(), b = numeric(), c = numeric())
    for (m in seq(1, 8, by = 1)) {
        tmp_best_coe <- data.frame(a = sorted_best_coe[[m]]$a[[1]], b = sorted_best_coe[[m]]$b[[1]], c = sorted_best_coe[[m]]$c[[1]])
        best_coe <- rbind(best_coe, tmp_best_coe)
    }
    prediction_data <- list(prediction_data = tail(all_data, prediction_term), regression_coefficient = best_coe, execute_time = time)
    return(prediction_data)
}

multi_resolution_wavelet_prediction <- function(data, training_percentage, resolution_level, name, regression_model, units = 4, epochs = 20) {
    if(resolution_level > get_max_resolution_level(data)){
        print("Error: resolution_level is too large")
        return(NULL)
    }
    training_data <- data[1:ceiling(length(data) * training_percentage)] / sqrt(2 ** resolution_level)
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

    # 各解像度レベルでの係数をRDataファイルに保存
    rdata_filename <- paste0(name, "resolution_level_", resolution_level, ".RData")
    save(trading_coefficients, all_coefficients_data, file = rdata_filename, envir = environment())

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

        # sorted_best_coe_listをRDataファイルに保存
        if (!dir.exists(name)) {
            dir.create(name, recursive = TRUE)
        }
        rdata_filename <- paste0(name, "sorted_best_coe_list.RData")
        save(sorted_best_coe_list, file = rdata_filename, envir = environment())
    } else if (regression_model == "lstm") {
        prediction_result <- list()
        for (j in seq(1, resolution_level + 1)) {
            training_data <- unlist(trading_coefficients[[j]])
            if (length(training_data) == 1) {
                prediction_result[[j]] <- data.frame(mean = rep(training_data, coefficients_prediction_term_list[[j]]))
            } else if (length(training_data) == 2) {
                prediction_result[[j]] <- data.frame(mean = rep(mean(training_data), coefficients_prediction_term_list[[j]]))
            } else {
                # LSTMモデルの適用
                prediction_result_each_resolution_level <- run_lstm_regression(training_data, coefficients_prediction_term_list[[j]], units, epochs)
                prediction_result[[j]] <- data.frame(mean = prediction_result_each_resolution_level)
            }
        }
    } else if (regression_model == "rnn") {
        prediction_result <- list()
        for (j in seq(1, resolution_level + 1)) {
            training_data <- unlist(trading_coefficients[[j]])
            if (length(training_data) == 1) {
                prediction_result[[j]] <- data.frame(mean = rep(training_data, coefficients_prediction_term_list[[j]]))
            } else if (length(training_data) == 2) {
                prediction_result[[j]] <- data.frame(mean = rep(mean(training_data), coefficients_prediction_term_list[[j]]))
            } else {
                # RNNモデルの適用
                prediction_result_each_resolution_level <- run_rnn_regression(training_data, coefficients_prediction_term_list[[j]], units, epochs)
                prediction_result[[j]] <- data.frame(mean = prediction_result_each_resolution_level)
            }
        }
    } else if (regression_model == "prophet") {
        prediction_result <- list()
        for (j in seq(1, resolution_level + 1)) {
            training_data <- unlist(trading_coefficients[[j]])
            if(length(training_data) == 1) {
                prediction_result[[j]] <- data.frame(mean = rep(training_data, coefficients_prediction_term_list[[j]]))
            } else {
                prediction_result_each_resolution_level <- run_prophet_regression(training_data, coefficients_prediction_term_list[[j]])
                prediction_result[[j]] <- data.frame(mean = prediction_result_each_resolution_level)
            }
        }
    }

    time <- toc()

    create_graph_for_mwwp(prediction_result, all_coefficients_data, length(data), prediction_term, name, resolution_level)

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
                a <- (inverse_data[ceiling(j/2)] + prediction_scaling[[i]][j]) / sqrt(2)
            } else {
                a <- append(a, (inverse_data[ceiling(j/2)] + prediction_scaling[[i]][j]) / sqrt(2))
            }
        }
        inverse_data <- a
    }
    prediction_result <- list(prediction_data = inverse_data[1:prediction_term] * sqrt(2 ** resolution_level), execute_time = time)
    return(prediction_result)
}

get_max_resolution_level <- function(data) {
    return(floor(log2(length(data))))
}

# private

get_all_coefficients_data <- function(cs, ds) {
    tmp_cs_4_1 <- list()
    tmp_ds <- vector("list", 7)  # D[1][1] ~ D[3][1]

    for (j in seq(1, length(ds), by = 1)) {
        tmp_cs_4_1 <- c(tmp_cs_4_1, cs[[j]][[4]][1])
        for (i in 1:7) {
            tmp_ds[[i]] <- c(tmp_ds[[i]], ds[[j]][[ceiling(i/4)]][(i - 1)%%4 + 1])
        }
    }

    coe <- c(list(tmp_cs_4_1), tmp_ds)
    return(coe)
}

prepare_data <- function(cs, ds, prediction_term) {
    tmp_cs_4_1 <- list()
    tmp_ds <- vector("list", 7)  # D[1][1] ~ D[3][1]

    for (j in seq(1, length(ds) - prediction_term, by = 1)) {
        tmp_cs_4_1 <- c(tmp_cs_4_1, cs[[j]][[4]][1])
        for (i in 1:7) {
            tmp_ds[[i]] <- c(tmp_ds[[i]], ds[[j]][[ceiling(i/4)]][(i - 1)%%4 + 1])
        }
    }

    coe <- c(list(tmp_cs_4_1), tmp_ds)
    return(coe)
}

apply_inverse_transform <- function(all_data, dt, var) {
    if (dt == "A1") {
        return(inverse_anscombe_transform_from_group(all_data, var))
    } else if (dt == "A2") {
        return(inverse_anscombe_transform2_from_group(all_data, var))
    } else if (dt == "A3") {
        return(inverse_anscombe_transform3_from_group(all_data, var))
    } else if (dt == "B1") {
        return(inverse_bartlett_transform_from_group(all_data, var))
    } else if (dt == "B2") {
        return(inverse_bartlett_transform2_from_group(all_data, var))
    } else if (dt == "Fr") {
        return(inverse_freeman_transform_from_group(all_data, var))
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
        if (length(wavelet_coefficients[[i]]) %% 2 != 0){
        wavelet_coefficients[[i]][length(wavelet_coefficients[[i]]) + 1] = wavelet_coefficients[[i]][length(wavelet_coefficients[[i]])]
    }
    for (j in seq(1, length(wavelet_coefficients[[i]]) - 1, by = 2)) {
        if(j == 1){
            wavelet_coefficients[[i + 1]] <- (wavelet_coefficients[[i]][j] + wavelet_coefficients[[i]][j + 1]) / sqrt(2)
            scaling_coefficients[[i+1]] <- (wavelet_coefficients[[i]][j] - wavelet_coefficients[[i]][j + 1]) / sqrt(2)
            scaling_coefficients[[i+1]] <- c(scaling_coefficients[[i+1]], -(wavelet_coefficients[[i]][j] - wavelet_coefficients[[i]][j + 1]) / sqrt(2))
        }
        else{
            wavelet_coefficients[[i + 1]] <- c(wavelet_coefficients[[i + 1]], (wavelet_coefficients[[i]][j] + wavelet_coefficients[[i]][j + 1]) / sqrt(2))
            scaling_coefficients[[i+1]] <- c(scaling_coefficients[[i+1]], (wavelet_coefficients[[i]][j] - wavelet_coefficients[[i]][j + 1]) / sqrt(2))
            scaling_coefficients[[i+1]] <- c(scaling_coefficients[[i+1]], -(wavelet_coefficients[[i]][j] - wavelet_coefficients[[i]][j + 1]) / sqrt(2))
        }
    }
    }

    return(list(wavelet_coefficients = wavelet_coefficients, scaling_coefficients = scaling_coefficients))
}
