# 周期関数
periodic_function <- function(x, a, b, c, d) {
    (a * sin((b * x) + c)) + d
}

# 二次関数
quadratic_function <- function(x, a, b, c) {
    a * x^2 + b * x + c
}

# 回帰実行関数
run_regression_for_periodic_function <- function(j, coe) {
    MAX_SEARCH_RANGE <- 10
    tmp_coe <- unlist(coe)
    x <- seq(1, length(tmp_coe))
    model_coe_list <- data.frame(mse = numeric(), a = numeric(), b = numeric(), c = numeric(), d = numeric())

    for (sub_a in seq(0.5, MAX_SEARCH_RANGE, by = 0.5)) {
        for (sub_b in seq(0.5, MAX_SEARCH_RANGE, by = 0.5)) {
            for (sub_c in seq(0.5, MAX_SEARCH_RANGE, by = 0.5)) {
                for (sub_d in seq(0, MAX_SEARCH_RANGE, by = 0.5)) {
                  fit <- tryCatch({
                    nls(tmp_coe ~ periodic_function(x, a, b, c, d), start = list(a = sub_a, b = sub_b, c = sub_c, d = sub_d), control = nls.control(warnOnly = TRUE))
                  }, error = function(e) NULL)

                  if (!is.null(fit)) {
                    params <- coef(fit)
                    pre <- periodic_function(x, params[1], params[2], params[3], params[4])
                    mse <- mean((tmp_coe - pre)^2)
                    add_data <- data.frame(mse = mse, a = params[1], b = params[2], c = params[3], d = params[4])
                    model_coe_list <- rbind(model_coe_list, add_data)
                  }
                }
            }
        }
    }

    row.names(model_coe_list) <- NULL
    model_coe_list <- model_coe_list[order(model_coe_list$mse, decreasing = FALSE), ]
    if (nrow(model_coe_list) == 0) {
        model_coe_list <- data.frame(mse = 0, a = 0, b = 0, c = 0, d = 0)
    }
    return(model_coe_list)
}

# cal coe in regression function
run_regression_for_quadratic_function <- function(i, coe) {
    MAX_SEARCH_RANGE <- 1
    tmp_coe <- unlist(coe)
    x <- seq(1, length(tmp_coe))
    a_data <- data.frame(mse = numeric(), a = numeric(), b = numeric(), c = numeric())
    if (all(sapply(coe[[i]], function(x) x == 0)) == TRUE) {
        a_data <- data.frame(mse = 0, a = 0, b = 0, c = 0)
    } else {
        for (sub_a in seq(0, MAX_SEARCH_RANGE, by = 0.5)) {
            for (sub_b in seq(0, MAX_SEARCH_RANGE, by = 0.5)) {
                for (sub_c in seq(0, MAX_SEARCH_RANGE, by = 0.5)) {
                  fit <- nls(tmp_coe ~ quadratic_function(x, a, b, c), start = list(a = sub_a, b = sub_b, c = sub_c), control = nls.control(warnOnly = TRUE))
                  params <- coef(fit)
                  pre <- quadratic_function(x, params[1], params[2], params[3])
                  mse <- mean((unlist(coe[[i]]) - pre)^2)
                  add_data <- data.frame(mse = mse, a = params[1], b = params[2], c = params[3])
                  a_data <- rbind(a_data, add_data)
                }
            }
        }
        row.names(a_data) <- NULL
        a_data <- a_data[order(a_data$mse, decreasing = F), ]
    }
    return(a_data)
}

run_parallel_regression <- function(coe, coe_length, prediction_term, regression_function) {
    # 並列処理のセットアップ
    num_cores <- detectCores()
    cl <- makeCluster(num_cores)
    registerDoParallel(cl)

    # 並列処理で回帰分析を実行
    if (regression_function == "periodic") {
        sorted_best_coe <- foreach(j = seq(1, 8), .packages = c("stats"), .export = c("run_regression_for_periodic_function", "periodic_function")) %dopar% {
            run_regression_for_periodic_function(j, coe[[j]])
        }
    } else if (regression_function == "quadratic") {
        sorted_best_coe <- foreach(i = seq(1, 8), .packages = c("stats"), .export = c("run_regression_for_quadratic_function", "quadratic_function")) %dopar% {
            run_regression_for_quadratic_function(i, coe[[i]])
        }
    }

    # 並列処理の停止
    stopCluster(cl)

    return(sorted_best_coe)
}

run_arima_regression <- function(j, coe, prediction_term) {
    # 対象データの取得
    train_data <- unlist(coe[[j]])

    # ARIMAモデルの適用
    fit <- auto.arima(train_data, stepwise = FALSE, approximation = FALSE, seasonal = FALSE, trace = TRUE)
    forecasted <- forecast(fit, h = prediction_term)  # 予測

    return(forecasted)
}

run_parallel_arima_regression <- function(coe, prediction_term) {
    # 並列処理のセットアップ
    num_cores <- detectCores()
    cl <- makeCluster(num_cores)
    registerDoParallel(cl)

    # 並列処理でARIMA回帰分析を実行
    prediction_result <- foreach(j = seq(1, 8), .packages = c("forecast"), .export = c("run_arima_regression")) %dopar% {
        run_arima_regression(j, coe, prediction_term)
    }

    # 並列処理の停止
    stopCluster(cl)

    return(prediction_result)
}

run_lstm_regression <- function(training_data, prediction_term) {
    min_val <- min(training_data)
    max_val <- max(training_data)

    training_data_norm <- (training_data - min_val) / (max_val - min_val)

    # --- 入力ベクトルの大きさ: 3 に変換 ---
    if (length(training_data_norm) < 3) {
        input_size <- 1
    } else if (length(training_data_norm) < 5) {
        input_size <- 2
    } else {
       input_size <- 3
    }
    X <- NULL
    Y <- NULL
    for (i in 1:(length(training_data_norm) - input_size)) {
        X <- rbind(X, training_data_norm[i:(i + input_size - 1)])
        Y <- c(Y, training_data_norm[i + input_size])
    }

    # 全データで再学習し予測
    model <- keras_model_sequential() %>%
        layer_lstm(units = 1000, input_shape = c(input_size, 1), return_sequences = FALSE, activation = "relu") %>%
        layer_dense(units = 1, activation = "relu")
    model %>% compile(
        loss = 'mean_squared_error',
        optimizer = optimizer_adam(lr = 0.001)
    )
    train_X_arr <- array_reshape(X, c(nrow(X), input_size, 1))
    model %>% fit(train_X_arr, Y, epochs = 500, batch_size = 32, verbose = 0)

    # 予測用データ作成
    last_seq <- training_data_norm[(length(training_data_norm) - input_size + 1):length(training_data_norm)]
    preds_norm <- c()
    for (h in 1:prediction_term) {
        input_pred <- array_reshape(last_seq, c(1, input_size, 1))
        pred <- as.numeric(model %>% predict(input_pred))
        preds_norm <- c(preds_norm, pred)
        last_seq <- c(last_seq[-1], pred)
    }

    # 逆正規化
    forecasted <- preds_norm * (max_val - min_val) + min_val
    prediction_result <- forecasted
    return(prediction_result)
}

run_rnn_regression <- function(training_data, prediction_term) {
    min_val <- min(training_data)
    max_val <- max(training_data)

    training_data_norm <- (training_data - min_val) / (max_val - min_val)

    # --- 入力ベクトルの大きさ: 3 に変換 ---
    if (length(training_data_norm) < 3) {
        input_size <- 1
    } else if (length(training_data_norm) < 5) {
        input_size <- 2
    } else {
       input_size <- 3
    }
    X <- NULL
    Y <- NULL
    for (i in 1:(length(training_data_norm) - input_size)) {
        X <- rbind(X, training_data_norm[i:(i + input_size - 1)])
        Y <- c(Y, training_data_norm[i + input_size])
    }

    # RNNモデルの構築
    model <- keras_model_sequential() %>%
        layer_simple_rnn(units = 4, input_shape = c(input_size, 1), return_sequences = FALSE, activation = "relu") %>%
        layer_dropout(rate = 0.1) %>%
        layer_dense(units = 1, activation = "relu")

    model %>% compile(
        loss = 'mean_squared_error',
        optimizer = optimizer_adam(lr = 0.001)
    )

    train_X_arr <- array_reshape(X, c(nrow(X), input_size, 1))
    model %>% fit(train_X_arr, Y, epochs = 20, batch_size = 1, verbose = 0)
    # 予測用データ作成
    last_seq <- training_data_norm[(length(training_data_norm) - input_size + 1):length(training_data_norm)]
    preds_norm <- c()
    for (h in 1:prediction_term) {
        input_pred <- array_reshape(last_seq, c(1, input_size, 1))
        pred <- as.numeric(model %>% predict(input_pred))
        preds_norm <- c(preds_norm, pred)
        last_seq <- c(last_seq[-1], pred)
    }

    # 逆正規化
    forecasted <- preds_norm * (max_val - min_val) + min_val
    prediction_result <- forecasted
    return(prediction_result)
}

run_prophet_regression <- function(training_data, prediction_term) {
   df <- data.frame(ds = seq.Date(from = as.Date("2000-01-01"), by = "day", length.out = length(training_data)),
                     y = training_data)
    m <- prophet(df, yearly.seasonality = FALSE, weekly.seasonality = FALSE, daily.seasonality = FALSE)
    future <- make_future_dataframe(m, periods = prediction_term, freq = "day")
    forecast <- predict(m, future)
    # 予測値のみ抽出
    pred <- tail(forecast$yhat, prediction_term)
    return(pred)
}
