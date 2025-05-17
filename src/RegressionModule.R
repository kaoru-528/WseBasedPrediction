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
    tmp_coe <- unlist(coe)
    x <- seq(1, length(tmp_coe))
    model_coe_list <- data.frame(mse = numeric(), a = numeric(), b = numeric(), c = numeric(), d = numeric())

    for (sub_a in seq(0.5, 1, by = 0.5)) {
        for (sub_b in seq(0.5, 1, by = 0.5)) {
            for (sub_c in seq(0.5, 1, by = 0.5)) {
                for (sub_d in seq(0, 1, by = 0.5)) {
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
    tmp_coe <- unlist(coe)
    x <- seq(1, length(tmp_coe))
    a_data <- data.frame(mse = numeric(), a = numeric(), b = numeric(), c = numeric())
    if (all(sapply(coe[[i]], function(x) x == 0)) == TRUE) {
        a_data <- data.frame(mse = 0, a = 0, b = 0, c = 0)
    } else {
        for (sub_a in seq(0, 1, by = 0.5)) {
            for (sub_b in seq(0, 1, by = 0.5)) {
                for (sub_c in seq(0, 1, by = 0.5)) {
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
FALSE
