# 二次関数を回帰モデルにする
f <- function(x, a, b, c, d) {
    (a * sin((b * x) + c)) + d
}

# 回帰実行関数
run_regression_for_periodic_function <- function(j, coe, coe_length, prediction_term) {
    x <- seq(1, (coe_length - prediction_term))
    model_coe_list <- data.frame(mse = numeric(), a = numeric(), b = numeric(), c = numeric(), d = numeric())
    tmp_coe <- unlist(coe[[j]])

    for (sub_a in seq(0.5, 1, by = 0.5)) {
        for (sub_b in seq(0.5, 1, by = 0.5)) {
            for (sub_c in seq(0.5, 1, by = 0.5)) {
                for (sub_d in seq(0, 1, by = 0.5)) {
                    fit <- tryCatch({
                        nls(tmp_coe ~ f(x, a, b, c, d), start = list(a = sub_a, b = sub_b, c = sub_c, d = sub_d), control = nls.control(warnOnly = TRUE))
                    }, error = function(e) NULL)

                    if (!is.null(fit)) {
                        params <- coef(fit)
                        pre <- f(x, params[1], params[2], params[3], params[4])
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
    return(model_coe_list)
}

run_parallel_regression <- function(coe, coe_length, prediction_term) {
    # 並列処理のセットアップ
    num_cores <- detectCores()
    cl <- makeCluster(num_cores)
    registerDoParallel(cl)

    # 並列処理で回帰分析を実行
    sorted_best_coe <- foreach(j = seq(1, 8), .packages = c("stats"), .export = c("run_regression_for_periodic_function", "f")) %dopar% {
        run_regression_for_periodic_function(j, coe, coe_length, prediction_term)
    }

    # 並列処理の停止
    stopCluster(cl)

    return(sorted_best_coe)
}