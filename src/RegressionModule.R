# RegressionModule.R

# 回帰関数の定義
f <- function(x, a, b, c, d) {
    (a * sin((b * x) + c)) + d
}

# 回帰実行関数
run_regression <- function(j, coe, coe_length, prediction_term) {
    x <- seq(1, (coe_length - prediction_term))
    a_data <- data.frame(mse = numeric(), a = numeric(), b = numeric(), c = numeric(), d = numeric())
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
                        a_data <- rbind(a_data, add_data)
                    }
                }
            }
        }
    }

    row.names(a_data) <- NULL
    a_data <- a_data[order(a_data$mse, decreasing = FALSE), ]
    return(a_data)
}