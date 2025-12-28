threshold_for_groups <- function(ds, threshold_mode, threshold_name, dt, groups, init_threshold_value) {
    group_length <- length(ds)
    lists <- list()
    u <- 1
    while (u <= group_length) {
        list_s <- threshold_for_group(ds[[u]], threshold_mode, threshold_name, dt, groups, init_threshold_value, u)
        lists <- append(lists, list(list_s))
        u <- u + 1
    }
    return(lists)
}

# Apply the soft or hard thresholding method of threshold_name to a set of wavelet coefficients
threshold_for_group <- function(group_wavelet_coefficients, threshold_mode, threshold_name, dt, groups, init_threshold_value, j) {
    if (threshold_name == "ut" || threshold_name == "ldt" || threshold_name == "lut" || threshold_name == "none") {
        data_length <- length(group_wavelet_coefficients[[1]])
        t <- 1000
        j <- 1
        if (threshold_name == "ut") {
            t <- get_universal_threshold(data_length)
        } else if (threshold_name == "none") {
            t <- init_threshold_value
        }
        lists <- list()
        lists <- append(lists, list(group_wavelet_coefficients[[1]]))
        i <- 2
        group_length <- length(group_wavelet_coefficients)

        if (threshold_name == "ldt" || threshold_name == "lut") {
            c <- get_scaling_coefficients_from_group(group_wavelet_coefficients[[1]])
            lam0 <- mean(group_wavelet_coefficients[[1]]) * (data_length^0.5)
        }

        while (i <= group_length) {
            if (threshold_name == "ldt") {
                temp_list <- ldt_threshold(group_wavelet_coefficients[[i]], threshold_mode, i, data_length, lam0)
            } else {
                if (threshold_name == "lut") {
                  ut_data_length <- length(c[[i]])
                  t <- get_universal_threshold(ut_data_length)
                }
                temp_list <- threshold_for_one_level(group_wavelet_coefficients[[i]], threshold_mode, t)
            }
            lists <- append(lists, list(temp_list))
            i <- i + 1
        }
    } else {
        sub_group_length <- length(groups[[1]])
        if (j != length(groups)) {
            next_value <- next_value <- groups[[j + 1]][sub_group_length]
        } else {
            next_value <- next_value <- groups[[1]][sub_group_length]
        }
        t <- lht_threshold(groups[[j]], dt, threshold_name, threshold_mode, next_value)
        j <- j + 1
        lists <- list()
        lists <- append(lists, list(group_wavelet_coefficients[[1]]))
        i <- 2
        group_length <- length(group_wavelet_coefficients)
        while (i <= group_length) {
            temp_list <- threshold_for_one_level(group_wavelet_coefficients[[i]], threshold_mode, t)
            lists <- append(lists, list(temp_list))
            i <- i + 1
        }
    }
    return(lists)
}

# --------------------------------- Get ut, lut threshold ut:Universal Threshold lut:Level-universal-Threshold lut is applied to the length of j-th level empirical wavelet coefficients.  ---------------------------------
get_universal_threshold <- function(group_length) {
    a <- log(group_length)
    b <- 2 * a
    c <- b^0.5
    return(c)
}

# --------------------------------- Get ldt threshold ldt:Level-dependent-Threshold ---------------------------------
get_level_dependent_threshold <- function(j_level, now_level, mean) {
    a <- 2^(-1 * 0.5 * (now_level + 1))
    log2j <- log(2^(j_level - now_level + 1))
    b <- 2 * log2j
    c <- 4 * (log2j)^2
    d <- 8 * mean * log2j
    t <- a * (b + ((c + d)^0.5))
    return(t)
}

# Thresholding the wavelet coefficients of a layer at a threshold value of t
threshold_for_one_level <- function(wavelet_coefficients, threshold_mode, t) {
    coefficients_length <- length(wavelet_coefficients)
    temp_list <- c()
    i <- 1
    while (i <= coefficients_length) {
        a <- threshold(wavelet_coefficients[i], t, threshold_mode)
        temp_list <- append(temp_list, a)
        i <- i + 1
    }
    return(temp_list)
}

# Calculating ldt and thresholding the data
ldt_threshold <- function(data, threshold_mode, loop_level, data_length, lam0) {
    # Highest Resolution
    j_level <- get_highest_resolution_level(data_length)
    # Thresholding the data one by one
    i <- 1
    temp_list <- c()
    mean <- lam0/length(data)
    while (i <= length(data)) {
        # Get ldt threshold
        t <- get_level_dependent_threshold(j_level, loop_level, mean)
        # Threshold processing
        denoise_data <- threshold(data[[i]], t, threshold_mode)
        temp_list <- append(temp_list, denoise_data)
        i <- i + 1
    }
    return(temp_list)
}

lht_threshold <- function(original_groups, transform_method, threshold_rule, threshold_mode, next_value) {
    # 偶数番目と奇数番目に分ける
    subgroup_len <- length(original_groups)
    minimum <- optim(par = 0, fn = loss_function, original_group = original_groups, dt = transform_method, threshold_name = threshold_rule, threshold_mode = threshold_mode, next_value = next_value, method = "Brent", lower = -5, upper = 5)$par
    # minimumが負の値になる場合は0にする
    if (minimum < 0) {
        minimum <- 0
    }
    threshold_value <- ((1 - log(2)/log(subgroup_len))^(-0.5)) * minimum
    return(threshold_value)
}

loss_function <- function(t, original_group, dt, threshold_name, threshold_mode, next_value) {
    # Separate even-numbered and odd-numbered
    odd_group <- original_group[seq(1, length(original_group), by = 2)]
    even_group <- original_group[seq(2, length(original_group), by = 2)]

    odd_index <- log(length(odd_group), base = 2)
    even_index <- log(length(even_group), base = 2)

    # Perform WSE for even and odd numbers
    thresholded_odd_group <- wse(odd_group, dt, "none", threshold_mode, 1, odd_index, t)
    thresholded_even_group <- wse(even_group, dt, "none", threshold_mode, 1, even_index, t)

    original_group <- append(original_group, next_value)

    odd_ave_list <- list()
    even_ave_list <- list()


    for (i in 1:length(thresholded_odd_group$idata)) {
        if (i != length(thresholded_odd_group$idata)) {
            odd_ave <- (thresholded_odd_group$idata[i] + thresholded_odd_group$idata[i + 1]) * 0.5
        } else {
            odd_ave <- (thresholded_odd_group$idata[i] + thresholded_odd_group$idata[1]) * 0.5
        }
        odd_ave_list <- c(odd_ave_list, odd_ave)
    }

    for (i in 1:length(thresholded_even_group$idata)) {
        if (i != length(thresholded_even_group$idata)) {
            even_ave <- (thresholded_even_group$idata[i] + thresholded_even_group$idata[i + 1]) * 0.5
        } else {
            even_ave <- (thresholded_even_group$idata[i] + thresholded_even_group$idata[1]) * 0.5
        }
        even_ave_list <- c(even_ave_list, even_ave)
    }
    squared_error <- 0
    for (i in 1:length(thresholded_odd_group$idata)) {
        odd_squared_error <- (odd_ave_list[[i]][1] - original_group[2 * i])^2
        even_squared_error <- (even_ave_list[[i]][1] - original_group[2 * i + 1])^2
        squared_error <- squared_error + odd_squared_error + even_squared_error
    }
    return(squared_error)
}

# Thresholding of the value coe according to the threshold r
threshold <- function(coe, r, threshold_mode) {
    if (threshold_mode == "h") {
        if (abs(coe) <= r) {
            return(0)
        } else {
            return(coe)
        }
    } else {
        if (abs(coe) <= r) {
            return(0)
        } else {
            if (coe > 0) {
                return(coe - r)
            } else {
                return(coe + r)
            }
        }
    }
}
