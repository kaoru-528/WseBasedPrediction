ThresholdForGroups <- function(Ds, thresholdMode, thresholdName, dt, groups, initThresholdvalue) {
    groupLength <- length(Ds)
    lists <- list()
    u <- 1
    while (u <= groupLength) {
        list_s <- ThresholdForGroup(Ds[[u]], thresholdMode, thresholdName, dt, groups, initThresholdvalue, u)
        lists <- append(lists, list(list_s))
        u <- u + 1
    }
    return(lists)
}

# Apply the soft or hard thresholding method of thresholdName to a set of wavelet coefficients
ThresholdForGroup <- function(GroupWaveletCoefficients, thresholdMode, thresholdName, dt, groups, initThresholdvalue, j) {
    if (thresholdName == "ut" || thresholdName == "ldt" || thresholdName == "lut" || thresholdName == "none") {
        dataLength <- length(GroupWaveletCoefficients[[1]])
        t <- 1000
        j <- 1
        if (thresholdName == "ut") {
            t <- getUniversalThreshold(dataLength)
        } else if (thresholdName == "none") {
            t <- initThresholdvalue
        }
        lists <- list()
        lists <- append(lists, list(GroupWaveletCoefficients[[1]]))
        i <- 2
        groupLength <- length(GroupWaveletCoefficients)

        if (thresholdName == "ldt" || thresholdName == "lut") {
            C <- getScalingCoefficientsFromGroup(GroupWaveletCoefficients[[1]])
            lam0 <- mean(GroupWaveletCoefficients[[1]]) * (dataLength^0.5)
        }

        while (i <= groupLength) {
            if (thresholdName == "ldt") {
                tempList <- ldtThreshold(GroupWaveletCoefficients[[i]], thresholdMode, i, dataLength, lam0)
            } else {
                if (thresholdName == "lut") {
                  ut_dataLength <- length(C[[i]])
                  t <- getUniversalThreshold(ut_dataLength)
                }
                tempList <- ThresholdForOneLevel(GroupWaveletCoefficients[[i]], thresholdMode, t)
            }
            lists <- append(lists, list(tempList))
            i <- i + 1
        }
    } else {
        sub_groupLength <- length(groups[[1]])
        if (j != length(groups)) {
            next_value <- next_value <- groups[[j + 1]][sub_groupLength]
        } else {
            next_value <- next_value <- groups[[1]][sub_groupLength]
        }
        t <- lhtThreshold(groups[[j]], dt, thresholdName, thresholdMode, next_value)
        j <- j + 1
        lists <- list()
        lists <- append(lists, list(GroupWaveletCoefficients[[1]]))
        i <- 2
        groupLength <- length(GroupWaveletCoefficients)
        while (i <= groupLength) {
            tempList <- ThresholdForOneLevel(GroupWaveletCoefficients[[i]], thresholdMode, t)
            lists <- append(lists, list(tempList))
            i <- i + 1
        }
    }
    return(lists)
}

# --------------------------------- Get ut, lut threshold ut:Universal Threshold lut:Level-universal-Threshold lut is applied to the length of j-th level empirical wavelet coefficients.  ---------------------------------
getUniversalThreshold <- function(groupLength) {
    a <- log(groupLength)
    b <- 2 * a
    c <- b^0.5
    return(c)
}

# --------------------------------- Get ldt threshold ldt:Level-dependent-Threshold ---------------------------------
getLevelDependentThreshold <- function(J, now_level, mean) {
    a <- 2^(-1 * 0.5 * (now_level + 1))
    log2j <- log(2^(J - now_level + 1))
    b <- 2 * log2j
    c <- 4 * (log2j)^2
    d <- 8 * mean * log2j
    t <- a * (b + ((c + d)^0.5))
    return(t)
}

# Thresholding the wavelet coefficients of a layer at a threshold value of t
ThresholdForOneLevel <- function(WaveletCoefficients, thresholdMode, t) {
    coefficientsLength <- length(WaveletCoefficients)
    tempList <- c()
    i <- 1
    while (i <= coefficientsLength) {
        a <- Threshold(WaveletCoefficients[i], t, thresholdMode)
        tempList <- append(tempList, a)
        i <- i + 1
    }
    return(tempList)
}

# Calculating ldt and thresholding the data
ldtThreshold <- function(data, thresholdMode, loop_level, dataLength, lam0) {
    # Highest Resolution
    J <- getHighestResolutionLevel(dataLength)
    # Thresholding the data one by one
    i <- 1
    tempList <- c()
    mean <- lam0/length(data)
    while (i <= length(data)) {
        # Get ldt threshold
        t <- getLevelDependentThreshold(J, loop_level, mean)
        # Threshold processing
        denoise_data <- Threshold(data[[i]], t, thresholdMode)
        tempList <- append(tempList, denoise_data)
        i <- i + 1
    }
    return(tempList)
}

lhtThreshold <- function(original_groups, transform_method, threshold_rule, thresholdMode, next_value) {
    # 偶数番目と奇数番目に分ける
    subgroup_len <- length(original_groups)
    minimum <- optim(par = 0, fn = loss_function, original_group = original_groups, dt = transform_method, thresholdName = threshold_rule, thresholdMode = thresholdMode, next_value = next_value, method = "Brent", lower = -5, upper = 5)$par
    # minimumが負の値になる場合は0にする
    if (minimum < 0) {
        minimum <- 0
    }
    threshold_value <- ((1 - log(2)/log(subgroup_len))^(-0.5)) * minimum
    return(threshold_value)
}

loss_function <- function(t, original_group, dt, thresholdName, thresholdMode, next_value) {
    # Separate even-numbered and odd-numbered
    odd_group <- original_group[seq(1, length(original_group), by = 2)]
    even_group <- original_group[seq(2, length(original_group), by = 2)]

    odd_index <- log(length(odd_group), base = 2)
    even_index <- log(length(even_group), base = 2)

    # Perform WSE for even and odd numbers
    thresholded_odd_group <- wse(odd_group, dt, "none", thresholdMode, 1, odd_index, t)
    thresholded_even_group <- wse(even_group, dt, "none", thresholdMode, 1, even_index, t)

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
Threshold <- function(coe, r, thresholdMode) {
    if (thresholdMode == "h") {
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
FALSE
