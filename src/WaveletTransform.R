# Get maximum resolution
get_highest_resolution_level <- function(group_length) {
    level <- log2(group_length)
    level <- as.integer(level)
    return(level)
}
# Get a value that satisfies the largest integer power of 2 less than or equal to data_length.  example：when data_length = 62, return 32.  reason：2^5 = 32 < 62 < 64 = 2^6
get_group_length <- function(data_length) {
    i <- 1
    x <- i
    while (i <= data_length) {
        x <- i
        i <- i * 2
    }
    return(x)
}

# Divide data set data of arbitrary length into multiple sub-datasets according to group_length.
get_groups <- function(data, group_length) {
    i <- 0
    data_length <- length(data)
    temp_list <- list()
    while (group_length + i <= data_length) {
        temp_data <- data
        a <- i + 1
        b <- group_length + i
        cut_data <- temp_data[a:b]
        temp_list <- append(temp_list, list(cut_data))
        i <- i + 1
    }
    return(temp_list)
}

# Scale coefficients of discrete Hal wavelets expanded for a set of data
get_scaling_coefficients_from_group <- function(time_list) {
    lists <- list()
    j_level <- get_highest_resolution_level(length(time_list))
    lists <- append(lists, list(time_list))
    j <- 1
    while (j <= j_level) {
        temp_list <- c()
        k <- 1
        while (k <= 2^(j_level - j)) {
            coe <- (1/sqrt(2)) * (lists[[j]][2 * k - 1] + lists[[j]][2 * k])
            temp_list <- append(temp_list, coe)
            k <- k + 1
        }
        lists <- append(lists, list(temp_list))
        j <- j + 1
    }
    return(lists)
}

# Wavelet coefficients of discrete Hal wavelets are simultaneously expanded for multiple data sets
get_wavelet_coefficients_from_group <- function(coe_list) {
    lists <- list()
    j_level <- get_highest_resolution_level(length(coe_list[[1]]))
    lists <- append(lists, list(coe_list[[1]]))
    j <- 1
    while (j <= j_level) {
        temp_list <- c()
        k <- 1
        while (k <= 2^(j_level - j)) {
            c <- (1/sqrt(2)) * (coe_list[[j]][2 * k - 1] - coe_list[[j]][2 * k])
            temp_list <- append(temp_list, c)
            k <- k + 1
        }
        lists <- append(lists, list(temp_list))
        j <- j + 1
    }
    return(lists)
}

# Scale coefficients of the discrete Hal wavelet are simultaneously expanded for multiple data sets
get_scaling_coefficients_from_groups <- function(groups) {
    lists <- list()
    groups_length <- length(groups)
    i <- 1
    while (i <= groups_length) {
        temp_list <- get_scaling_coefficients_from_group(groups[[i]])
        lists <- append(lists, list(temp_list))
        i <- i + 1
    }
    return(lists)
}

# Wavelet coefficients of discrete Hal wavelets unfolded for a set of data
get_wavelet_coefficients_from_groups <- function(cs) {
    lists <- list()
    groups_length <- length(cs)
    i <- 1
    while (i <= groups_length) {
        temp_list <- get_wavelet_coefficients_from_group(cs[[i]])
        lists <- append(lists, list(temp_list))
        i <- i + 1
    }
    return(lists)
}

# Convert a set of Haar wavelet coefficients with Haar scale coefficients to the original data
inverse_haar_wavelet_transform_for_group <- function(scaling_coe, wavelet_coe) {
    group_length <- length(scaling_coe)
    if (group_length != length(wavelet_coe)) {
        return(FALSE)
    }
    j_level <- group_length
    k <- 0
    j <- group_length
    # print('inverse_haar_wavelet_transform_for_group') print(scaling_coe[j])
    while (j > 1) {
        k <- 1
        while (k <= 2^(j_level - j)) {
            scaling_coe[[j - 1]][2 * k - 1] <- (1/sqrt(2)) * (scaling_coe[[j]][k] + wavelet_coe[[j]][k])
            scaling_coe[[j - 1]][2 * k] <- (1/sqrt(2)) * (scaling_coe[[j]][k] - wavelet_coe[[j]][k])
            k <- k + 1
        }
        j <- j - 1
    }
    return(scaling_coe[[1]])
}

# Convert multiple sets of Haar wavelet coefficients with Haar scale coefficients to the original data
inverse_haar_wavelet_transform_for_groups <- function(scaling_coes, wavelet_coes) {
    groups_length <- length(scaling_coes)
    if (groups_length != length(wavelet_coes)) {
        return(FALSE)
    }
    i <- 1
    lists <- list()
    while (i <= groups_length) {
        temp_list <- inverse_haar_wavelet_transform_for_group(scaling_coes[[i]], wavelet_coes[[i]])
        lists <- append(lists, list(temp_list))
        i <- i + 1
    }
    return(lists)
}

# Average multiple sub-datasets by displacement and combine them into one set
moving_average <- function(i_groups, data_length) {
    data_sum <- numeric(data_length)
    counter <- numeric(data_length)
    result <- numeric(data_length)

    groups_sum <- length(i_groups)
    group_length <- length(i_groups[[1]])
    i <- 1
    while (i <= groups_sum) {
        j <- 1
        while (j <= group_length) {
            data_sum[i + j - 1] <- data_sum[i + j - 1] + i_groups[[i]][j]
            counter[i + j - 1] <- counter[i + j - 1] + 1
            j <- j + 1
        }
        i <- i + 1
    }

    k <- 1
    while (k <= data_length) {
        result[k] <- data_sum[k]/counter[k]
        if (result[k] < 0) {
            result[k] <- 0
        }
        k <- k + 1
    }
    return(result)
}

# Load data from file
load_data <- function(data_path) {
    data_path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), data_path)
    ds <- read.table(data_path)[2]
    ds <- as.numeric(ds$V2)
    return(ds)
}
