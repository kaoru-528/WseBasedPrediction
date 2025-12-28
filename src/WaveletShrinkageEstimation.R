# Load wavelet conversion module
wavelet_transform_path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/src/WaveletTransform.R")
source(wavelet_transform_path)
# Load data conversion module
dt_path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/src/DataTransform.R")
source(dt_path)
# Load Threshold Module
threshold_path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/src/Threshold.R")
source(threshold_path)

# Hal wavelet estimation without data transformation
wse <- function(data, dt, threshold_name, threshold_mode, var = 1, index, init_threshold_value) {
    if (dt == "none" && threshold_name != "ldt") {
        print("Please chack the parameter. If you want to use dt=none, please set threshold_name=ldt.")
    } else if (dt != "none" && threshold_name == "ldt") {
        print("Please chack the parameter. If you want to use threshold_name=ldt, please set dt=none.")
    } else {
        group_length <- 2^index
        # Get data length
        data_length <- length(data)
        if (group_length >= get_group_length(data_length)) {
            # Get subdata length
            group_length <- get_group_length(data_length)
        }

        # Cut the original data into a number of sub-data of length 2^J
        groups <- get_groups(data, group_length)

        if (dt == "Fi") {
            # Transform the sub-data into Gaussian data by Fisz transformation
            cs1 <- get_scaling_coefficients_from_groups(groups)
            ds1 <- get_wavelet_coefficients_from_groups(cs1)
            fi1 <- fisz_transform_from_groups(cs1, ds1, var)
            fisz_groups <- inverse_haar_wavelet_transform_for_groups(cs1, fi1)

            fisz_groups <- lapply(fisz_groups, function(x) x/group_length^0.5)

            # Calculate c
            cs2 <- get_scaling_coefficients_from_groups(fisz_groups)
            # Calculate d
            ds2 <- get_wavelet_coefficients_from_groups(cs2)

            # Noise reduction of wavelet coefficients using threshold_mode noise reduction rule, threshold_name threshold
            denoise_ds2 <- threshold_for_groups(ds2, threshold_mode, threshold_name)

            # Perform inverse Fisz data conversion
            f_igroups <- inverse_haar_wavelet_transform_for_groups(cs2, denoise_ds2)
            cs3 <- get_scaling_coefficients_from_groups(f_igroups)
            fs2 <- get_wavelet_coefficients_from_groups(cs3)
            cds <- inverse_fisz_transform_from_groups(cs3, fs2, var)
            cs4 <- cds[[1]]
            ds3 <- cds[[2]]

            # Perform inverse wavelet conversion
            thresholded_groups <- inverse_haar_wavelet_transform_for_groups(cs4, ds3)
            thresholded_groups <- lapply(thresholded_groups, function(x) x * group_length^0.5)

            # Perform moving average
            thresholded_data <- moving_average(thresholded_groups, data_length)

            thresholded_data <- list(estimation_data = thresholded_data, cs = cs4, ds = ds3, denoise_ds = denoise_ds2)
        } else {
            if (dt == "A1" || dt == "A2" || dt == "A3") {
                # Transform sub-data to Gaussian data by Anscombe
                groups <- anscombe_transform_from_groups(groups, var)
            } else if (dt == "B1") {
                # Transform sub-data to Gaussian data by Bartlet
                groups <- bartlett_transform_from_groups(groups, var)
            } else if (dt == "B2") {
                # Transform sub-data to Gaussian data by Bartlet
                groups <- bartlett_transform2_from_groups(groups, var)
            } else if (dt == "Fr") {
                groups <- freeman_transform_from_groups(groups, var)
            } else {
                groups <- groups
            }
            groups <- lapply(groups, function(x) x/(group_length^0.5))

            # Calculate c
            cs <- get_scaling_coefficients_from_groups(groups)
            # Calculate d
            ds <- get_wavelet_coefficients_from_groups(cs)

            denoise_ds <- threshold_for_groups(ds, threshold_mode, threshold_name, dt, groups, init_threshold_value)
            # Perform inverse wavelet conversion
            thresholded_groups <- inverse_haar_wavelet_transform_for_groups(cs, denoise_ds)
            thresholded_groups <- lapply(thresholded_groups, function(x) x * group_length^0.5)

            # Perform moving average
            if (threshold_name == "none") {
                thresholded_data <- thresholded_groups
                print("none")
            } else {
                thresholded_data <- moving_average(thresholded_groups, data_length)
            }

            if (dt == "A1") {
                # Perform inverse Anscombe data conversion
                thresholded_data <- inverse_anscombe_transform_from_group(thresholded_data, var)
            } else if (dt == "A2") {
                # Perform inverse Anscombe data conversion
                thresholded_data <- inverse_anscombe_transform2_from_group(thresholded_data, var)
            } else if (dt == "A3") {
                # Perform inverse Anscombe data conversion
                thresholded_data <- inverse_anscombe_transform3_from_group(thresholded_data, var)
            } else if (dt == "B1") {
                # Perform inverse Anscombe data conversion
                thresholded_data <- inverse_bartlett_transform_from_group(thresholded_data, var)
            } else if (dt == "B2") {
                # Perform inverse Anscombe data conversion
                thresholded_data <- inverse_bartlett_transform2_from_group(thresholded_data, var)
            } else if (dt == "Fr") {
                thresholded_data <- inverse_freeman_transform_from_group(thresholded_data, var)
            } else {
                thresholded_data <- thresholded_data
            }
            thresholded_data <- list(estimation_data = thresholded_data, cs = cs, ds = ds, denoised_ds = denoise_ds)
        }

        return(thresholded_data)
    }
}

# Translation-invariant Hal wavelet estimation without data transformation
tipsh <- function(data, threshold_mode, var, index) {
    threshold_name <- "ldt"
    group_length <- 2^index
    # Get data length
    data_length <- length(data)
    if (group_length >= get_group_length(data_length)) {
        # Get subdata length
        group_length <- get_group_length(data_length)
    }
    # Cut the original data into a number of sub-data of length 2^J
    groups <- get_groups(data, group_length)
    groups <- lapply(groups, function(x) x/group_length^0.5)
    thresholded_groups <- list()
    # Transration-Invariant Denoising
    for (i in 1:(data_length - group_length + 1)) {
        templist <- groups[[i]]
        shiftgroup <- list()
        lists <- list()
        for (h in 1:(group_length - 1)) {
            shiftgroup <- list(c(templist[(h + 1):group_length], templist[1:h]))
            cs <- get_scaling_coefficients_from_group(as.numeric(shiftgroup[[1]]))
            ds <- get_wavelet_coefficients_from_group(cs)
            denoise_ds <- threshold_for_group(ds, threshold_mode, threshold_name)
            thresholded_group <- inverse_haar_wavelet_transform_for_group(cs, denoise_ds)
            lists <- append(lists, list(thresholded_group))
        }
        cs <- get_scaling_coefficients_from_group(templist)
        ds <- get_wavelet_coefficients_from_group(cs)
        denoise_ds <- threshold_for_group(ds, threshold_mode, threshold_name)
        thresholded_group <- inverse_haar_wavelet_transform_for_group(cs, denoise_ds)
        lists <- append(lists, list(thresholded_group))
        for (h in 1:(group_length - 1)) {
            templist <- lists[[h]]
            lists[h] <- list(c(templist[(group_length - h + 1):(group_length)], templist[1:(group_length - h)]))
        }
        sumlist <- c()
        for (j in 1:group_length) {
            sl <- 0
            for (k in 1:group_length) {
                sl <- sl + lists[[k]][j]
            }
            sumlist <- c(sumlist, sl)
        }
        thresholded_groups[[i]] <- (sumlist/group_length)
    }

    thresholded_groups <- lapply(thresholded_groups, function(x) x * group_length^0.5)

    # Perform moving average
    thresholded_data <- moving_average(thresholded_groups, data_length)
    thresholded_data <- list(estimation_data = thresholded_data, cs = cs, ds = ds, denoised_ds = denoise_ds)

    # Return Results
    return(thresholded_data)
}

# cumulative function
to_cul_data <- function(data) {
    cul_data <- c()
    old_value <- 0
    index <- 1
    while (index <= length(data)) {
        now_value <- data[[index]] + old_value
        cul_data <- append(cul_data, now_value)
        old_value <- now_value
        index <- index + 1
    }
    return(cul_data)
}

# Load data from file
load_data <- function(data_path) {
    data_path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), data_path)
    ds <- read.table(data_path)[2]
    ds <- as.numeric(ds$V2)
    return(ds)
}


# creating file format
create_file <- function(i, result_path, time) {
    file_name_edata <- paste0(time, "_edata_J=", i, ".csv")
    file_name_coe <- paste0(time, "_coe_J=", i, ".csv")
    file_name_variable <- paste0(time, "_var_J=", i, ".RData")
    edata <- paste0(result_path, file_name_edata)
    coe <- paste0(result_path, file_name_coe)
    variable <- paste0(result_path, file_name_variable)
    file_path <- list(edata = edata, coe = coe, variable = variable)
    return(file_path)
}

# creating result
create_result <- function(hard, soft, index, result_path) {
    time <- Sys.time() %>%
        format("%H-%M-%S")
    edata <- list(hard = round(hard$estimation_data, digits = 3), soft = round(soft$estimation_data, digits = 3))
    hard_coe <- rbind("cs", as.data.frame(t(sapply(hard$cs, unlist))), "ds", as.data.frame(t(sapply(hard$ds, unlist))), "denoise_ds", as.data.frame(t(sapply(hard$denoise_ds, unlist))))
    soft_coe <- rbind("cs", as.data.frame(t(sapply(soft$cs, unlist))), "ds", as.data.frame(t(sapply(soft$ds, unlist))), "denoise_ds", as.data.frame(t(sapply(soft$denoise_ds, unlist))))
    coe <- rbind("hard", hard_coe, "soft", soft_coe)
    file_path <- create_file(index, result_path, time)
    write.csv(edata, file_path$edata, row.names = FALSE)
    write.csv(coe, file_path$coe, row.names = FALSE)
    save(hard, soft, file = file_path$variable)
}
