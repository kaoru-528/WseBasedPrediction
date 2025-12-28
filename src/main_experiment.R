# # when you run this program for the first time, you have to install thire packages install.packages('tictoc') install.packages('doParallel') install.packages('foreach') Load necessary libraries
library(doParallel)
library(foreach)
library(tictoc)
library(openxlsx)

# Clear the workspace
rm(list = ls())

# Load data set
data_path1 <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/DS/DT_Ans_WSE/A1/D1_Ans_A1_J=3.RData")
data_path2 <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/DS/DT_Ans_WSE/A1/D2_Ans_A1_J=3.RData")
data_path3 <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/DS/DT_Ans_WSE/A1/D3_Ans_A1_J=3.RData")
data_path4 <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/DS/DT_Ans_WSE/A1/D4_Ans_A1_J=3.RData")
data_path5 <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/DS/DT_Ans_WSE/A1/D5_Ans_A1_J=3.RData")
data_path6 <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/DS/DT_Ans_WSE/A1/D6_Ans_A1_J=3.RData")
data_path7 <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/DS/NDT_WSE/D1_none_ldt_J=3.RData")
data_path8 <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/DS/NDT_WSE/D2_none_ldt_J=3.RData")
data_path9 <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/DS/NDT_WSE/D3_none_ldt_J=3.RData")
data_path10 <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/DS/NDT_WSE/D4_none_ldt_J=3.RData")
data_path11 <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/DS/NDT_WSE/D5_none_ldt_J=3.RData")
data_path12 <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/DS/NDT_WSE/D6_none_ldt_J=3.RData")

# Load libraries
src_dir <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/src")
r_files <- list.files(src_dir, pattern = "\\.R$", full.names = TRUE)  # フルパスを取得

# 各Rスクリプトをsourceで読み込む
lapply(r_files, source)

# you can set the precntage of using data for prediction
prediction_percentage <- list(0.3, 0.5, 0.7)

for (p in seq(1, 1, by = 1)) {
    for (o in seq(3, 3, by = 1)) {
        term <- 62
        name <- paste0("DS1_2^3_None_ldt_hard_", prediction_percentage[o])
        load(data_path1)
        data <- soft

        # set the prediction term
        prediction_term <- floor((1 - prediction_percentage[[o]]) * term)
        print("start")

        # Set the number of CPU cores to use
        num_cores <- detectCores()
        cl <- makeCluster(num_cores)
        registerDoParallel(cl)
        data <- soft

        # definition of data
        cs <- data$cs
        ds <- data$ds
        d_ds <- data$denoise_ds

        coe_length <- length(cs)

        tmp_cs_4_1 <- list()

        tmp_ds_2_1 <- list()
        tmp_ds_2_2 <- list()
        tmp_ds_2_3 <- list()
        tmp_ds_2_4 <- list()
        tmp_ds_3_1 <- list()
        tmp_ds_3_2 <- list()
        tmp_ds_4_1 <- list()

        tmp_d_ds_2_1 <- list()
        tmp_d_ds_2_2 <- list()
        tmp_d_ds_2_3 <- list()
        tmp_d_ds_2_4 <- list()
        tmp_d_ds_3_1 <- list()
        tmp_d_ds_3_2 <- list()
        tmp_d_ds_4_1 <- list()

        coe <- list()
        # coe_name_list
        coe_name <- list("C[4][1]", "D[1][1]", "D[1][2]", "D[1][3]", "D[1][4]", "D[2][1]", "D[2][2]", "D[3][1]", "Donise_D[1][1]", "Donise_D[1][2]", "Donise_D[1][3]", "Donise_D[1][4]", "Donise_D[2][1]", "Donise_D[2][2]", "Donise_D[3][1]")

        for (j in seq(1, length(ds) - prediction_term, by = 1)) {
            tmp_cs_4_1 <- c(tmp_cs_4_1, cs[[j]][[4]][1])

            tmp_ds_2_1 <- c(tmp_ds_2_1, ds[[j]][[2]][1])
            tmp_ds_2_2 <- c(tmp_ds_2_2, ds[[j]][[2]][2])
            tmp_ds_2_3 <- c(tmp_ds_2_3, ds[[j]][[2]][3])
            tmp_ds_2_4 <- c(tmp_ds_2_4, ds[[j]][[2]][4])
            tmp_ds_3_1 <- c(tmp_ds_3_1, ds[[j]][[3]][1])
            tmp_ds_3_2 <- c(tmp_ds_3_2, ds[[j]][[3]][2])
            tmp_ds_4_1 <- c(tmp_ds_4_1, ds[[j]][[4]][1])

            tmp_d_ds_2_1 <- c(tmp_d_ds_2_1, d_ds[[j]][[2]][1])
            tmp_d_ds_2_2 <- c(tmp_d_ds_2_2, d_ds[[j]][[2]][2])
            tmp_d_ds_2_3 <- c(tmp_d_ds_2_3, d_ds[[j]][[2]][3])
            tmp_d_ds_2_4 <- c(tmp_d_ds_2_4, d_ds[[j]][[2]][4])
            tmp_d_ds_3_1 <- c(tmp_d_ds_3_1, d_ds[[j]][[3]][1])
            tmp_d_ds_3_2 <- c(tmp_d_ds_3_2, d_ds[[j]][[3]][2])
            tmp_d_ds_4_1 <- c(tmp_d_ds_4_1, d_ds[[j]][[4]][1])
        }

        # coe_list
        coe <- list(tmp_cs_4_1, tmp_ds_2_1, tmp_ds_2_2, tmp_ds_2_3, tmp_ds_2_4, tmp_ds_3_1, tmp_ds_3_2, tmp_ds_4_1, tmp_d_ds_2_1, tmp_d_ds_2_2, tmp_d_ds_2_3, tmp_d_ds_2_4, tmp_d_ds_3_1, tmp_d_ds_3_2, tmp_d_ds_4_1)

        tic()
        prediction_result <- run_parallel_arima_regression(coe, coe_length, prediction_term)
        toc()

        # create_graph(name, data, sorted_best_coe, coe_name, coe_length, prediction_term)

        y <- c(1:coe_length)
        c_4_1 <- c(unlist(coe[[1]]), prediction_result[[1]]$mean)
        d_1_1 <- c(unlist(coe[[2]]), prediction_result[[2]]$mean)
        d_1_2 <- c(unlist(coe[[3]]), prediction_result[[3]]$mean)
        d_1_3 <- c(unlist(coe[[4]]), prediction_result[[4]]$mean)
        d_1_4 <- c(unlist(coe[[5]]), prediction_result[[5]]$mean)
        d_2_1 <- c(unlist(coe[[6]]), prediction_result[[6]]$mean)
        d_2_2 <- c(unlist(coe[[7]]), prediction_result[[7]]$mean)
        d_3_1 <- c(unlist(coe[[8]]), prediction_result[[8]]$mean)

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

        if (p == 1 || p == 3 || p == 5 || p == 7 || p == 9 || p == 11) {
            # print(p) print('ldt_soft')
            denoise_ds <- threshold_for_groups(ds, "s", "ldt")

            i_groups <- inverse_haar_wavelet_transform_for_groups(cs, denoise_ds)
            i_groups <- lapply(i_groups, function(x) x * 8^0.5)

            a_idata <- moving_average(i_groups, term)

            # Perform inverse Anscombe data conversion
            idata <- inverse_anscombe_transform_from_group(a_idata, 1)
        } else if (p == 2 || p == 4 || p == 6 || p == 8 || p == 10 || p == 12) {
            # print(p) print('ldt_hard')
            denoise_ds <- threshold_for_groups(ds, "h", "ldt")

            i_groups <- inverse_haar_wavelet_transform_for_groups(cs, denoise_ds)
            i_groups <- lapply(i_groups, function(x) x * 8^0.5)

            a_idata <- moving_average(i_groups, term)

            # Perform inverse Anscombe data conversion
            idata <- inverse_anscombe_transform_from_group(a_idata, 1)
        }

        prediction <- data.frame(t = numeric(), prediction_data = numeric(), caliculating_time = numeric())

        for (l in seq(length(idata) - prediction_term + 1, length(idata), by = 1)) {
            add_data <- data.frame(t = l, prediction_data = idata[[l]], caliculating_time = 10000)
            prediction <- rbind(prediction, add_data)
        }
        best_coe <- data.frame(a = numeric(), b = numeric(), c = numeric(), d = numeric())
    }
}
