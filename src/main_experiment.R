# # when you run this program for the first time, you have to install thire packages
# install.packages("tictoc")
# install.packages("doParallel")
# install.packages("foreach")

# Load data set
dataPath1 <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/DS/DT_Ans_WSE/A1/D1_Ans_A1_J=3.RData")
dataPath2 <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/DS/DT_Ans_WSE/A1/D2_Ans_A1_J=3.RData")
dataPath3 <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/DS/DT_Ans_WSE/A1/D3_Ans_A1_J=3.RData")
dataPath4 <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/DS/DT_Ans_WSE/A1/D4_Ans_A1_J=3.RData")
dataPath5 <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/DS/DT_Ans_WSE/A1/D5_Ans_A1_J=3.RData")
dataPath6 <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/DS/DT_Ans_WSE/A1/D6_Ans_A1_J=3.RData")
dataPath7 <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/DS/NDT_WSE/D1_none_ldt_J=3.RData")
dataPath8 <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/DS/NDT_WSE/D2_none_ldt_J=3.RData")
dataPath9 <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/DS/NDT_WSE/D3_none_ldt_J=3.RData")
dataPath10 <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/DS/NDT_WSE/D4_none_ldt_J=3.RData")
dataPath11 <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/DS/NDT_WSE/D5_none_ldt_J=3.RData")
dataPath12 <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/DS/NDT_WSE/D6_none_ldt_J=3.RData")


# Load creating graph module
createGraph_Path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/createGraph.R")
source(createGraph_Path)

# Load creating regression module
regression_Path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/regression.R")
source(regression_Path)

# Load wavelet conversion module
WaveletTransform_Path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/WaveletTransform.R")
source(WaveletTransform_Path)

# Load data conversion module
DT_Path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/DataTransform.R")
source(DT_Path)

# Load Threshold Module
Threshold_Path <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/Threshold.R")
source(Threshold_Path)

# Load necessary libraries
library(doParallel)
library(foreach)
library(tictoc)
library(openxlsx)

# you can set the precntage of using data for prediction
prediction_percentage <- list(0.3, 0.7, 0.5)

for (p in seq(1, 12, by = 1)) {
    for (o in seq(1, 3, by = 1)) {
        # if(p == 1){
        #     term = 100
        #     name = paste0("DS5_2^3_A1_ut_hard_", prediction_percentage[o])
        #     load(dataPath5)
        #     data = hard
        # }
        # else if(p == 2){
        #     name = paste0("DS5_2^3_A1_ut_soft_", prediction_percentage[o])
        #     data = soft
        # }
        # else if (p == 3){
        #     term = 59
        #     name = paste0("DS6_2^3_A1_ut_hard_", prediction_percentage[o])
        #     load(dataPath6)
        #     data = hard
        # }
        # else if(p == 4){
        #     name = paste0("DS6_2^3_A1_ut_soft_", prediction_percentage[o])
        #     data = soft
        # }
        # else if (p == 5){
        #     term = 73
        #     name = paste0("DS3_2^3_A1_ut_hard_", prediction_percentage[o])
        #     load(dataPath3)
        #     data = hard
        # }
        # else if(p == 6){
        #     name = paste0("DS3_2^3_A1_ut_soft_", prediction_percentage[o])
        #     data = soft
        # }
        # else if (p == 7){
        #     term = 81
        #     name = paste0("DS4_2^3_A1_ut_hard_", prediction_percentage[o])
        #     load(dataPath4)
        #     data = hard
        # }
        # else if(p == 8){
        #     name = paste0("DS4_2^3_A1_ut_soft_", prediction_percentage[o])
        #     data = soft
        # }
        if (p == 1) {
            term <- 62
            name <- paste0("DS1_2^3_None_ldt_hard_", prediction_percentage[o])
            load(dataPath1)
            data <- soft
        } else if (p == 2) {
            name <- paste0("DS1_2^3_None_ldt_soft_", prediction_percentage[o])
            data <- hard
        } else if (p == 3) {
            term <- 41
            name <- paste0("DS2_2^3_None_ldt_hard_", prediction_percentage[o])
            load(dataPath2)
            data <- soft
        } else if (p == 4) {
            name <- paste0("DS2_2^3_None_ldt_soft_", prediction_percentage[o])
            data <- hard
        } else if (p == 5) {
            term <- 73
            name <- paste0("DS3_2^3_None_ldt_hard_", prediction_percentage[o])
            load(dataPath3)
            data <- soft
        } else if (p == 6) {
            name <- paste0("DS3_2^3_None_ldt_soft_", prediction_percentage[o])
            data <- hard
        } else if (p == 7) {
            term <- 81
            name <- paste0("DS4_2^3_None_ldt_hard_", prediction_percentage[o])
            load(dataPath4)
            data <- soft
        } else if (p == 8) {
            name <- paste0("DS4_2^3_None_ldt_soft_", prediction_percentage[o])
            data <- hard
        } else if (p == 9) {
            term <- 100
            name <- paste0("DS5_2^3_None_ldt_soft_", prediction_percentage[o])
            load(dataPath11)
            data <- soft
        } else if (p == 10) {
            name <- paste0("DS5_2^3_None_ldt_hard_", prediction_percentage[o])
            data <- hard
        } else if (p == 11) {
            term <- 59
            name <- paste0("DS6_2^3_None_ldt_soft_", prediction_percentage[o])
            load(dataPath12)
            data <- soft
        } else if (p == 12) {
            name <- paste0("DS6_2^3_None_ldt_hard_", prediction_percentage[o])
            data <- hard
        }
        # else if (p == 13){
        #     term = 73
        #     name = paste0("DS3_2^3_None_ldt_soft_", prediction_percentage[o])
        #     load(dataPath7)
        #     data = soft
        # }
        # else if (p == 14){
        #     term = 73
        #     name = paste0("DS3_2^3_None_ldt_hard_", prediction_percentage[o])
        #     load(dataPath7)
        #     data = hard
        # }
        # else if (p == 15){
        #     term = 81
        #     name = paste0("DS4_2^3_None_ldt_soft_", prediction_percentage[o])
        #     load(dataPath8)
        #     data = soft
        # }
        # else if (p == 16){
        #     term = 81
        #     name = paste0("DS4_2^3_None_ldt_hard_", prediction_percentage[o])
        #     load(dataPath8)
        #     data = hard
        # }

        # set the prediction term
        prediction_term <- floor((1 - prediction_percentage[[o]]) * term)
        print("start")

        # Set the number of CPU cores to use
        num_cores <- detectCores()
        cl <- makeCluster(num_cores)
        registerDoParallel(cl)
        data <- soft

        # definition of data
        Cs <- data$Cs
        Ds <- data$Ds
        dDs <- data$Denoise_Ds

        coe_length <- length(Cs)

        tmp_Cs_4_1 <- list()

        tmp_Ds_2_1 <- list()
        tmp_Ds_2_2 <- list()
        tmp_Ds_2_3 <- list()
        tmp_Ds_2_4 <- list()
        tmp_Ds_3_1 <- list()
        tmp_Ds_3_2 <- list()
        tmp_Ds_4_1 <- list()

        tmp_dDs_2_1 <- list()
        tmp_dDs_2_2 <- list()
        tmp_dDs_2_3 <- list()
        tmp_dDs_2_4 <- list()
        tmp_dDs_3_1 <- list()
        tmp_dDs_3_2 <- list()
        tmp_dDs_4_1 <- list()

        coe <- list()
        # coe_name_list
        coe_name <- list("C[4][1]", "D[1][1]", "D[1][2]", "D[1][3]", "D[1][4]", "D[2][1]", "D[2][2]", "D[3][1]", "Donise_D[1][1]", "Donise_D[1][2]", "Donise_D[1][3]", "Donise_D[1][4]", "Donise_D[2][1]", "Donise_D[2][2]", "Donise_D[3][1]")

        for (j in seq(1, length(Ds) - prediction_term, by = 1)) {
            tmp_Cs_4_1 <- c(tmp_Cs_4_1, Cs[[j]][[4]][1])

            tmp_Ds_2_1 <- c(tmp_Ds_2_1, Ds[[j]][[2]][1])
            tmp_Ds_2_2 <- c(tmp_Ds_2_2, Ds[[j]][[2]][2])
            tmp_Ds_2_3 <- c(tmp_Ds_2_3, Ds[[j]][[2]][3])
            tmp_Ds_2_4 <- c(tmp_Ds_2_4, Ds[[j]][[2]][4])
            tmp_Ds_3_1 <- c(tmp_Ds_3_1, Ds[[j]][[3]][1])
            tmp_Ds_3_2 <- c(tmp_Ds_3_2, Ds[[j]][[3]][2])
            tmp_Ds_4_1 <- c(tmp_Ds_4_1, Ds[[j]][[4]][1])

            tmp_dDs_2_1 <- c(tmp_dDs_2_1, dDs[[j]][[2]][1])
            tmp_dDs_2_2 <- c(tmp_dDs_2_2, dDs[[j]][[2]][2])
            tmp_dDs_2_3 <- c(tmp_dDs_2_3, dDs[[j]][[2]][3])
            tmp_dDs_2_4 <- c(tmp_dDs_2_4, dDs[[j]][[2]][4])
            tmp_dDs_3_1 <- c(tmp_dDs_3_1, dDs[[j]][[3]][1])
            tmp_dDs_3_2 <- c(tmp_dDs_3_2, dDs[[j]][[3]][2])
            tmp_dDs_4_1 <- c(tmp_dDs_4_1, dDs[[j]][[4]][1])
        }

        # coe_list
        coe <- list(tmp_Cs_4_1, tmp_Ds_2_1, tmp_Ds_2_2, tmp_Ds_2_3, tmp_Ds_2_4, tmp_Ds_3_1, tmp_Ds_3_2, tmp_Ds_4_1, tmp_dDs_2_1, tmp_dDs_2_2, tmp_dDs_2_3, tmp_dDs_2_4, tmp_dDs_3_1, tmp_dDs_3_2, tmp_dDs_4_1)



        # regression function
        f <- function(x, a, b, c, d) {
            (a * sin((b * x) + c)) + d
        }

        # cal coe in regression function
        run_regression <- function(j) {
            x <- c(1:(coe_length - prediction_term))
            a_data <- data.frame(mse = numeric(), a = numeric(), b = numeric(), c = numeric(), d = numeric())
            coe <- list(tmp_Cs_4_1, tmp_Ds_2_1, tmp_Ds_2_2, tmp_Ds_2_3, tmp_Ds_2_4, tmp_Ds_3_1, tmp_Ds_3_2, tmp_Ds_4_1, tmp_dDs_2_1, tmp_dDs_2_2, tmp_dDs_2_3, tmp_dDs_2_4, tmp_dDs_3_1, tmp_dDs_3_2, tmp_dDs_4_1)
            tmp_coe <- unlist(coe[[j]])
            for (sub_a in seq(0.5, 10, by = 0.5)) {
                for (sub_b in seq(0.5, 10, by = 0.5)) {
                    for (sub_c in seq(0.5, 10, by = 0.5)) {
                        for (sub_d in seq(0, 10, by = 0.5)) {
                            fit <- nls(tmp_coe ~ f(x, a, b, c, d), start = list(a = sub_a, b = sub_b, c = sub_c, d = sub_d), control = nls.control(warnOnly = TRUE))
                            params <- coef(fit)
                            pre <- f(x, params[1], params[2], params[3], params[4])
                            mse <- mean((unlist(coe[[j]]) - pre)^2)
                            add_data <- data.frame(mse = mse, a = params[1], b = params[2], c = params[3], d = params[4])
                            a_data <- rbind(a_data, add_data)
                        }
                    }
                }
            }
            row.names(a_data) <- NULL
            a_data <- a_data[order(a_data$mse, decreasing = F), ]
            return(a_data)
        }
        # start cal execution time
        tic()
        # Use foreach for parallel processing easyliy
        sorted_best_coe <- foreach(j = seq(1, 8, by = 1)) %dopar% run_regression(j)
        # stop parallel processing
        stopCluster(cl)
        # stop cal execution time
        time <- toc()

        createGraph(name, data, sorted_best_coe, coe_name, coe_length, prediction_term)

        y <- c(1:coe_length)
        C_4_1 <- f(y, sorted_best_coe[[1]]$a[[1]], sorted_best_coe[[1]]$b[[1]], sorted_best_coe[[1]]$c[[1]], sorted_best_coe[[1]]$d[[1]])
        D_1_1 <- f(y, sorted_best_coe[[2]]$a[[1]], sorted_best_coe[[2]]$b[[1]], sorted_best_coe[[2]]$c[[1]], sorted_best_coe[[2]]$d[[1]])
        D_1_2 <- f(y, sorted_best_coe[[3]]$a[[1]], sorted_best_coe[[3]]$b[[1]], sorted_best_coe[[3]]$c[[1]], sorted_best_coe[[3]]$d[[1]])
        D_1_3 <- f(y, sorted_best_coe[[4]]$a[[1]], sorted_best_coe[[4]]$b[[1]], sorted_best_coe[[4]]$c[[1]], sorted_best_coe[[4]]$d[[1]])
        D_1_4 <- f(y, sorted_best_coe[[5]]$a[[1]], sorted_best_coe[[5]]$b[[1]], sorted_best_coe[[5]]$c[[1]], sorted_best_coe[[5]]$d[[1]])
        D_2_1 <- f(y, sorted_best_coe[[6]]$a[[1]], sorted_best_coe[[6]]$b[[1]], sorted_best_coe[[6]]$c[[1]], sorted_best_coe[[6]]$d[[1]])
        D_2_2 <- f(y, sorted_best_coe[[7]]$a[[1]], sorted_best_coe[[7]]$b[[1]], sorted_best_coe[[7]]$c[[1]], sorted_best_coe[[7]]$d[[1]])
        D_3_1 <- f(y, sorted_best_coe[[8]]$a[[1]], sorted_best_coe[[8]]$b[[1]], sorted_best_coe[[8]]$c[[1]], sorted_best_coe[[8]]$d[[1]])

        for (k in seq(coe_length - prediction_term + 1, coe_length, by = 1)) {
            Cs[[k]][[4]][1] <- C_4_1[k]

            Ds[[k]][[2]][1] <- D_1_1[[k]]
            Ds[[k]][[2]][2] <- D_1_2[[k]]
            Ds[[k]][[2]][3] <- D_1_3[[k]]
            Ds[[k]][[2]][4] <- D_1_4[[k]]
            Ds[[k]][[3]][1] <- D_2_1[[k]]
            Ds[[k]][[3]][2] <- D_2_2[[k]]
            Ds[[k]][[4]][1] <- D_3_1[[k]]
        }
        # if(p == 1 || p == 3){
        #     # print(p)
        #     # print("ut_soft")
        #     Denoise_Ds = ThresholdForGroups(Ds,"h","ut",)

        #     i_groups = inverseHaarWaveletTransformForGroups(Cs,Denoise_Ds)
        #     i_groups = lapply(i_groups, function(x) x*8**0.5)

        #     a_idata = movingAverage(i_groups,term)

        #     # Perform inverse Anscombe data conversion
        #     idata = inverseAnscombeTransformFromGroup(a_idata,1)
        # }
        # else if(p == 2 || p == 4){
        #     # print(p)
        #     # print("ut_soft")
        #     Denoise_Ds = ThresholdForGroups(Ds,"s","ut")

        #     i_groups = inverseHaarWaveletTransformForGroups(Cs,Denoise_Ds)
        #     i_groups = lapply(i_groups, function(x) x*8**0.5)

        #     a_idata = movingAverage(i_groups,term)

        #     # Perform inverse Anscombe data conversion
        #     idata = inverseAnscombeTransformFromGroup(a_idata,1)
        # }
        if (p == 1 || p == 3 || p == 5 || p == 7 || p == 9 || p == 11) {
            # print(p)
            # print("ldt_soft")
            Denoise_Ds <- ThresholdForGroups(Ds, "s", "ldt")

            i_groups <- inverseHaarWaveletTransformForGroups(Cs, Denoise_Ds)
            i_groups <- lapply(i_groups, function(x) x * 8**0.5)

            a_idata <- movingAverage(i_groups, term)

            # Perform inverse Anscombe data conversion
            idata <- inverseAnscombeTransformFromGroup(a_idata, 1)
        } else if (p == 2 || p == 4 || p == 6 || p == 8 || p == 10 || p == 12) {
            # print(p)
            # print("ldt_hard")
            Denoise_Ds <- ThresholdForGroups(Ds, "h", "ldt")

            i_groups <- inverseHaarWaveletTransformForGroups(Cs, Denoise_Ds)
            i_groups <- lapply(i_groups, function(x) x * 8**0.5)

            a_idata <- movingAverage(i_groups, term)

            # Perform inverse Anscombe data conversion
            idata <- inverseAnscombeTransformFromGroup(a_idata, 1)
        }

        prediction <- data.frame(t = numeric(), prediction_data = numeric(), caliculating_time = numeric())

        for (l in seq(length(idata) - prediction_term + 1, length(idata), by = 1)) {
            add_data <- data.frame(t = l, prediction_data = idata[[l]], caliculating_time = time$callback_msg)
            prediction <- rbind(prediction, add_data)
        }
        best_coe <- data.frame(a = numeric(), b = numeric(), c = numeric(), d = numeric())
        for (m in seq(1, 8, by = 1)) {
            tmp_best_coe <- data.frame(a = sorted_best_coe[[m]]$a[[1]], b = sorted_best_coe[[m]]$b[[1]], c = sorted_best_coe[[m]]$c[[1]], d = sorted_best_coe[[m]]$d[[1]])
            best_coe <- rbind(best_coe, tmp_best_coe)
        }
        best_coe_filename <- paste0("./OUTPUT/", name, "_best_coe.xlsx")
        write.xlsx(best_coe, best_coe_filename)
        prediction_filename <- paste0("./OUTPUT/", name, ".xlsx")
        write.xlsx(prediction, prediction_filename)
        variable_path <- paste0("./OUTPUT/", name, "_best_coe.RData")
        save(sorted_best_coe, file = variable_path)
    }
}
