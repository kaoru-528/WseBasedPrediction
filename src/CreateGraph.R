createGraph <- function(name, data, sorted_best_coe, coe_name, coeLength, predictionTerm) {
    for (i in seq(1, 8, by = 1)) {
        # definition of data
        Cs <- data$Cs
        Ds <- data$Ds
        dDs <- data$Denoise_Ds

        coeLength <- length(Cs)

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


        for (j in seq(1, length(Ds) - predictionTerm, by = 1)) {
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

        title <- paste0(name, "\n", coe_name[[i]])
        filename_graph <- paste0("./OUTPUT/", name, "_", coe_name[[i]], ".png")
        print(filename_graph)
        png(filename_graph, width = 1344, height = 914)
        x <- c(1:(coeLength - predictionTerm))
        f <- function(x, a, b, c, d) {
            (a * sin((b * x) + c)) + d
        }
        plot(x, coe[[i]], main = title, xlab = "number", ylab = "C", pch = 16, col = "blue", type = "b")
        lines(x, f(x, sorted_best_coe[[i]]$a[[1]], sorted_best_coe[[i]]$b[[1]], sorted_best_coe[[i]]$c[[1]], sorted_best_coe[[i]]$d[[1]]), col = "red")
        dev.off()
    }
}

CreateGraphForArimaBasedPrediction <- function(prediction_result, all_coefficients_data, coeLength, predictionTerm, name) {
    for (i in seq(1, 8, by = 1)) {
        coe_name <- list("C[0][0]", "D[2][0]", "D[2][1]", "D[2][2]", "D[2][3]", "D[1][0]", "D[1][1]", "D[0][0]")
        file_name <- paste0(name, coe_name[[i]], ".png")
        x_label <- paste0("number")
        if (coe_name[[i]] == "C[0][0]") {
            y_label <- paste0("scaling coefficients value ", coe_name[[i]])
        } else {
            y_label <- paste0("wavelet coefficients value ", coe_name[[i]])
        }
        all_coefficient_data <- all_coefficients_data[[i]]
        prediction_coefficient_data <- prediction_result[[i]]$mean

        x1 <- 1:length(all_coefficient_data)
        y1 <- as.numeric(all_coefficient_data)
        x2 <- seq(length(all_coefficient_data) - length(prediction_coefficient_data) + 1, length(all_coefficient_data))
        y2 <- as.numeric(prediction_coefficient_data)

        plot <- ggplot() + geom_line(aes(x = x1, y = y1), color = "blue", linewidth = 1) + geom_point(aes(x = x1, y = y1), color = "blue", size = 4) + geom_line(aes(x = x2, y = y2), color = "red", linewidth = 1) + geom_point(aes(x = x2, y = y2), color = "red", size = 4) + geom_vline(xintercept = length(all_coefficient_data) - length(prediction_coefficient_data) + 1, linetype = "dotted", color = "black", linewidth = 1) + labs(x = x_label, y = y_label) + theme(panel.background = element_rect(fill = "transparent",
            color = "black", linewidth = 0.2), panel.grid = element_blank(), text = element_text(size = 24), legend.title = element_blank(), legend.background = element_rect(fill = "white", color = "black", linewidth = 0.2), legend.position.inside = c(0.005, 0.83), legend.justification = c(0, 0), legend.key.size = unit(1, "cm"), legend.key = element_rect(fill = "white", color = "white"))

        # プロットを保存
        ggsave(file_name, plot = plot, width = 13.44, height = 9.14)
    }
}

CreateGraphForWaveleDecomposePrediction <- function(prediction_result, all_coefficients_data, coeLength, predictionTerm, name, resolution) {
    for (i in seq(1, resolution + 1, by = 1)) {
        coe_name <- list()
        for (j in seq(1, resolution + 1, by = 1)) {
            if (j == 1) {
                coe_name <- c(coe_name, paste0("f_", resolution))
            } else {
                coe_name <- c(coe_name, paste0("g", j - 1))
            }
        }
        file_name <- paste0(name, coe_name[[i]], ".png")
        x_label <- paste0("number")
        if (coe_name[[i]] == "C[0][0]") {
            y_label <- paste0("scaling coefficient value ", coe_name[[i]])
        } else {
            y_label <- paste0("wavelet coefficient value ", coe_name[[i]])
        }
        all_coefficient_data <- all_coefficients_data[[i]]
        prediction_coefficient_data <- prediction_result[[i]]$mean

        x1 <- 1:length(all_coefficient_data)
        y1 <- as.numeric(all_coefficient_data)
        x2 <- seq(length(all_coefficient_data) - length(prediction_coefficient_data) + 1, length(all_coefficient_data))
        y2 <- as.numeric(prediction_coefficient_data)

        plot <- ggplot() + geom_line(aes(x = x1, y = y1), color = "blue", linewidth = 1) + geom_point(aes(x = x1, y = y1), color = "blue", size = 4) + geom_line(aes(x = x2, y = y2), color = "red", linewidth = 1) + geom_point(aes(x = x2, y = y2), color = "red", size = 4) + geom_vline(xintercept = length(all_coefficient_data) - length(prediction_coefficient_data) + 1, linetype = "dotted", color = "black", linewidth = 1) + labs(x = x_label, y = y_label) + theme(panel.background = element_rect(fill = "transparent",
            color = "black", linewidth = 0.2), panel.grid = element_blank(), text = element_text(size = 24), legend.title = element_blank(), legend.background = element_rect(fill = "white", color = "black", linewidth = 0.2), legend.position.inside = c(0.005, 0.83), legend.justification = c(0, 0), legend.key.size = unit(1, "cm"), legend.key = element_rect(fill = "white", color = "white"))

        # プロットを保存
        ggsave(file_name, plot = plot, width = 13.44, height = 9.14)
    }
}
FALSE
