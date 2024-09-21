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
