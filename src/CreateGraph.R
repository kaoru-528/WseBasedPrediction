create_graph_for_swwsp <- function(prediction_result, all_coefficients_data, coe_length, prediction_term, name) {
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

create_graph_for_mwwp <- function(prediction_result, all_coefficients_data, coe_length, prediction_term, name, resolution) {
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
