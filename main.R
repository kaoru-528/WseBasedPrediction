# Load data set
dataPath = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/DS/NDT_WSE/NDT_J=3.RData")
load(dataPath)

# Load creating graph module
createGraph_Path = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/createGraph.R")
source(createGraph_Path)

name = "NDT_soft"
createGraph(soft, name)
name = "NDT_hard"
createGraph(hard, name)
name = "Ans_A1_ut_hard"
createGraph(ut_hard, name)
name = "Ans_A1_ut_soft"
createGraph(ut_soft, name)


# # NDT
# plot(c(1:length(tmp_Cs_4_1)), tmp_Cs_4_1,main = "NDT\nC[4][1]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")

# plot(c(1:length(tmp_Ds_2_1)), tmp_Ds_2_1,main = "NDT\nD[1][1]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")
# plot(c(1:length(tmp_Ds_2_2)), tmp_Ds_2_2,main = "NDT\nD[1][2]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")
# plot(c(1:length(tmp_Ds_2_3)), tmp_Ds_2_3,main = "NDT\nD[1][3]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")
# plot(c(1:length(tmp_Ds_2_4)), tmp_Ds_2_4,main = "NDT\nD[1][4]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")
# plot(c(1:length(tmp_Ds_3_1)), tmp_Ds_3_1,main = "NDT\nD[2][1]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")
# plot(c(1:length(tmp_Ds_3_2)), tmp_Ds_3_2,main = "NDT\nD[2][2]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")
# plot(c(1:length(tmp_Ds_4_1)), tmp_Ds_4_1,main = "NDT\nD[3][1]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")

# # DT_Ans_A1
# png("C[4][1].png", width = 1344, height = 914 )
# plot(c(1:length(tmp_Cs_4_1)), tmp_Cs_4_1,main = "DT_Ans_A1\nC[4][1]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")
# dev.off()

# png("C[4][1].png", width = 1344, height = 914 )
# plot(c(1:length(tmp_Ds_2_1)), tmp_Ds_2_1,main = "DT_Ans_A1\nD[1][1]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")
# dev.off()

# png("C[4][1].png", width = 1344, height = 914 )
# plot(c(1:length(tmp_Ds_2_2)), tmp_Ds_2_2,main = "DT_Ans_A1\nD[1][2]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")

# png("C[4][1].png", width = 1344, height = 914 )
# plot(c(1:length(tmp_Ds_2_3)), tmp_Ds_2_3,main = "DT_Ans_A1\nD[1][3]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")

# png("C[4][1].png", width = 1344, height = 914 )
# plot(c(1:length(tmp_Ds_2_4)), tmp_Ds_2_4,main = "DT_Ans_A1\nD[1][4]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")

# png("C[4][1].png", width = 1344, height = 914 )
# plot(c(1:length(tmp_Ds_3_1)), tmp_Ds_3_1,main = "DT_Ans_A1\nD[2][1]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")

# png("C[4][1].png", width = 1344, height = 914 )
# plot(c(1:length(tmp_Ds_3_2)), tmp_Ds_3_2,main = "DT_Ans_A1\nD[2][2]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")

# png("C[4][1].png", width = 1344, height = 914 )
# plot(c(1:length(tmp_Ds_4_1)), tmp_Ds_4_1,main = "DT_Ans_A1\nD[3][1]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")

# plot(c(1:length(tmp_dDs_2_1)), tmp_dDs_2_1,main = "DT_Ans_A1\nDnoise_D[1][1]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")
# plot(c(1:length(tmp_dDs_2_2)), tmp_dDs_2_2,main = "DT_Ans_A1\nDnoise_D[1][2]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")
# plot(c(1:length(tmp_dDs_2_3)), tmp_dDs_2_3,main = "DT_Ans_A1\nDnoise_D[1][3]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")
# plot(c(1:length(tmp_dDs_2_4)), tmp_dDs_2_4,main = "DT_Ans_A1\nDnoise_D[1][4]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")
# plot(c(1:length(tmp_dDs_3_1)), tmp_dDs_3_1,main = "DT_Ans_A1\nDnoise_D[2][1]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")
# plot(c(1:length(tmp_dDs_3_2)), tmp_dDs_3_2,main = "DT_Ans_A1\nDnoise_D[2][2]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")
# plot(c(1:length(tmp_dDs_4_1)), tmp_dDs_4_1,main = "DT_Ans_A1\nDnoise_D[3][1]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")

