# Load data set
dataPath = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/DS/NDT_WSE/NDT_J=3.RData")
load(dataPath)
Cs = ut_hard$Cs
Ds = ut_hard$Ds
dDs = ut_hard$Denoise_Ds

tmp_Cs_4_1 = list()
tmp_Ds_2_1 = list()
tmp_Ds_2_2 = list()
tmp_Ds_2_3 = list()
tmp_Ds_2_4 = list()
tmp_Ds_3_1 = list()
tmp_Ds_3_2 = list()
tmp_Ds_4_1 = list()

tmp_dDs_2_1 = list()
tmp_dDs_2_2 = list()
tmp_dDs_2_3 = list()
tmp_dDs_2_4 = list()
tmp_dDs_3_1 = list()
tmp_dDs_3_2 = list()
tmp_dDs_4_1 = list()

# j = 1
# for(i in seq(1, 62, by=8)){
#     tmp_Cs = c(tmp_Cs,Cs[i])
#     tmp_Cs_1 = c(tmp_Cs_1,tmp_Cs[[j]][[4]])
#     tmp_Ds = c(tmp_Ds,Ds[i])
#     tmp_Ds_1 = c(tmp_Ds_1,tmp_Ds[[j]][[4]][1])

#     j = j + 1 
# }

for(i in seq(1, length(Ds), by=1)){
    
    tmp_Cs_4_1 = c(tmp_Cs_4_1,Cs[[i]][[4]][1])

    tmp_Ds_2_1 = c(tmp_Ds_2_1,Ds[[i]][[2]][1])
    tmp_Ds_2_2 = c(tmp_Ds_2_2,Ds[[i]][[2]][2])
    tmp_Ds_2_3 = c(tmp_Ds_2_3,Ds[[i]][[2]][3])
    tmp_Ds_2_4 = c(tmp_Ds_2_4,Ds[[i]][[2]][4])
    tmp_Ds_3_1 = c(tmp_Ds_3_1,Ds[[i]][[2]][1])
    tmp_Ds_3_2 = c(tmp_Ds_3_2,Ds[[i]][[2]][2])
    tmp_Ds_4_1 = c(tmp_Ds_4_1,Ds[[i]][[2]][1])

#     tmp_dDs_2_1 = c(tmp_dDs_2_1,dDs[[i]][[2]][1])
#     tmp_dDs_2_2 = c(tmp_dDs_2_2,dDs[[i]][[2]][2])
#     tmp_dDs_2_3 = c(tmp_dDs_2_3,dDs[[i]][[2]][3])
#     tmp_dDs_2_4 = c(tmp_dDs_2_4,dDs[[i]][[2]][4])
#     tmp_dDs_3_1 = c(tmp_dDs_3_1,dDs[[i]][[2]][1])
#     tmp_dDs_3_2 = c(tmp_dDs_3_2,dDs[[i]][[2]][2])
#     tmp_dDs_4_1 = c(tmp_dDs_4_1,dDs[[i]][[2]][1])
}

# NDT
plot(c(1:length(tmp_Cs_4_1)), tmp_Cs_4_1,main = "NDT\nC[4][1]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")

plot(c(1:length(tmp_Ds_2_1)), tmp_Ds_2_1,main = "NDT\nD[1][1]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")
plot(c(1:length(tmp_Ds_2_2)), tmp_Ds_2_2,main = "NDT\nD[1][2]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")
plot(c(1:length(tmp_Ds_2_3)), tmp_Ds_2_3,main = "NDT\nD[1][3]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")
plot(c(1:length(tmp_Ds_2_4)), tmp_Ds_2_4,main = "NDT\nD[1][4]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")
plot(c(1:length(tmp_Ds_3_1)), tmp_Ds_3_1,main = "NDT\nD[2][1]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")
plot(c(1:length(tmp_Ds_3_2)), tmp_Ds_3_2,main = "NDT\nD[2][2]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")
plot(c(1:length(tmp_Ds_4_1)), tmp_Ds_4_1,main = "NDT\nD[3][1]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")

# # DT_Ans_A1
# plot(c(1:length(tmp_Cs_4_1)), tmp_Cs_4_1,main = "DT_Ans_A1\nC[4][1]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")

# plot(c(1:length(tmp_Ds_2_1)), tmp_Ds_2_1,main = "DT_Ans_A1\nD[1][1]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")
# plot(c(1:length(tmp_Ds_2_2)), tmp_Ds_2_2,main = "DT_Ans_A1\nD[1][2]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")
# plot(c(1:length(tmp_Ds_2_3)), tmp_Ds_2_3,main = "DT_Ans_A1\nD[1][3]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")
# plot(c(1:length(tmp_Ds_2_4)), tmp_Ds_2_4,main = "DT_Ans_A1\nD[1][4]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")
# plot(c(1:length(tmp_Ds_3_1)), tmp_Ds_3_1,main = "DT_Ans_A1\nD[2][1]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")
# plot(c(1:length(tmp_Ds_3_2)), tmp_Ds_3_2,main = "DT_Ans_A1\nD[2][2]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")
# plot(c(1:length(tmp_Ds_4_1)), tmp_Ds_4_1,main = "DT_Ans_A1\nD[3][1]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")

# plot(c(1:length(tmp_dDs_2_1)), tmp_dDs_2_1,main = "DT_Ans_A1\nDnoise_D[1][1]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")
# plot(c(1:length(tmp_dDs_2_2)), tmp_dDs_2_2,main = "DT_Ans_A1\nDnoise_D[1][2]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")
# plot(c(1:length(tmp_dDs_2_3)), tmp_dDs_2_3,main = "DT_Ans_A1\nDnoise_D[1][3]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")
# plot(c(1:length(tmp_dDs_2_4)), tmp_dDs_2_4,main = "DT_Ans_A1\nDnoise_D[1][4]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")
# plot(c(1:length(tmp_dDs_3_1)), tmp_dDs_3_1,main = "DT_Ans_A1\nDnoise_D[2][1]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")
# plot(c(1:length(tmp_dDs_3_2)), tmp_dDs_3_2,main = "DT_Ans_A1\nDnoise_D[2][2]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")
# plot(c(1:length(tmp_dDs_4_1)), tmp_dDs_4_1,main = "DT_Ans_A1\nDnoise_D[3][1]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue", type = "b")