# Load data set
dataPath = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/DS/DT_Ans_WSE/A1/Ans_A1_J=3.RData")
load(dataPath)

# Cs = hard$Cs
# Ds = hard$Ds
# dDs = hard$Denoise_Ds
createGraph = function(data, name){
    Cs = data$Cs
    Ds = data$Ds
    dDs = data$Denoise_Ds

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

    coe = list()
    coe_name = list("C[4][1]","D[1][1]","D[1][2]","D[1][3]","D[1][4]","D[2][1]","D[2][2]","D[3][1]","Donise_D[1][1]","Donise_D[1][2]","Donise_D[1][3]","Donise_D[1][4]","Donise_D[2][1]","Donise_D[2][2]","Donise_D[3][1]")

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
        tmp_Ds_3_1 = c(tmp_Ds_3_1,Ds[[i]][[3]][1])
        tmp_Ds_3_2 = c(tmp_Ds_3_2,Ds[[i]][[3]][2])
        tmp_Ds_4_1 = c(tmp_Ds_4_1,Ds[[i]][[4]][1])

        tmp_dDs_2_1 = c(tmp_dDs_2_1,dDs[[i]][[2]][1])
        tmp_dDs_2_2 = c(tmp_dDs_2_2,dDs[[i]][[2]][2])
        tmp_dDs_2_3 = c(tmp_dDs_2_3,dDs[[i]][[2]][3])
        tmp_dDs_2_4 = c(tmp_dDs_2_4,dDs[[i]][[2]][4])
        tmp_dDs_3_1 = c(tmp_dDs_3_1,dDs[[i]][[3]][1])
        tmp_dDs_3_2 = c(tmp_dDs_3_2,dDs[[i]][[3]][2])
        tmp_dDs_4_1 = c(tmp_dDs_4_1,dDs[[i]][[4]][1])
    }
    coe = list(tmp_Cs_4_1,tmp_Ds_2_1,tmp_Ds_2_2,tmp_Ds_2_3,tmp_Ds_2_4,tmp_Ds_3_1,tmp_Ds_3_2,tmp_Ds_4_1,tmp_dDs_2_1,tmp_dDs_2_2,tmp_dDs_2_3,tmp_dDs_2_4,tmp_dDs_3_1,tmp_dDs_3_2,tmp_dDs_4_1)


    # "Ans_A1_ut_hard
    for(j in seq(1, 15, by = 1)){
        title = paste0(name,"\n",coe_name[[j]])
        filename = paste0("OUTPUT/",name,"_",coe_name[[j]],".png")
        png(filename, width = 1344, height = 914 )
        plot(c(1:length(coe[[j]])), coe[[j]],main = title, xlab = "number", ylab = "C", pch = 16, col = "blue", type = "b")
        dev.off()

    }
}


name = "NDT_soft"
createGraph(soft, name)
# name = "Ans_A1_ut_hard"
# createGraph(ut_hard, name)
# name = "Ans_A1_ut_soft"
# createGraph(ut_soft, name)


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