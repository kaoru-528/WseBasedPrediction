

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
        if((name == "NDT_soft" || name == "NDT_soft") && j >= 9){
            break
        }
        title = paste0(name,"\n",coe_name[[j]])
        filename = paste0("OUTPUT/",name,"_",coe_name[[j]],".png")
        png(filename, width = 1344, height = 914 )
        plot(c(1:length(coe[[j]])), coe[[j]],main = title, xlab = "number", ylab = "C", pch = 16, col = "blue", type = "b")
        dev.off()
    }
}