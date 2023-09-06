regression = function(data, name){

    # definition of data
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
    # coe_name_list
    coe_name = list("C[4][1]","D[1][1]","D[1][2]","D[1][3]","D[1][4]","D[2][1]","D[2][2]","D[3][1]","Donise_D[1][1]","Donise_D[1][2]","Donise_D[1][3]","Donise_D[1][4]","Donise_D[2][1]","Donise_D[2][2]","Donise_D[3][1]")

# assignment for sub-data
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
# coe_list
coe = list(tmp_Cs_4_1,tmp_Ds_2_1,tmp_Ds_2_2,tmp_Ds_2_3,tmp_Ds_2_4,tmp_Ds_3_1,tmp_Ds_3_2,tmp_Ds_4_1,tmp_dDs_2_1,tmp_dDs_2_2,tmp_dDs_2_3,tmp_dDs_2_4,tmp_dDs_3_1,tmp_dDs_3_2,tmp_dDs_4_1)

# regression function
x = c(1:55)
f <- function(x, a, b, c, d) {
    (a * sin((b * x) + c)) + d
}

sort_data = list()

for(j in seq(1, 8, by = 1)){
    data = data.frame(mse = numeric(), a = numeric(), b = numeric(), c = numeric(), d = numeric())
    for(sub_a in seq(5, 10, by = 1)){
        for(sub_b in seq(5, 10, by = 1)){
            for(sub_c in seq(5, 10, by = 1)){
                for(sub_d in seq(5, 10, by = 1)){
                    fit <- nls(unlist(coe[[j]]) ~ f(x, a, b, c, d), start = list(a =  sub_a, b = sub_b, c = sub_c, d = sub_d),control=nls.control(warnOnly=TRUE))
                    params = coef(fit)
                    pre = f(x, params[1], params[2], params[3] ,params[4])
                    mse = mean((unlist(coe[[j]]) - pre)^2)
                    add_data = data.frame(mse = mse, a = params[1], b = params[2], c = params[3], d = params[4])
                    data = rbind(data, add_data)
                }
            }
        }
    }
    row.names(data) = NULL
    sort_data[[j]] = data[order(data$mse, decreasing = F),]
    print(j)
    rm(data)
    rm(add_data)
}
return(sort_data)
}
