createGraph = function(data, name){

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
    # 係数の名前リスト
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
# 係数リスト
coe = list(tmp_Cs_4_1,tmp_Ds_2_1,tmp_Ds_2_2,tmp_Ds_2_3,tmp_Ds_2_4,tmp_Ds_3_1,tmp_Ds_3_2,tmp_Ds_4_1,tmp_dDs_2_1,tmp_dDs_2_2,tmp_dDs_2_3,tmp_dDs_2_4,tmp_dDs_3_1,tmp_dDs_3_2,tmp_dDs_4_1)

# 回帰関数
x = c(1:55)
f <- function(x, a, b, c, d) {
    (a * sin((b * x) + c)) + d
}

# # testing 
# j = 8
# title = paste0(name,"\n",coe_name[[j]])
# filename = paste0("OUTPUT/",name,"_",coe_name[[j]],".png")
# png(filename, width = 1344, height = 914 )
# fit <- nls(unlist(coe[[j]]) ~ f(x, a, b, c, d), start = list(a =  3.1, b = 1.6, c = 0.6, d = 1),control=nls.control(warnOnly=TRUE))
# params <- coef(fit)
# # print(params)
# pre = f(x, params[1], params[2], params[3] ,params[4])
# ans = mean((unlist(coe[[j]]) - pre)^2)
# print(ans)
# # plot(x, coe[[j]],main = title, xlab = "number", ylab = "C", pch = 16, col = "blue", type = "b")
# # lines(x, f(x, params[1], params[2], params[3] ,params[4]), col = "red")
# # dev.off()

ans = list()

# C[4][1]
j = 1
for(sub_a in seq(1, 5, by = 1)){
    for(sub_b in seq(1, 3, by = 1)){
        for(sub_c in seq(-1, 1, by = 0.1)){
            for(sub_d in seq(5, 10, by = 1)){
                # title = paste0(name,"\n",coe_name[[j]])
                # filename = paste0("OUTPUT/",sub_a,"_",sub_b,"_",sub_c,"_",sub_d,".png")
                # png(filename, width = 1344, height = 914 )
                fit <- nls(unlist(coe[[j]]) ~ f(x, a, b, c, d), start = list(a =  sub_a, b = sub_b, c = sub_c, d = sub_d),control=nls.control(warnOnly=TRUE))
                params <- coef(fit)
                pre = f(x, params[1], params[2], params[3] ,params[4])
                ans = c(ans, mean((unlist(coe[[j]]) - pre)^2))
                # print(params)
                # plot(x, coe[[j]],main = title, xlab = "number", ylab = "C", pch = 16, col = "blue", type = "b")
                # lines(x, f(x, params[1], params[2], params[3] ,params[4]), col = "red")
                # dev.off()
            }
        }
    }
}

return(min(unlist(ans)))

# j = 2
# for(sub_a in seq(1.5, 2.5, by = 0.1)){
#     for(sub_b in seq(0.5, 1.5, by = 0.1)){
#         for(sub_c in seq(0, 0.5, by = 0.1)){
#             for(sub_d in seq(0, 0.5, by = 0.1)){
#                 title = paste0(name,"\n",coe_name[[j]])
#                 filename = paste0("OUTPUT/",sub_a,"_",sub_b,"_",sub_c,"_",sub_d,".png")
#                 png(filename, width = 1344, height = 914 )
#                 fit <- nls(unlist(coe[[j]]) ~ f(x, a, b, c, d), start = list(a =  sub_a, b = sub_b, c = sub_c, d = sub_d),control=nls.control(warnOnly=TRUE))
#                 params <- coef(fit)
#                 # print(params)
#                 plot(x, coe[[j]],main = title, xlab = "number", ylab = "C", pch = 16, col = "blue", type = "b")
#                 lines(x, f(x, params[1], params[2], params[3] ,params[4]), col = "red")
#                 dev.off()
#             }
#         }
#     }
# }

# j = 3
# for(sub_a in seq(1.5, 2.5, by = 0.1)){
#     for(sub_b in seq(0.5, 1.5, by = 0.1)){
#         for(sub_c in seq(0, 0.5, by = 0.1)){
#             for(sub_d in seq(0, 0.5, by = 0.1)){
#                 title = paste0(name,"\n",coe_name[[j]])
#                 filename = paste0("OUTPUT/",sub_a,"_",sub_b,"_",sub_c,"_",sub_d,".png")
#                 png(filename, width = 1344, height = 914 )
#                 fit <- nls(unlist(coe[[j]]) ~ f(x, a, b, c, d), start = list(a =  sub_a, b = sub_b, c = sub_c, d = sub_d),control=nls.control(warnOnly=TRUE))
#                 params <- coef(fit)
#                 # print(params)
#                 plot(x, coe[[j]],main = title, xlab = "number", ylab = "C", pch = 16, col = "blue", type = "b")
#                 lines(x, f(x, params[1], params[2], params[3] ,params[4]), col = "red")
#                 dev.off()
#             }
#         }
#     }
# }

# j = 4
# for(sub_a in seq(1.5, 2.5, by = 0.1)){
#     for(sub_b in seq(0.5, 1.5, by = 0.1)){
#         for(sub_c in seq(0, 0.5, by = 0.1)){
#             for(sub_d in seq(0, 0.5, by = 0.1)){
#                 title = paste0(name,"\n",coe_name[[j]])
#                 filename = paste0("OUTPUT/",sub_a,"_",sub_b,"_",sub_c,"_",sub_d,".png")
#                 png(filename, width = 1344, height = 914 )
#                 fit <- nls(unlist(coe[[j]]) ~ f(x, a, b, c, d), start = list(a =  sub_a, b = sub_b, c = sub_c, d = sub_d),control=nls.control(warnOnly=TRUE))
#                 params <- coef(fit)
#                 # print(params)
#                 plot(x, coe[[j]],main = title, xlab = "number", ylab = "C", pch = 16, col = "blue", type = "b")
#                 lines(x, f(x, params[1], params[2], params[3] ,params[4]), col = "red")
#                 dev.off()
#             }
#         }
#     }
# }

# j = 5
# for(sub_a in seq(1.5, 2.5, by = 0.1)){
#     for(sub_b in seq(0.5, 1.5, by = 0.1)){
#         for(sub_c in seq(0, 0.5, by = 0.1)){
#             for(sub_d in seq(0, 0.5, by = 0.1)){
#                 title = paste0(name,"\n",coe_name[[j]])
#                 filename = paste0("OUTPUT/",sub_a,"_",sub_b,"_",sub_c,"_",sub_d,".png")
#                 png(filename, width = 1344, height = 914 )
#                 fit <- nls(unlist(coe[[j]]) ~ f(x, a, b, c, d), start = list(a =  sub_a, b = sub_b, c = sub_c, d = sub_d),control=nls.control(warnOnly=TRUE))
#                 params <- coef(fit)
#                 # print(params)
#                 plot(x, coe[[j]],main = title, xlab = "number", ylab = "C", pch = 16, col = "blue", type = "b")
#                 lines(x, f(x, params[1], params[2], params[3] ,params[4]), col = "red")
#                 dev.off()
#             }
#         }
#     }
# }

# j = 6
# for(sub_a in seq(1.5, 2.5, by = 0.1)){
#     for(sub_b in seq(0.5, 1.5, by = 0.1)){
#         for(sub_c in seq(0, 0.5, by = 0.1)){
#             for(sub_d in seq(0, 0.5, by = 0.1)){
#                 title = paste0(name,"\n",coe_name[[j]])
#                 filename = paste0("OUTPUT/",sub_a,"_",sub_b,"_",sub_c,"_",sub_d,".png")
#                 png(filename, width = 1344, height = 914 )
#                 fit <- nls(unlist(coe[[j]]) ~ f(x, a, b, c, d), start = list(a =  sub_a, b = sub_b, c = sub_c, d = sub_d),control=nls.control(warnOnly=TRUE))
#                 params <- coef(fit)
#                 # print(params)
#                 plot(x, coe[[j]],main = title, xlab = "number", ylab = "C", pch = 16, col = "blue", type = "b")
#                 lines(x, f(x, params[1], params[2], params[3] ,params[4]), col = "red")
#                 dev.off()
#             }
#         }
#     }
# }

# D[4][1]
# j = 8
# for(sub_a in seq(2.5, 4.5, by = 0.2)){
#     for(sub_b in seq(1, 2, by = 0.1)){
#         for(sub_c in seq(0, 1, by = 0.1)){
#             for(sub_d in seq(-1, 1, by = 0.1)){
#                 title = paste0(name,"\n",coe_name[[j]])
#                 filename = paste0("OUTPUT/",sub_a,"_",sub_b,"_",sub_c,"_",sub_d,".png")
#                 png(filename, width = 1344, height = 914 )
#                 fit <- nls(unlist(coe[[j]]) ~ f(x, a, b, c, d), start = list(a =  sub_a, b = sub_b, c = sub_c, d = sub_d),control=nls.control(warnOnly=TRUE))
#                 params <- coef(fit)
#                 # print(params)
#                 plot(x, coe[[j]],main = title, xlab = "number", ylab = "C", pch = 16, col = "blue", type = "b")
#                 lines(x, f(x, params[1], params[2], params[3] ,params[4]), col = "red")
#                 dev.off()
#             }
#         }
#     }
# }




#     for(j in seq(1, 15, by = 1)){
#         if(name == "NDT_soft" && j >= 9){
#             break
#         }
#         if(name == "NDT_hard" && j >= 9){
#             break
#         }
#         title = paste0(name,"\n",coe_name[[j]])
#         filename = paste0("OUTPUT/",name,"_",coe_name[[j]],".png")
#         png(filename, width = 1344, height = 914 )
#         x = c(1:55)
#         f <- function(x, a, b, c) {
#             # a * sin(b * x) + c * cos(d * x) + e
#             a * sin(b * x) + c
#         }
#         fit <- nls(unlist(coe[[j]]) ~ f(x, a, b, c), start = list(a = 10, b = 10, c = 10),  control=nls.control(maxiter=2000))
#         params <- coef(fit)
#         print(params)
#         plot(x, coe[[j]],main = title, xlab = "number", ylab = "C", pch = 16, col = "blue", type = "b")
#         lines(x, f(x, params[1], params[2], params[3]), col = "red")
#         dev.off()
#     }
}
