# Load data set
dataPath = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/DS/NDT_WSE/16-58-32_var_J=3.RData")
load(dataPath)
Cs = hard$Cs
Ds = hard$Ds
dDs = hard$Denoise_Ds

tmp_Cs = list()
tmp_Cs_1 = list()
tmp_Ds_1 = list()
tmp_Ds = list()
j = 1
for(i in seq(1, 62, by=8)){
    tmp_Cs = c(tmp_Cs,Cs[i])
    tmp_Cs_1 = c(tmp_Cs_1,tmp_Cs[[j]][[4]])
    tmp_Ds = c(tmp_Ds,Ds[i])
    tmp_Ds_1 = c(tmp_Ds_1,tmp_Ds[[j]][[4]])

    j = j + 1 
}

# plot(c(1:length(tmp_Cs_1)), tmp_Cs_1,main = "C[1][4]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue")
plot(c(1:length(tmp_Ds_1)), tmp_Ds_1,main = "D[1][2]", xlab = "number", ylab = "C_coe", pch = 16, col = "blue")

