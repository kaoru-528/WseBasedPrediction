# Load data set
dataPath = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/DS/NDT_WSE/16-58-32_var_J=3.RData")
load(dataPath)
Cs = hard$Cs
Ds = hard$Ds
dDs = hard$Denoise_Ds

tmp_Cs = list()
tmp_Ds = list()
for(i in seq(1, 62, by=8)){
    print(i)
    tmp_Cs = c(tmp_Cs,Cs[i])
    tmp_Ds = c(tmp_Ds,Ds[i])
}

plot(x = c(1:i),y = tmp_Cs[[1]][[1]])