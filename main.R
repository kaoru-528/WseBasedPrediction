# Load data set
dataPath1 = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/DS/NDT_WSE/NDT_J=3.RData")
load(dataPath1)

dataPath2 = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/DS/DT_Ans_WSE/A1/Ans_A1_J=3.RData")
load(dataPath2)

# Load creating graph module
createGraph_Path = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/createGraph.R")
source(createGraph_Path)

# name = "NDT_soft"
# createGraph(soft, name)
# name = "NDT_hard"
# createGraph(hard, name)
name = "Ans_A1_ut_hard"
createGraph(ut_hard, name)
# name = "Ans_A1_ut_soft"
# createGraph(ut_soft, name)
