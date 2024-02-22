# Load Hal wavelet estimation module
WSE_Path = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/src/waveletShrinkageEstimation.R")
source(WSE_Path)
# Load wavelet shrin module
WaveletTransform_Path = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/src/waveletTransform.R")
source(WaveletTransform_Path)
# Load data conversion module
DT_Path = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/src/dataTransform.R")
source(DT_Path)
# Load Threshold Module
Threshold_Path = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/src/threshold.R")
source(Threshold_Path)

periodicBasedPrediction = function(data, dt, thresholdName, thresholdMode, index, initThresholdvalue, predictionPercentage, term){
  term = length(data)
  data = wse(data = data, dt = dt, thresholdName = thresholdName, thresholdMode = thresholdMode , index = index , initThresholdvalue = initThresholdvalue)
  # set the prediction term
  predictionTerm = floor((1 - predictionPercentage) * term)

  # Set the number of CPU cores to use
  num_cores = detectCores()
  cl = makeCluster(num_cores)
  registerDoParallel(cl)

  # definition of data
  Cs = data$cs
  Ds = data$ds

  coeLength = length(Cs)

  tmp_Cs_4_1 = list()

  tmp_Ds_2_1 = list()
  tmp_Ds_2_2 = list()
  tmp_Ds_2_3 = list()
  tmp_Ds_2_4 = list()
  tmp_Ds_3_1 = list()
  tmp_Ds_3_2 = list()
  tmp_Ds_4_1 = list()

  coe = list()
  # coe_name_list
  coe_name = list("C[4][1]","D[1][1]","D[1][2]","D[1][3]","D[1][4]","D[2][1]","D[2][2]","D[3][1]")

  for(j in seq(1, length(Ds) - predictionTerm, by=1)){
  tmp_Cs_4_1 = c(tmp_Cs_4_1,Cs[[j]][[4]][1])

  tmp_Ds_2_1 = c(tmp_Ds_2_1,Ds[[j]][[2]][1])
  tmp_Ds_2_2 = c(tmp_Ds_2_2,Ds[[j]][[2]][2])
  tmp_Ds_2_3 = c(tmp_Ds_2_3,Ds[[j]][[2]][3])
  tmp_Ds_2_4 = c(tmp_Ds_2_4,Ds[[j]][[2]][4])
  tmp_Ds_3_1 = c(tmp_Ds_3_1,Ds[[j]][[3]][1])
  tmp_Ds_3_2 = c(tmp_Ds_3_2,Ds[[j]][[3]][2])
  tmp_Ds_4_1 = c(tmp_Ds_4_1,Ds[[j]][[4]][1])
  }

  # coe_list
  coe = list(tmp_Cs_4_1,tmp_Ds_2_1,tmp_Ds_2_2,tmp_Ds_2_3,tmp_Ds_2_4,tmp_Ds_3_1,tmp_Ds_3_2,tmp_Ds_4_1)

    # regression function
    f = function(x, a, b, c, d) {
    (a * sin((b * x) + c)) + d
    }

  # cal coe in regression function
  run_regression = function(j) {
      x = c(1:(coeLength - predictionTerm))
      a_data = data.frame(mse = numeric(), a = numeric(), b = numeric(), c = numeric(), d = numeric())
      coe = list(tmp_Cs_4_1,tmp_Ds_2_1,tmp_Ds_2_2,tmp_Ds_2_3,tmp_Ds_2_4,tmp_Ds_3_1,tmp_Ds_3_2,tmp_Ds_4_1)
      tmp_coe = unlist(coe[[j]])
      for(sub_a in seq(0.5, 1, by = 0.5)){
          for(sub_b in seq(0.5, 1, by = 0.5)){
              for(sub_c in seq(0.5, 1, by = 0.5)){
                  for(sub_d in seq(0, 1, by = 0.5)){
                      fit = nls(tmp_coe ~ f(x, a, b, c, d), start = list(a =  sub_a, b = sub_b, c = sub_c, d = sub_d),control=nls.control(warnOnly=TRUE))
                      params = coef(fit)
                      pre = f(x, params[1], params[2], params[3], params[4])
                      mse = mean((unlist(coe[[j]]) - pre)^2)
                      add_data = data.frame(mse = mse, a = params[1], b = params[2], c = params[3], d = params[4])
                      a_data = rbind(a_data, add_data)
                  }
              }
          }
      }
    row.names(a_data) = NULL
    a_data = a_data[order(a_data$mse, decreasing = F),]
    return(a_data)
  }

  # start cal execution time
  tic()
  # Use foreach for parallel processing easyliy
  sorted_best_coe = foreach(j = seq(1, 8, by = 1)) %dopar% run_regression(j)
  # stop parallel processing
  stopCluster(cl)
  # stop cal execution time
  time = toc()

  y = c(1:coeLength)
  C_4_1 = f(y,sorted_best_coe[[1]]$a[[1]],sorted_best_coe[[1]]$b[[1]],sorted_best_coe[[1]]$c[[1]],sorted_best_coe[[1]]$d[[1]])
  D_1_1 = f(y,sorted_best_coe[[2]]$a[[1]],sorted_best_coe[[2]]$b[[1]],sorted_best_coe[[2]]$c[[1]],sorted_best_coe[[2]]$d[[1]])
  D_1_2 = f(y,sorted_best_coe[[3]]$a[[1]],sorted_best_coe[[3]]$b[[1]],sorted_best_coe[[3]]$c[[1]],sorted_best_coe[[3]]$d[[1]])
  D_1_3 = f(y,sorted_best_coe[[4]]$a[[1]],sorted_best_coe[[4]]$b[[1]],sorted_best_coe[[4]]$c[[1]],sorted_best_coe[[4]]$d[[1]])
  D_1_4 = f(y,sorted_best_coe[[5]]$a[[1]],sorted_best_coe[[5]]$b[[1]],sorted_best_coe[[5]]$c[[1]],sorted_best_coe[[5]]$d[[1]])
  D_2_1 = f(y,sorted_best_coe[[6]]$a[[1]],sorted_best_coe[[6]]$b[[1]],sorted_best_coe[[6]]$c[[1]],sorted_best_coe[[6]]$d[[1]])
  D_2_2 = f(y,sorted_best_coe[[7]]$a[[1]],sorted_best_coe[[7]]$b[[1]],sorted_best_coe[[7]]$c[[1]],sorted_best_coe[[7]]$d[[1]])
  D_3_1 = f(y,sorted_best_coe[[8]]$a[[1]],sorted_best_coe[[8]]$b[[1]],sorted_best_coe[[8]]$c[[1]],sorted_best_coe[[8]]$d[[1]])

  for(k in seq(coeLength - predictionTerm + 1, coeLength, by = 1)){
      Cs[[k]][[4]][1] = C_4_1[k]

      Ds[[k]][[2]][1] = D_1_1[[k]]
      Ds[[k]][[2]][2] = D_1_2[[k]]
      Ds[[k]][[2]][3] = D_1_3[[k]]
      Ds[[k]][[2]][4] = D_1_4[[k]]
      Ds[[k]][[3]][1] = D_2_1[[k]]
      Ds[[k]][[3]][2] = D_2_2[[k]]
      Ds[[k]][[4]][1] = D_3_1[[k]]
  }


  denoiseDs = ThresholdForGroups(Ds = Ds, thresholdMode = thresholdMode ,thresholdName = thresholdName ,dt = dt,groups=0 ,initThresholdvalue = 1)

  i_groups = inverseHaarWaveletTransformForGroups(Cs,denoiseDs)
  i_groups = lapply(i_groups, function(x) x*8**0.5)

  allData = movingAverage(i_groups,term)

  if(dt == "A1"){
    # Perform inverse Anscombe data conversion
    allData = inverseAnscombeTransformFromGroup(allData,var);
  } else if(dt == "A2"){
    # Perform inverse Anscombe data conversion
    allData = inverseAnscombeTransform2FromGroup(allData,var);
  } else if(dt == "A3"){
    # Perform inverse Anscombe data conversion
    allData = inverseAnscombeTransform3FromGroup(allData,var);
  } else if(dt == "B1"){
    # Perform inverse Anscombe data conversion
    allData = inverseBartlettTransformFromGroup(allData,var);
  } else if(dt == "B2"){
    # Perform inverse Anscombe data conversion
    allData = inverseBartlettTransform2FromGroup(allData,var);
  } else if (dt == "Fr") {
    allData = inverseFreemanTransformFromGroup(allData,var)
  } else{
    allData = allData
  }
  best_coe = data.frame(a = numeric(), b = numeric(), c = numeric(), d = numeric())
  for(m in seq(1, 8, by = 1)){
      tmp_best_coe = data.frame(a = sorted_best_coe[[m]]$a[[1]],b = sorted_best_coe[[m]]$b[[1]],c = sorted_best_coe[[m]]$c[[1]],d = sorted_best_coe[[m]]$d[[1]])
      best_coe = rbind(best_coe, tmp_best_coe)
  }
  predictionData = list(predictionData=tail(allData,predictionTerm), regressionCoefficient = best_coe)
  return(predictionData)
}

quatraticBasedPrediction = function(data, dt, thresholdName, thresholdMode, index, initThresholdvalue, predictionPercentage, term){
  term = length(data)
  data = wse(data = data, dt = "none", thresholdName = "ldt", thresholdMode = "h", index = 3, initThresholdvalue = 1)
  # set the prediction term
  predictionTerm = floor((1 - predictionPercentage) * term)

  # Set the number of CPU cores to use
  num_cores = detectCores()
  cl = makeCluster(num_cores)
  registerDoParallel(cl)

  # definition of data
  Cs = data$cs
  dDs = data$denoiseDs

  coe_length = length(Cs)

  tmp_Cs_4_1 = list()

  tmp_dDs_2_1 = list()
  tmp_dDs_2_2 = list()
  tmp_dDs_2_3 = list()
  tmp_dDs_2_4 = list()
  tmp_dDs_3_1 = list()
  tmp_dDs_3_2 = list()
  tmp_dDs_4_1 = list()

  # coe_name_list
  coe_name = list("C[4][1]","Donise_D[1][1]","Donise_D[1][2]","Donise_D[1][3]","Donise_D[1][4]","Donise_D[2][1]","Donise_D[2][2]","Donise_D[3][1]")

  tmp_num = (term - predictionTerm) %/% 8
  for(j in seq(1, (tmp_num-1) * 8 + 1, by=8)){
      tmp_Cs_4_1 = c(tmp_Cs_4_1,Cs[[j]][[4]][1])

      tmp_dDs_2_1 = c(tmp_dDs_2_1,dDs[[j]][[2]][1])
      tmp_dDs_2_2 = c(tmp_dDs_2_2,dDs[[j]][[2]][2])
      tmp_dDs_2_3 = c(tmp_dDs_2_3,dDs[[j]][[2]][3])
      tmp_dDs_2_4 = c(tmp_dDs_2_4,dDs[[j]][[2]][4])
      tmp_dDs_3_1 = c(tmp_dDs_3_1,dDs[[j]][[3]][1])
      tmp_dDs_3_2 = c(tmp_dDs_3_2,dDs[[j]][[3]][2])
      tmp_dDs_4_1 = c(tmp_dDs_4_1,dDs[[j]][[4]][1])
  }

  # coe_list
  coe = list(tmp_Cs_4_1,tmp_dDs_2_1,tmp_dDs_2_2,tmp_dDs_2_3,tmp_dDs_2_4,tmp_dDs_3_1,tmp_dDs_3_2,tmp_dDs_4_1)

  # regression function
  f = function(x, a, b, c) {
      a * x^2 + b * x + c
  }

  # cal coe in regression function
  run_regression = function(i) {
  x = c(1:tmp_num)
  tmp_coe = unlist(coe[[i]])
  a_data = data.frame(mse = numeric(), a = numeric(), b = numeric(), c = numeric())
  if(all(sapply(coe[[i]], function(x) x == 0)) == TRUE){
      a_data = data.frame(mse = 0, a = 0, b = 0, c = 0)
  }
  else{
      for(sub_a in seq(0, 10, by = 0.5)){
          for(sub_b in seq(0, 10, by = 0.5)){
              for(sub_c in seq(0, 10, by = 0.5)){
                  fit = nls(tmp_coe ~ f(x, a, b, c), start = list(a =  sub_a, b = sub_b, c = sub_c), control=nls.control(warnOnly=TRUE))
                  params = coef(fit)
                  pre = f(x, params[1], params[2], params[3])
                  mse = mean((unlist(coe[[i]]) - pre)^2)
                  add_data = data.frame(mse = mse, a = params[1], b = params[2], c = params[3])
                  a_data = rbind(a_data, add_data)
              }
          }
      }
      row.names(a_data) = NULL
      a_data = a_data[order(a_data$mse, decreasing = F),]
  }
  return(a_data)
  }

  # start cal execution time
  tic()
  # Use foreach for parallel processing easyliy
  sorted_best_coe = foreach(i = seq(1, 8, by = 1)) %dopar% run_regression(i)
  # stop parallel processing
  stopCluster(cl)
  # stop cal execution time
  time = toc()

  y = c(1:(term %/%8 + 1))
  C_4_1 = f(y,sorted_best_coe[[1]]$a[[1]],sorted_best_coe[[1]]$b[[1]],sorted_best_coe[[1]]$c[[1]])
  D_1_1 = f(y,sorted_best_coe[[2]]$a[[1]],sorted_best_coe[[2]]$b[[1]],sorted_best_coe[[2]]$c[[1]])
  D_1_2 = f(y,sorted_best_coe[[3]]$a[[1]],sorted_best_coe[[3]]$b[[1]],sorted_best_coe[[3]]$c[[1]])
  D_1_3 = f(y,sorted_best_coe[[4]]$a[[1]],sorted_best_coe[[4]]$b[[1]],sorted_best_coe[[4]]$c[[1]])
  D_1_4 = f(y,sorted_best_coe[[5]]$a[[1]],sorted_best_coe[[5]]$b[[1]],sorted_best_coe[[5]]$c[[1]])
  D_2_1 = f(y,sorted_best_coe[[6]]$a[[1]],sorted_best_coe[[6]]$b[[1]],sorted_best_coe[[6]]$c[[1]])
  D_2_2 = f(y,sorted_best_coe[[7]]$a[[1]],sorted_best_coe[[7]]$b[[1]],sorted_best_coe[[7]]$c[[1]])
  D_3_1 = f(y,sorted_best_coe[[8]]$a[[1]],sorted_best_coe[[8]]$b[[1]],sorted_best_coe[[8]]$c[[1]])

  for(k in seq(1, term%/%8+1, by = 1)){
    Cs[[k]][[4]][1] = C_4_1[k]

    dDs[[k]][[2]][1] = D_1_1[[k]]
    dDs[[k]][[2]][2] = D_1_2[[k]]
    dDs[[k]][[2]][3] = D_1_3[[k]]
    dDs[[k]][[2]][4] = D_1_4[[k]]
    dDs[[k]][[3]][1] = D_2_1[[k]]
    dDs[[k]][[3]][2] = D_2_2[[k]]
    dDs[[k]][[4]][1] = D_3_1[[k]]
  }
  i_groups = inverseHaarWaveletTransformForGroups(Cs,dDs)
  i_groups = lapply(i_groups, function(x) x*8**0.5)

  allData = movingAverage(i_groups,term)
  if(dt == "A1"){
    # Perform inverse Anscombe data conversion
    allData = inverseAnscombeTransformFromGroup(allData,var);
  } else if(dt == "A2"){
    # Perform inverse Anscombe data conversion
    allData = inverseAnscombeTransform2FromGroup(allData,var);
  } else if(dt == "A3"){
    # Perform inverse Anscombe data conversion
    allData = inverseAnscombeTransform3FromGroup(allData,var);
  } else if(dt == "B1"){
    # Perform inverse Anscombe data conversion
    allData = inverseBartlettTransformFromGroup(allData,var);
  } else if(dt == "B2"){
    # Perform inverse Anscombe data conversion
    allData = inverseBartlettTransform2FromGroup(allData,var);
  } else if (dt == "Fr") {
    allData = inverseFreemanTransformFromGroup(allData,var)
  } else{
    allData = allData
  }
  best_coe = data.frame(a = numeric(), b = numeric(), c = numeric())
  for(m in seq(1, 8, by = 1)){
      tmp_best_coe = data.frame(a = sorted_best_coe[[m]]$a[[1]],b = sorted_best_coe[[m]]$b[[1]],c = sorted_best_coe[[m]]$c[[1]])
      best_coe = rbind(best_coe, tmp_best_coe)
  }
  predictionData = list(predictionData=tail(allData,predictionTerm), regressionCoefficient = best_coe)
  return(predictionData)
}