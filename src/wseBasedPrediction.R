WaveletShrinkageEstimation_Path = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/src/WaveletShrinkageEstimation.R")
source(WaveletShrinkageEstimation_Path)
Regression_Path = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/src/RegressionModule.R")
source(Regression_Path)

PeriodicBasedPrediction <- function(data, dt, thresholdName, thresholdMode, index, initThresholdvalue, predictionPercentage) {
  term <- length(data)
  data <- wse(data = data, dt = dt, thresholdName = thresholdName, thresholdMode = thresholdMode, index = index, initThresholdvalue = initThresholdvalue)
  predictionTerm <- floor((1 - predictionPercentage) * term)
  Cs <- data$cs
  Ds <- data$ds
  coeLength <- length(Cs)
  coe <- prepare_data(data$cs, data$ds, predictionTerm)

  tic()
  sorted_best_coe <- run_parallel_regression(coe, coeLength, predictionTerm, "periodic")
  time <- toc()

  y <- c(1:coeLength)
  C_4_1 <- periodic_function(y, sorted_best_coe[[1]]$a[[1]], sorted_best_coe[[1]]$b[[1]], sorted_best_coe[[1]]$c[[1]], sorted_best_coe[[1]]$d[[1]])
  D_1_1 <- periodic_function(y, sorted_best_coe[[2]]$a[[1]], sorted_best_coe[[2]]$b[[1]], sorted_best_coe[[2]]$c[[1]], sorted_best_coe[[2]]$d[[1]])
  D_1_2 <- periodic_function(y, sorted_best_coe[[3]]$a[[1]], sorted_best_coe[[3]]$b[[1]], sorted_best_coe[[3]]$c[[1]], sorted_best_coe[[3]]$d[[1]])
  D_1_3 <- periodic_function(y, sorted_best_coe[[4]]$a[[1]], sorted_best_coe[[4]]$b[[1]], sorted_best_coe[[4]]$c[[1]], sorted_best_coe[[4]]$d[[1]])
  D_1_4 <- periodic_function(y, sorted_best_coe[[5]]$a[[1]], sorted_best_coe[[5]]$b[[1]], sorted_best_coe[[5]]$c[[1]], sorted_best_coe[[5]]$d[[1]])
  D_2_1 <- periodic_function(y, sorted_best_coe[[6]]$a[[1]], sorted_best_coe[[6]]$b[[1]], sorted_best_coe[[6]]$c[[1]], sorted_best_coe[[6]]$d[[1]])
  D_2_2 <- periodic_function(y, sorted_best_coe[[7]]$a[[1]], sorted_best_coe[[7]]$b[[1]], sorted_best_coe[[7]]$c[[1]], sorted_best_coe[[7]]$d[[1]])
  D_3_1 <- periodic_function(y, sorted_best_coe[[8]]$a[[1]], sorted_best_coe[[8]]$b[[1]], sorted_best_coe[[8]]$c[[1]], sorted_best_coe[[8]]$d[[1]])

  for (k in seq(coeLength - predictionTerm + 1, coeLength, by = 1)) {
    Cs[[k]][[4]][1] <- C_4_1[k]

    Ds[[k]][[2]][1] <- D_1_1[[k]]
    Ds[[k]][[2]][2] <- D_1_2[[k]]
    Ds[[k]][[2]][3] <- D_1_3[[k]]
    Ds[[k]][[2]][4] <- D_1_4[[k]]
    Ds[[k]][[3]][1] <- D_2_1[[k]]
    Ds[[k]][[3]][2] <- D_2_2[[k]]
    Ds[[k]][[4]][1] <- D_3_1[[k]]
  }


  denoiseDs <- ThresholdForGroups(Ds = Ds, thresholdMode = thresholdMode, thresholdName = thresholdName, dt = dt, groups = 0, initThresholdvalue = 1)
  i_groups <- inverseHaarWaveletTransformForGroups(Cs, denoiseDs)
  i_groups <- lapply(i_groups, function(x) x * 8**0.5)
  allData <- movingAverage(i_groups, term)
  allData <- apply_inverse_transform(allData, dt)

  best_coe <- data.frame(a = numeric(), b = numeric(), c = numeric(), d = numeric())
  for (m in seq(1, 8, by = 1)) {
    tmp_best_coe <- data.frame(a = sorted_best_coe[[m]]$a[[1]], b = sorted_best_coe[[m]]$b[[1]], c = sorted_best_coe[[m]]$c[[1]], d = sorted_best_coe[[m]]$d[[1]])
    best_coe <- rbind(best_coe, tmp_best_coe)
  }
  predictionData <- list(predictionData = tail(allData, predictionTerm), regressionCoefficient = best_coe)
  return(predictionData)
}

ArimaBasedPrediction <- function(data, dt, thresholdName, thresholdMode, index, initThresholdvalue, predictionPercentage) {
  term <- length(data)
  data <- wse(data = data, dt = dt, thresholdName = thresholdName, thresholdMode = thresholdMode, index = index, initThresholdvalue = initThresholdvalue)
  predictionTerm <- floor((1 - predictionPercentage) * term)
  Cs <- data$cs
  Ds <- data$ds
  coeLength <- length(Cs)
  coe <- prepare_data(data$cs, data$ds, predictionTerm)

  tic()
  prediction_result <- run_parallel_arima_regression(coe, coeLength, predictionTerm)
  time <- toc()

  y <- c(1:coeLength)
  C_4_1 <- c(unlist(coe[[1]]), prediction_result[[1]]$mean)
  D_1_1 <- c(unlist(coe[[2]]), prediction_result[[2]]$mean)
  D_1_2 <- c(unlist(coe[[3]]), prediction_result[[3]]$mean)
  D_1_3 <- c(unlist(coe[[4]]), prediction_result[[4]]$mean)
  D_1_4 <- c(unlist(coe[[5]]), prediction_result[[5]]$mean)
  D_2_1 <- c(unlist(coe[[6]]), prediction_result[[6]]$mean)
  D_2_2 <- c(unlist(coe[[7]]), prediction_result[[7]]$mean)
  D_3_1 <- c(unlist(coe[[8]]), prediction_result[[8]]$mean)

  for (k in seq(coeLength - predictionTerm + 1, coeLength, by = 1)) {
    Cs[[k]][[4]][1] <- C_4_1[k]

    Ds[[k]][[2]][1] <- D_1_1[[k]]
    Ds[[k]][[2]][2] <- D_1_2[[k]]
    Ds[[k]][[2]][3] <- D_1_3[[k]]
    Ds[[k]][[2]][4] <- D_1_4[[k]]
    Ds[[k]][[3]][1] <- D_2_1[[k]]
    Ds[[k]][[3]][2] <- D_2_2[[k]]
    Ds[[k]][[4]][1] <- D_3_1[[k]]
  }


  denoiseDs <- ThresholdForGroups(Ds = Ds, thresholdMode = thresholdMode, thresholdName = thresholdName, dt = dt, groups = 0, initThresholdvalue = 1)
  i_groups <- inverseHaarWaveletTransformForGroups(Cs, denoiseDs)
  i_groups <- lapply(i_groups, function(x) x * 8**0.5)
  allData <- movingAverage(i_groups, term)
  allData <- apply_inverse_transform(allData, dt)

  PredictionData <- list(predictionData = tail(allData, predictionTerm))
  return(PredictionData)
}

QuatraticBasedPrediction <- function(data, dt, thresholdName, thresholdMode, index, initThresholdvalue, predictionPercentage) {
  term <- length(data)
  data <- wse(data = data, dt = dt, thresholdName = thresholdName, thresholdMode = thresholdMode, index = index, initThresholdvalue = initThresholdvalue)
  predictionTerm <- floor((1 - predictionPercentage) * term)
  Cs <- data$cs
  Ds <- data$ds
  dDs <- data$denoisedDs
  coeLength <- length(Cs)
  coe <- prepare_data(data$cs, data$ds, predictionTerm)

  tic()
  sorted_best_coe <- run_parallel_regression(coe, coeLength, predictionTerm, "quadratic")
  time <- toc()

  y <- c(1:(term %/% 8 + 1))
  C_4_1 <- quadratic_function(y, sorted_best_coe[[1]]$a[[1]], sorted_best_coe[[1]]$b[[1]], sorted_best_coe[[1]]$c[[1]])
  D_1_1 <- quadratic_function(y, sorted_best_coe[[2]]$a[[1]], sorted_best_coe[[2]]$b[[1]], sorted_best_coe[[2]]$c[[1]])
  D_1_2 <- quadratic_function(y, sorted_best_coe[[3]]$a[[1]], sorted_best_coe[[3]]$b[[1]], sorted_best_coe[[3]]$c[[1]])
  D_1_3 <- quadratic_function(y, sorted_best_coe[[4]]$a[[1]], sorted_best_coe[[4]]$b[[1]], sorted_best_coe[[4]]$c[[1]])
  D_1_4 <- quadratic_function(y, sorted_best_coe[[5]]$a[[1]], sorted_best_coe[[5]]$b[[1]], sorted_best_coe[[5]]$c[[1]])
  D_2_1 <- quadratic_function(y, sorted_best_coe[[6]]$a[[1]], sorted_best_coe[[6]]$b[[1]], sorted_best_coe[[6]]$c[[1]])
  D_2_2 <- quadratic_function(y, sorted_best_coe[[7]]$a[[1]], sorted_best_coe[[7]]$b[[1]], sorted_best_coe[[7]]$c[[1]])
  D_3_1 <- quadratic_function(y, sorted_best_coe[[8]]$a[[1]], sorted_best_coe[[8]]$b[[1]], sorted_best_coe[[8]]$c[[1]])

  for (k in seq(1, term %/% 8 + 1, by = 1)) {
    Cs[[k]][[4]][1] <- C_4_1[k]

    dDs[[k]][[2]][1] <- D_1_1[[k]]
    dDs[[k]][[2]][2] <- D_1_2[[k]]
    dDs[[k]][[2]][3] <- D_1_3[[k]]
    dDs[[k]][[2]][4] <- D_1_4[[k]]
    dDs[[k]][[3]][1] <- D_2_1[[k]]
    dDs[[k]][[3]][2] <- D_2_2[[k]]
    dDs[[k]][[4]][1] <- D_3_1[[k]]
  }
  i_groups <- inverseHaarWaveletTransformForGroups(Cs, dDs)
  i_groups <- lapply(i_groups, function(x) x * 8**0.5)
  allData <- movingAverage(i_groups, term)
  allData <- apply_inverse_transform(allData, dt)

  best_coe <- data.frame(a = numeric(), b = numeric(), c = numeric())
  for (m in seq(1, 8, by = 1)) {
    tmp_best_coe <- data.frame(a = sorted_best_coe[[m]]$a[[1]], b = sorted_best_coe[[m]]$b[[1]], c = sorted_best_coe[[m]]$c[[1]])
    best_coe <- rbind(best_coe, tmp_best_coe)
  }
  predictionData <- list(predictionData = tail(allData, predictionTerm), regressionCoefficient = best_coe)
  return(predictionData)
}

prepare_data <- function(Cs, Ds, predictionTerm) {
    tmp_Cs_4_1 <- list()
    tmp_Ds <- vector("list", 7)  # D[1][1] ~ D[3][1]

    for (j in seq(1, length(Ds) - predictionTerm, by = 1)) {
        tmp_Cs_4_1 <- c(tmp_Cs_4_1, Cs[[j]][[4]][1])
        for (i in 1:7) {
            tmp_Ds[[i]] <- c(tmp_Ds[[i]], Ds[[j]][[ceiling(i / 4)]][(i - 1) %% 4 + 1])
        }
    }

    coe <- c(list(tmp_Cs_4_1), tmp_Ds)
    return(coe)
}

apply_inverse_transform <- function(allData, dt) {
    if (dt == "A1") {
        return(inverseAnscombeTransformFromGroup(allData, var))
    } else if (dt == "A2") {
        return(inverseAnscombeTransform2FromGroup(allData, var))
    } else if (dt == "A3") {
        return(inverseAnscombeTransform3FromGroup(allData, var))
    } else if (dt == "B1") {
        return(inverseBartlettTransformFromGroup(allData, var))
    } else if (dt == "B2") {
        return(inverseBartlettTransform2FromGroup(allData, var))
    } else if (dt == "Fr") {
        return(inverseFreemanTransformFromGroup(allData, var))
    } else {
        return(allData)
    }
}