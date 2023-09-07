# Load wavelet conversion module
WaveletTransform_Path = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/WaveletTransform.R")
print("Load wavelet conversion module")
print(WaveletTransform_Path)
source(WaveletTransform_Path)
# Load data conversion module
DT_Path = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/DataTransform.R")
print("Load data conversion module")
print(DT_Path)
source(DT_Path)
# Load Threshold Module
Threshold_Path = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/Threshold.R")
print("Load Threshold Module")
print(Threshold_Path)
source(Threshold_Path)
# Load Evaluation Module
Evaluation_Path = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/EvaluationIndex.R")
print("Load Evaluation Module")
print(Evaluation_Path)
source(Evaluation_Path)

# Hal wavelet estimation without data transformation
H = function(data,thresholdName,thresholdMode, index)
{
  groupLength = 2^index
  # Get data length
  dataLength = length(data)
  #print("dataLength")
  #print(dataLength)
  

  if(groupLength >= getGroupLength(dataLength)){
    # Get subdata length
    groupLength = getGroupLength(dataLength)
    #print("groupLength")
    #print(groupLength)
  }
  
  # Cut the original data into a number of sub-data of length 2^J
  groups = getGroups(data,groupLength)
  #print("groups[[1]]")
  #print(groups[[1]])
  
  # Calculate c
  #print("Start calculating scale factor")
  Cs = getScalingCoefficientsFromGroups(groups)
  # df = as.data.frame(t(sapply(Cs, unlist)))
  # write.csv(round(df,digits = 3), "./output/NDT_WSE/ScallingCoefficients.csv", row.names = FALSE)
  # print("Cs[[1]]")
  # print(Cs[[1]])
  
  #Calculate d
  #print("Start calculating wavelet coefficients")
  Ds = getWaveletCoefficientsFromGroups(Cs)
  # dw = as.data.frame(t(sapply(Ds, unlist)))
  # write.csv(round(dw, digits = 3), "./output/NDT_WSE/WaveletCoefficients.csv", row.names = FALSE)
  # print("Ds[[1]]")
  # print(Ds[[1]])
  
  # Noise reduction of wavelet coefficients using thresholdMode noise reduction rule, thresholdName threshold
  #print("Start calculating the noise reduction wavelet coefficients")
  Denoise_Ds = ThresholdForGroups(Ds,thresholdMode,thresholdName)
  
  # No noise reduction (for testing)
  if(thresholdName == "none")
  {
    Denoise_Ds = Ds
  }
  # dn = as.data.frame(t(sapply(Denoise_Ds, unlist)))
  # write.csv(round(dn,digits = 3), "./output/NDT_WSE/Dnoise_Ds.csv", row.names = FALSE)
  
  # Perform inverse wavelet conversion
  #print("Start restoring data")
  i_groups = inverseHaarWaveletTransformForGroups(Cs,Denoise_Ds)
  #print("i_groups[[1]]")
  #print(i_groups[[1]])
  
  # Perform moving average
  #print("Perform moving average")
  idata = movingAverage(i_groups,dataLength)

  idata = list(idata=idata, Cs=Cs,Ds=Ds, Denoise_Ds=Denoise_Ds)
  
  # Return Results
  return(idata)
}

# Anscombe Transformation of Hal Wavelet Estimation
HAT = function(data,thresholdName,thresholdMode,var=1, index)
{
  groupLength = 2^index
  # Get data length
  dataLength = length(data)
  #print("dataLength")
  #print(dataLength)
  
  # Get subdata length
  if(groupLength >= getGroupLength(dataLength)){
    # Get subdata length
    groupLength = getGroupLength(dataLength)
    #print("groupLength")
    #print(groupLength)
  }
  
  # Cut the original data into a number of sub-data of length 2^J
  groups = getGroups(data,groupLength)
  #print("groups[[1]]")
  #print(groups[[1]])
  
  #Transform sub-data to Gaussian data by Anscombe
  a_groups = AnscombeTransformFromGroups(groups,var)
  #print("Anscombe : groups[[1]]")
  #print(groups[[1]])
  
  # Calculate c
  #print("Start calculating scale factor")
  Cs = getScalingCoefficientsFromGroups(a_groups)
  #print("Cs[[1]]")
  #print(Cs[[1]])
  
  #Calculate d
  #print("Start calculating wavelet coefficients")
  Ds = getWaveletCoefficientsFromGroups(Cs)
  #print("Ds[[1]]")
  #print(Ds[[1]])
  
  # Noise reduction of wavelet coefficients using thresholdMode noise reduction rule, thresholdName threshold
  #print("Start calculating the noise reduction wavelet coefficients")
  Denoise_Ds = ThresholdForGroups(Ds,thresholdMode,thresholdName)
  
  # No noise reduction (for testing)
  if(thresholdName == "none")
  {
    Denoise_Ds = Ds
  }
  
  # Perform inverse wavelet conversion
  #print("Start restoring data")
  i_groups = inverseHaarWaveletTransformForGroups(Cs,Denoise_Ds)
  #print("i_groups[[1]]")
  #print(i_groups[[1]])
  
  # Perform moving average
  #print("Perform moving average")
  a_idata = movingAverage(i_groups,dataLength)
  
  # Perform inverse Anscombe data conversion
  idata = inverseAnscombeTransformFromGroup(a_idata,var);
  
  idata = list(idata=idata, Cs=Cs,Ds=Ds, Denoise_Ds=Denoise_Ds)
  # Return Results
  return(idata)
}
