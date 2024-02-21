# Load wavelet conversion module
WaveletTransform_Path = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/src/WaveletTransform.R")
source(WaveletTransform_Path)
# Load data conversion module
DT_Path = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/src/DataTransform.R")
source(DT_Path)
# Load Threshold Module
Threshold_Path = paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/src/Threshold.R")
source(Threshold_Path)

# Hal wavelet estimation without data transformation
wse = function(data, dt, thresholdName, thresholdMode, var, index, initThresholdvalue)
{
  if(dt == "none" && thresholdName != "ldt"){
    print("Please chack the parameter. If you want to use dt=none, please set thresholdName=ldt.")
  } else if(dt != "none" && thresholdName == "ldt"){
    print("Please chack the parameter. If you want to use thresholdName=ldt, please set dt=none.")
  }
  else{
    groupLength = 2^index
    # Get data length
    dataLength = length(data)
    if(groupLength >= getGroupLength(dataLength)){
      # Get subdata length
      groupLength = getGroupLength(dataLength)
    }

    # Cut the original data into a number of sub-data of length 2^J
    groups = getGroups(data,groupLength)

    if(dt == "Fi"){
      # Transform the sub-data into Gaussian data by Fisz transformation
      Cs1  = getScalingCoefficientsFromGroups(groups)
      Ds1  = getWaveletCoefficientsFromGroups(Cs1)
      Fi1  = FiszTransformFromGroups(Cs1,Ds1,var)
      fiszGroups = inverseHaarWaveletTransformForGroups(Cs1,Fi1)

      fiszGroups = lapply(fiszGroups, function(x) x/groupLength**0.5)
      
      # Calculate c
      Cs2  = getScalingCoefficientsFromGroups(fiszGroups)
      #Calculate d
      Ds2  = getWaveletCoefficientsFromGroups(Cs2)
      
      # Noise reduction of wavelet coefficients using thresholdMode noise reduction rule, thresholdName threshold
      Denoise_Ds2 = ThresholdForGroups(Ds2,thresholdMode,thresholdName)
        
      # Perform inverse Fisz data conversion
      f_igroups = inverseHaarWaveletTransformForGroups(Cs2,Denoise_Ds2)
      Cs3  = getScalingCoefficientsFromGroups(f_igroups)
      Fs2  = getWaveletCoefficientsFromGroups(Cs3)
      CDs  = inverseFiszTransformFromGroups(Cs3,Fs2,var)
      Cs4 = CDs[[1]]
      Ds3 = CDs[[2]]
      
      # Perform inverse wavelet conversion
      thresholded_groups = inverseHaarWaveletTransformForGroups(Cs4,Ds3)
      thresholded_groups = lapply(thresholded_groups, function(x) x*groupLength**0.5)
      
      # Perform moving average
      thresholded_data= movingAverage(thresholded_groups,dataLength)

      thresholdedData = list(estimationData=thresholded_data, cs=Cs4,ds=Ds3, denoiseDs=Denoise_Ds2)
    }
    else{
      if(dt == "A1" || dt == "A2"|| dt == "A3"){
        #Transform sub-data to Gaussian data by Anscombe
        groups = AnscombeTransformFromGroups(groups,var)
      }
      else if(dt == "B1"){
        #Transform sub-data to Gaussian data by Bartlet
        groups = BartlettTransformFromGroups(groups,var)
      }
      else if(dt == "B2"){
        #Transform sub-data to Gaussian data by Bartlet
        groups = BartlettTransform2FromGroups(groups,var)
      }
      else if (dt == "Fr") {
        groups = FreemanTransformFromGroups(groups,var)
      }
      else{
        groups = groups
      }
      groups = lapply(groups, function(x) x/(groupLength**0.5))

      # Calculate c
      Cs = getScalingCoefficientsFromGroups(groups)
      #Calculate d
      Ds = getWaveletCoefficientsFromGroups(Cs)

      Denoise_Ds = ThresholdForGroups(Ds,thresholdMode,thresholdName, dt, groups, initThresholdvalue)
      # Perform inverse wavelet conversion
      thresholded_groups = inverseHaarWaveletTransformForGroups(Cs,Denoise_Ds)
      thresholded_groups = lapply(thresholded_groups, function(x) x*groupLength**0.5)

      # Perform moving average
      if(thresholdName == "none"){
        thresholded_data = thresholded_groups
        print("none")
      }
      else {
        thresholded_data = movingAverage(thresholded_groups,dataLength)
      }

      if(dt == "A1"){
      # Perform inverse Anscombe data conversion
      thresholded_data = inverseAnscombeTransformFromGroup(thresholded_data,var);
      }
      else if(dt == "A2"){
        # Perform inverse Anscombe data conversion
        thresholded_data = inverseAnscombeTransform2FromGroup(thresholded_data,var);
      }
      else if(dt == "A3"){
        # Perform inverse Anscombe data conversion
        thresholded_data = inverseAnscombeTransform3FromGroup(thresholded_data,var);
      }
      else if(dt == "B1"){
        # Perform inverse Anscombe data conversion
        thresholded_data = inverseBartlettTransformFromGroup(thresholded_data,var);
      }
      else if(dt == "B2"){
        # Perform inverse Anscombe data conversion
        thresholded_data = inverseBartlettTransform2FromGroup(thresholded_data,var);
      }
      else if (dt == "Fr") {
        thresholded_data = inverseFreemanTransformFromGroup(thresholded_data,var)
      }
      else{
        thresholded_data = thresholded_data
      }
      thresholdedData = list(estimationData=thresholded_data, cs=Cs,ds=Ds, denoiseDs=Denoise_Ds)
    }

    return(thresholdedData)
  }
}

# Translation-invariant Hal wavelet estimation without data transformation
tipsh = function(data, thresholdMode, var, index)
{
  thresholdName = "ldt"
  groupLength = 2^index
  # Get data length
  dataLength = length(data)
  if(groupLength >= getGroupLength(dataLength)){
          # Get subdata length
          groupLength = getGroupLength(dataLength)
  }
  # Cut the original data into a number of sub-data of length 2^J
  groups = getGroups(data,groupLength)
  groups = lapply(groups, function(x) x/groupLength**0.5)
  thresholdedGroups = list()
  #Transration-Invariant Denoising
  for(i in 1:(dataLength-groupLength+1)){
          templist=groups[[i]]
          shiftgroup=list()
          lists=list()
          for(h in 1:(groupLength-1)){
                  shiftgroup = list(c(templist[(h+1):groupLength],templist[1:h]))
                  Cs = getScalingCoefficientsFromGroup(as.numeric(shiftgroup[[1]]))
                  Ds = getWaveletCoefficientsFromGroup(Cs)
                  Denoise_Ds = ThresholdForGroup(Ds,thresholdMode,thresholdName)
                  thresholded_group = inverseHaarWaveletTransformForGroup(Cs,Denoise_Ds)
                  lists = append(lists,list(thresholded_group))
          }
          Cs=getScalingCoefficientsFromGroup(templist)
          DS=getWaveletCoefficientsFromGroup(Cs)
          Denoise_Ds = ThresholdForGroup(Ds,thresholdMode,thresholdName)
          thresholded_group = inverseHaarWaveletTransformForGroup(Cs,Denoise_Ds)
          lists = append(lists,list(thresholded_group))
          for(h in 1:(groupLength-1)){
                  templist=lists[[h]]
                  lists[h]=list(c(templist[(groupLength-h+1):(groupLength)],templist[1:(groupLength-h)]))
          }
          sumlist=c()
          for(j in 1:groupLength){
                  sl=0
                  for(k in 1:groupLength){
                          sl=sl+lists[[k]][j]
                  }
                  sumlist = c(sumlist,sl)
          }
          thresholdedGroups[[i]]=(sumlist/groupLength)
  }
  
  thresholdedGroups = lapply(thresholdedGroups, function(x) x*groupLength**0.5)
  
  # Perform moving average
  thresholdedData = movingAverage(thresholdedGroups,dataLength)
  thresholdedData = list(estimationData=thresholdedData, cs=Cs,ds=Ds, denoiseDs=Denoise_Ds)
  
  # Return Results
  return(thresholdedData)
}

# cumulative function
toCulData = function(data)
{
  culData = c()
  oldValue = 0
  index = 1
  while(index <= length(data))
  {
    nowValue = data[[index]] + oldValue
    culData = append(culData,nowValue)
    oldValue = nowValue
    index = index + 1
  }
  return(culData)
}

# Load data from file
loadData = function(dataPath)
{
  dataPath = paste0(dirname(rstudioapi::getSourceEditorContext()$path),dataPath)
  ds = read.table(dataPath)[2]
  ds = as.numeric(ds$V2)
  return(ds)
}


# creating file format
createFile = function(i, resultPath, time)
{
    file_name_edata = paste0(time,"_edata_J=",i  ,".csv")
    file_name_coe = paste0(time,"_coe_J=",i  ,".csv")
    file_name_variable = paste0(time,"_var_J=",i  ,".RData")
    edata = paste0(resultPath, file_name_edata)
    coe = paste0(resultPath, file_name_coe)
    variable = paste0(resultPath, file_name_variable)
    file_path = list(edata = edata, coe = coe, variable = variable)
    return(file_path)
}

# creating result
createResult = function(hard, soft, index, resultPath){
  time = Sys.time() %>% format("%H-%M-%S")
  edata = list(hard = round(hard$estimationData, digits = 3), soft = round(soft$estimationData, digits = 3))
  hard_coe= rbind("Cs",as.data.frame(t(sapply(hard$Cs, unlist))),"Ds",as.data.frame(t(sapply(hard$Ds, unlist))),"Denoise_Ds",as.data.frame(t(sapply(hard$Denoise_Ds, unlist))))
  soft_coe= rbind("Cs",as.data.frame(t(sapply(soft$Cs, unlist))),"Ds",as.data.frame(t(sapply(soft$Ds, unlist))),"Denoise_Ds",as.data.frame(t(sapply(soft$Denoise_Ds, unlist))))
  coe = rbind("hard",hard_coe,"soft",soft_coe)
  file_path = createFile(index, resultPath, time)
  write.csv(edata, file_path$edata, row.names = FALSE)
  write.csv(coe, file_path$coe, row.names = FALSE)
  save(hard, soft, file = file_path$variable)
}