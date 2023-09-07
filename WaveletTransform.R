#Get maximum resolution
getHighestResolutionLevel = function(groupLength)
{
  level = log2(groupLength)
  level = as.integer(level)
  return(level)
}
#dataLength以下の最大の2の整数乗を満たす値を取得する。
#例：dataLength = 62の場合、戻り値は32です。
#理由：2^5 = 32 < 62 < 64 = 2^6
getGroupLength = function(dataLength)
{
  i = 1
  x = i
  while (i <= dataLength)
  {
    x = i
    i = i * 2
  }
  return(x)
}

#任意の長さのデータセットデータを、groupLengthに従って複数のサブデータセットに分割する。
getGroups = function(data, groupLength)
{
  i = 0
  dataLength = length(data)
  tempList = list()
  #print(data)
  while (groupLength + i <= dataLength)
  {
    tempData = data
    a = i+1
    b = groupLength + i
    cutData = tempData[a : b]
    tempList = append(tempList,list(cutData))
    i = i + 1
  }
  return(tempList)
}

# 1/sqrt(2) times
#Scale coefficients of discrete Hal wavelets expanded for a set of data
getScalingCoefficientsFromGroup = function(timeList) 
{
  lists = list()
  J = getHighestResolutionLevel( length(timeList) )
  lists = append(lists, list(timeList))
  j = 1
  while(j <= J)
  {
    tempList = c()
    k = 1
    while(k <= 2**(J - j))
    {
      coe = (1/sqrt(2))*(lists[[j]][2 * k - 1] + lists[[j]][2 * k])
      tempList = append(tempList,coe)
      k = k + 1
    }
    lists = append(lists, list(tempList))
    j = j + 1
  }
  return(lists)
}

# 1/sqrt(2)times
#Wavelet coefficients of discrete Hal wavelets are simultaneously expanded for multiple data sets
getWaveletCoefficientsFromGroup = function(coeList)
{
  lists = list()
  J = getHighestResolutionLevel(length(coeList[[1]]) )
  lists = append(lists, list(coeList[[1]]))
  j = 1
  while(j <= J)
  {
    tempList = c()
    k = 1
    while(k <= 2**(J - j))
    {
      c = (1/sqrt(2))*( coeList[[j]][2 * k - 1] - coeList[[j]][2 * k])
      tempList = append(tempList,c)
      k = k + 1
    }
    lists = append(lists,list(tempList))
    j = j + 1
  }
  return(lists)
}

#Scale coefficients of the discrete Hal wavelet are simultaneously expanded for multiple data sets
getScalingCoefficientsFromGroups = function(Groups)
{
  lists = list()
  groupsLength = length(Groups)
  i = 1
  while(i <= groupsLength)
  {
    tempList = getScalingCoefficientsFromGroup(Groups[[i]])
    lists = append(lists,list(tempList))
    i = i + 1
  }
  return(lists)
}

#Wavelet coefficients of discrete Hal wavelets unfolded for a set of data
getWaveletCoefficientsFromGroups = function(CS)
{
  lists = list()
  groupsLength = length(CS)
  i = 1
  while(i <= groupsLength)
  {
    tempList = getWaveletCoefficientsFromGroup(CS[[i]])
    lists = append(lists,list(tempList))
    i = i + 1
  }
  return(lists)
}

# 1/sqrt(2)times
#Convert a set of Haar wavelet coefficients with Haar scale coefficients to the original data
inverseHaarWaveletTransformForGroup = function(scalingCoe,waveletCoe)
{
  groupLength = length(scalingCoe)
  if (groupLength != length(waveletCoe))
  {
    return(FALSE)
  }
  J = groupLength
  k = 0
  j = groupLength
  #print("inverseHaarWaveletTransformForGroup")
  #print(scalingCoe[j])
  while(j > 1)
  {
    k = 1
    while(k <= 2**(J - j))
    {
      scalingCoe[[j - 1]][2 * k - 1] = (1/sqrt(2))*(scalingCoe[[j]][k] + waveletCoe[[j]][k])
      scalingCoe[[j - 1]][2 * k] = (1/sqrt(2))*(scalingCoe[[j]][k] - waveletCoe[[j]][k])
      k = k + 1
    }
    j = j - 1
  }
  return(scalingCoe[[1]])
}

#Convert multiple sets of Haar wavelet coefficients with Haar scale coefficients to the original data
inverseHaarWaveletTransformForGroups = function(scalingCoes,waveletCoes)
{
  groupsLength = length(scalingCoes)
  if (groupsLength != length(waveletCoes))
  {
    return(FALSE)
  }
  i = 1
  lists = list()
  while(i <= groupsLength)
  {
    tempList = inverseHaarWaveletTransformForGroup(scalingCoes[[i]],waveletCoes[[i]])
    lists = append(lists,list(tempList))
    i = i + 1
  }
  return(lists)
}

#Average multiple sub-datasets by displacement and combine them into one set
movingAverage = function(iGroups,dataLength)
{
  dataSum = numeric(dataLength)
  counter = numeric(dataLength)
  result  = numeric(dataLength)
  
  groupsSum = length(iGroups)
  groupLength = length(iGroups[[1]])
  i = 1
  while(i <= groupsSum)
  {
    j = 1
    while(j <= groupLength)
    {
      dataSum[i + j - 1] = dataSum[i + j - 1] + iGroups[[i]][j]
      counter[i + j - 1] = counter[i + j - 1] + 1
      j = j + 1
    }
    #print("循环")
    #print(dataSum)
    i = i + 1
  }

  k = 1
  while(k <= dataLength)
  {
    result[k] = dataSum[k] / counter[k]
    #if(result[k] < 0)
    #{
    #  result[k] = 0
    #}
    k = k + 1
  }
  return(result)
}
