ThresholdForGroups = function(Ds,mode,ThresholdName)
{
	#print("4")
	groupLength = length(Ds)
	lists = list()
	i = 1
	#print(Ds[0][0])
	#print(Ds[1][0])
	while(i <= groupLength)
	{
		#if(i + 1 <= groupLength)
		#	lht_last = Ds[[i+1]][[1]][length(Ds[[i+1]][[1]])-1]
		#else
		#	lht_last = Ds[[1]][[1]][length(Ds[[1]][[1]])-1]
		#Temporarily close lht_last
		lht_last = Ds[[i]][[1]][[1]]
		#print(lht_last)
		list_s = ThresholdForGroup(Ds[[i]],mode,ThresholdName,lht_last)
		
		lists = append(lists, list(list_s))
		i = i + 1
	}
	return(lists)
}

#Apply the soft or hard thresholding method of ThresholdName to a set of wavelet coefficients
ThresholdForGroup = function(GroupWaveletCoefficients,mode,ThresholdName,lht_last)
{
    dataLength = length(GroupWaveletCoefficients[[1]])
    #print(ThresholdName)
    str_ThresholdName = ThresholdName
    #print(str_ThresholdName)
    #Default Threshold
    t = 1000
    #Calculating ut thresholds
    if(str_ThresholdName == 'ut')
    {
        t = getUniversalThreshold(dataLength)
    }
    #Calculate lht threshold
    if(str_ThresholdName == 'lht')
    {
        #t = getMyThreshold(GroupWaveletCoefficients[[1]],mode,lht_last)
    }
    #print("step 1")
    #Preparing for threshold processing
	lists = list()
    lists = append(lists,list(GroupWaveletCoefficients[[1]]))
    i = 2
    groupLength = length(GroupWaveletCoefficients)
    #Scale factor at full resolution
    if(str_ThresholdName == 'ldt' || str_ThresholdName == 'lut')
    {
        C = getScalingCoefficientsFromGroup(GroupWaveletCoefficients[[1]])
    }
    if(str_ThresholdName == 'lht')
    {
        #print(t)
        #pass
    }
    while(i <= groupLength)
    {
        #Calculate ldt threshold, and perform threshold processing
        if(str_ThresholdName == 'ldt')
        {
            #print("step 1.5")
            tempList = ldtThreshold(GroupWaveletCoefficients[[i]],mode,i,dataLength,C)
            #print("step 2")
        }
        else
        {
            #Calculate lut threshold
            if(str_ThresholdName == 'lut')
            {
                ut_dataLength = length(C[[i]])
                t = getUniversalThreshold(ut_dataLength)
            }
            #Start of non-ldt threshold processing
            if(str_ThresholdName == 'lht')
            {
                #print(GroupWaveletCoefficients[i])
                #pass
            }
            tempList = ThresholdForOneLevel(GroupWaveletCoefficients[[i]],mode,t)
            if(str_ThresholdName == 'lht')
            {
                #print(list)
                #pass
            }
        }
        #print("step 3")
        lists = append(lists,list(tempList))
        i = i + 1
    }
    #print("group end")
    return(lists)
}

# ---------------------------------
# Get ut threshold
# ut:Universal Threshold
# ---------------------------------
getUniversalThreshold = function(groupLength)
{
	#return 0;
	a = log(groupLength)
	#a=math.log(81)
	b = 2*a
	c = b**0.5
	return(c)
}
# ---------------------------------
# Get ldt threshold
# ldt:Level-dependent-Threshold
# ---------------------------------
getLevelDependentThreshold = function(J,now_level,mean)
{
	a = 2 ** (-1 * 0.5 * (J - now_level + 2))
	log2j = log(2 ** now_level)
	b = 2 * log2j
	c = (4 * log2j) ** 2
	d = 8 * mean * log2j
	t = a * (b + ((c + d) ** 0.5))
	
	return(t)
}
# getLevelDependentThreshold = function(J,now_level,mean)
# {
# 	a = 2 ** (-1 * 0.5 * (J - now_level))
# 	log2j = log(2 ** now_level)
# 	b = log2j
# 	c = log2j ** 2
# 	d = 2 * mean * log2j * 2 ** now_level
# 	t = a * (b + ((c + d) ** 0.5))	
# 	return(t)
# }


# Thresholding the wavelet coefficients of a layer at a threshold value of t
ThresholdForOneLevel = function(WaveletCoefficients,mode,t)
{
	coefficientsLength = length(WaveletCoefficients)
	tempList = c()
	i = 1
	while(i <= coefficientsLength)
	{
		a = Threshold(WaveletCoefficients[i],t,mode)
		tempList = append(tempList,a)
		i = i + 1
	}
	return(tempList)
}

#Thresholding of the value coe according to the threshold r
Threshold = function(coe,r,mode)
{
	if(abs(coe) <= r)
	{
	    return(0)
	}
	if(mode == 'h')
	{
	    return(coe)
	}
	else
	{
	    if(coe > 0)
	    {
	        return(coe - r)
	    }
	    else
	    {
	        return(coe + r)
	    }
	}
}

#Calculating ldt and thresholding the data
ldtThreshold = function(data,mode,loop_level,dataLength,C)
{
	#Highest Resolution
	J = getHighestResolutionLevel(dataLength)
	#Current Resolution
	#j = J - loop_level + 1
	#Thresholding the data one by one
	i = 1
	tempList = c()
	#print(loop_level)
	#print(data)
	while (i <= length(data))
	{
		#Get ldt threshold
		mean = C[[loop_level]][i]
		#print(mean)
		#t = getLevelDependentThreshold(J,j,mean)
		t = getLevelDependentThreshold(J,loop_level,mean)
		#t = getLevelDependentThreshold_test(J,loop_level,C,dataLength,i)
		#print(mean)
		#print(t)
		#Threshold processing
		denoise_data = Threshold(data[[i]],t,mode)
		tempList = append(tempList,denoise_data)
		i = i + 1
	}
	#print(list)
	return(tempList)
}
