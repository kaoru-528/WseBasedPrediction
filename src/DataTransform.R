# -----------------------------------------------
# Anscombe transformation
# -----------------------------------------------
AnscombeTransformFromGroups = function(groups,var)
{
    groupsLength = length(groups)
    lists = list()
    i = 1
    while(i <= groupsLength)
    {
        group = groups[[i]]
        lists = append(lists, list(AnscombeTransformFromGroup(group,var)))
        i = i + 1
    }
    return(lists)
}

#Applying the Anscombe transformation to a data set with a variance of var after transformation
AnscombeTransformFromGroup = function(group,var)
{
    Anscombelist = c()
    groupLength = length(group)
    i = 1
    while(i <= groupLength)
    {
        a = group[[i]] + 3/8
        b = a**0.5
        c = b * 2 * (var**0.5)
        Anscombelist = append(Anscombelist, c)
        i = i + 1
    }
    return(Anscombelist)
}

#The inverse Anscombe transformation is applied to multiple data sets simultaneously, and the variance before transformation is var
inverseAnscombeTransformFromGroups = function(AT_datas,var)
{
    AT_datas = copy.deepcopy(AT_datas)
    groupsLength = length(AT_datas)
    i = 1
    lists = list()
    while(i <= groupsLength)
    {
        lists = append(lists, list(inverseAnscombeTransformFromGroup(AT_datas[[i]],var)))
        i = i + 1
    }
    return(lists)
}

#Applying the inverse Anscombe transformation to a dataset with a variance of var before transformation
inverseAnscombeTransformFromGroup = function(AT_data,var)
{
    groupsLength = length(AT_data)
    i = 1
    lists = c()
    while(i <= groupsLength)
    {
        a = AT_data[[i]]
        b = a * a
        d = (2 * (var**0.5)) ** -2
        c = d*b - 3/8
        c = round(c, 11)
        lists = append(lists, c)
        i = i + 1
    }
    return(lists)
}

# -----------------------------------------
# The inverse Anscombe transformation 2
# ((si/2)^2)-1/8
# -----------------------------------------
inverseAnscombeTransform2FromGroups = function(AT_datas,var)
{
    AT_datas = copy.deepcopy(AT_datas)
    groupsLength = length(AT_datas)
    i = 1
    lists = list()
    while(i <= groupsLength)
    {
            lists = append(lists, list(inverseAnscombeTransform2FromGroup(AT_datas[[i]],var)))
            i = i + 1
    }
    return(lists)
}

#Applying the inverse Anscombe transformation 2 to a dataset with a variance of var before transformation
inverseAnscombeTransform2FromGroup = function(AT_data,var)
{
    groupsLength = length(AT_data)
    i = 1
    lists = c()
    while(i <= groupsLength)
    {
            a = AT_data[[i]]
            b = a * a
            d = (2 * (var**0.5)) ** -2
            c = d*b - 1/8
            c = round(c, 11)
            lists = append(lists, c)
            i = i + 1
    }
    return(lists)
}


# -----------------------------------------
# The inverse Anscombe transformation 3
# (si^2)/4+sqrt(3/2)/(4*si)-11/(8*(si^2))+5*sqrt(3/2)/(8*(si^3))-1/8
# -----------------------------------------
inverseAnscombeTransform3FromGroups = function(AT_datas,var)
{
    AT_datas = copy.deepcopy(AT_datas)
    groupsLength = length(AT_datas)
    i = 1
    lists = list()
    while(i <= groupsLength)
    {
            lists = append(lists, list(inverseAnscombeTransform3FromGroup(AT_datas[[i]],var)))
            i = i + 1
    }
    return(lists)
}


#Applying the inverse Anscombe transformation 3 to a dataset with a variance of var before transformation
inverseAnscombeTransform3FromGroup = function(AT_data,var)
{
    groupsLength = length(AT_data)
    i = 1
    lists = c()
    while(i <= groupsLength)
    {
            a = AT_data[[i]]
            b = a * a
            d = (2 * (var**0.5)) ** -2
            e = a**(-1)
            f = a**(-2)
            g = a**(-3)
            c = d*b + (d**-0.5)*((3/2)**(0.5))*e - (d**-1)*11*f/2 + (d**-1.5)*5*((3/2)**(0.5))*g/4- 1/8
            if(a<2*((3/8)**(0.5)))
            {
                    c=0
            }
            c = round(c, 11)
            lists = append(lists, c)
            i = i + 1
    }
    return(lists)
}

# -----------------------------------------------
# Bartlet
# -----------------------------------------------
BartlettTransformFromGroups = function(groups,var)
{
    groupsLength = length(groups)
    lists = list()
    i = 1
    while(i <= groupsLength)
    {
        lists = append(lists, list(BartlettTransformFromGroup(groups[[i]],var)))
        i = i + 1
    }
    return(lists)
}

#Applying a Bartlett transformation to a data set with a variance of var after transformation
BartlettTransformFromGroup = function(group,var)
{
    lists = c()
    groupLength = length(group)
    i = 1
    while(i <= groupLength)
    {
        a = group[[i]] + 0.5
        b = a**0.5
        c = b * 2 * (var**0.5)
        lists = append(lists, c)
        i = i + 1
    }
    return(lists)
}

#The inverse Bartlett transformation is applied simultaneously to multiple data sets, and the variance before transformation is var
inverseBartlettTransformFromGroups = function(groups,var)
{
    groupsLength = length(groups)
    lists = list()
    i = 1
    while(i <= groupsLength)
    {
        lists = append(lists, list(inverseBartlettTransformFromGroup(groups[[i]],var)))
        i = i + 1
    }
    return(lists)
}

#Applying an inverse Bartlett transformation to a dataset with a pre-transformation variance of var
inverseBartlettTransformFromGroup = function(BT_data,var)
{
    groupsLength = length(BT_data)
    i = 1
    lists = c()
    while(i <= groupsLength)
    {
        a = BT_data[[i]] * BT_data[[i]]
        b = (2 * (var**0.5)) ** -2
        c = b*a - 0.5
        c = round(c, 11)
        lists = append(lists, c)
        i = i + 1
    }
    return(lists)
}

# -----------------------------------------------
# Applying Bartlett transformation 2
# bi=2*sqrt(yi)
# -----------------------------------------------
BartlettTransform2FromGroups = function(groups,var)
{
    groupsLength = length(groups)
    lists = list()
    i = 1
    while(i <= groupsLength)
    {
            lists = append(lists, list(BartlettTransform2FromGroup(groups[[i]],var)))
            i = i + 1
    }
    return(lists)
}


#Applying a Bartlett transformation 2 to a data set with a variance of var after transformation
BartlettTransform2FromGroup = function(group,var)
{
    lists = c()
    groupLength = length(group)
    i = 1
    while(i <= groupLength)
    {
            a = group[[i]]
            b = a**0.5
            c = b * 2 * (var**0.5)
            lists = append(lists, c)
            i = i + 1
    }
    return(lists)
}


# -----------------------------------------------
# The inverse Bartlett transformation 2
# (bi^2)/4
# -----------------------------------------------
inverseBartlettTransform2FromGroups = function(groups,var)
{
    groupsLength = length(groups)
    lists = list()
    i = 1
    while(i <= groupsLength)
    {
            lists = append(lists, list(inverseBartlettTransform2FromGroup(groups[[i]],var)))
            i = i + 1
    }
    return(lists)
}


#Applying an inverse Bartlett transformation 2 to a dataset with a pre-transformation variance of var
inverseBartlettTransform2FromGroup = function(BT_data,var)
{
    groupsLength = length(BT_data)
    i = 1
    lists = c()
    while(i <= groupsLength)
    {
            a = BT_data[[i]] * BT_data[[i]]
            b = (2 * (var**0.5)) ** -2
            c = b*a
            c = round(c, 11)
            lists = append(lists, c)
            i = i + 1
    }
    return(lists)
}


# -----------------------------------------------
# Fisz
# -----------------------------------------------
FiszTransformFromGroups = function(scalingCoes,waveletCoes,var)
{
    groupsLength = length(scalingCoes)
    lists = list()
    i = 1
    while(i <= groupsLength)
    {
        lists = append(lists, list(FiszTransformFromGroup(scalingCoes[[i]],waveletCoes[[i]],var)))
        i = i + 1
    }
    return(lists)
}

#Applying the Fisz transformation to a data set, the variance after transformation is var
FiszTransformFromGroup = function(scalingCoe,waveletCoe,var)
{
    lists = list()
    groupLength = length(scalingCoe)
    j = 1
    while(j <= groupLength)
    {
        i = 1
        levelLength = length(scalingCoe[[j]])
        coeList = c()
        while(i <= levelLength)
        {
            if(scalingCoe[[j]][i] == 0)
            {
                coeList = append(coeList, 0.0)
            }
            else
            {
                if(scalingCoe[[j]][i] < 0)
                {
                    print("FiszTransformFromGroup")
                }
                coeList = append(coeList, (var**0.5) * waveletCoe[[j]][[i]]/(scalingCoe[[j]][i]**0.5))
            }
            i = i + 1
        }
        lists = append(lists, list(coeList))
        j = j + 1
    }
    return(lists)
}

#The inverse Fisz transformation is applied simultaneously to multiple data sets, and the variance before transformation is var
inverseFiszTransformFromGroups = function(scalingCoes,FiszCoes,var)
{
    groupsLength = length(scalingCoes)
    C_list = list()
    D_list = list()
    lists = list()
    i = 1
    while(i <= groupsLength)
    {
        a = inverseFiszTransformFromGroup(scalingCoes[[i]],FiszCoes[[i]],var)
        C_list = append(C_list, list(a[[1]]))
        D_list = append(D_list, list(a[[2]]))
        i = i + 1
    }

    lists = append(lists, list(C_list))
    lists = append(lists, list(D_list))
    return(lists)
}

#Apply the Fisz transformation to a data set with a variance of var before transformation
inverseFiszTransformFromGroup = function(scalingCoe,FiszCoe,var)
{
    lists = list()
    C_lists = list()
    D_lists = list()
    groupLength = length(scalingCoe)
    j = groupLength
    while(j > 0)
    {
        levelLength = length(scalingCoe[[j]])
        D_list = c()
        i = 1
        while(i <= levelLength)
        {
            D_list = append(D_list, Fisz_getD(scalingCoe[[j]][i],FiszCoe[[j]][i],var))
            i = i + 1
        }
        i = 1
        while(i <= levelLength && j > 1)
        {
            scalingCoe[[j - 1]][2 * i - 1]     = scalingCoe[[j]][i] + D_list[i];
            scalingCoe[[j - 1]][2 * i] = scalingCoe[[j]][i] - D_list[i];
            if(scalingCoe[[j - 1]][2 * i - 1] < 0)
            {
                scalingCoe[[j - 1]][2 * i - 1] = 0
            }
            if(scalingCoe[[j - 1]][2 * i] < 0)
            {
                scalingCoe[[j - 1]][2 * i] = 0
            }
            i = i + 1;
        }
        D_lists = append(D_lists, list(D_list))
        j = j - 1
    }
    D_listx = list()
    i = groupLength;
    while(i >= 1)
    {
        D_listx = append(D_listx, list(D_lists[[i]]))
        i = i - 1;
    }
    lists = append(lists, list(scalingCoe))
    lists = append(lists, list(D_listx))
    return( lists)
}
#Wavelet coefficients in Poisson space are calculated from scale coefficients and wavelet coefficients in Gaussian space
Fisz_getD = function(c,f,var)
{
    f = f/(var ** 0.5)
    res = f*(c**0.5)
    res = round(res, 11)
    return(res)
}

# -----------------------------------------------
# Freeman
# -----------------------------------------------
FreemanTransformFromGroups = function(groups,var)
{
    groupsLength = length(groups)
    lists = list()
    i = 1
    while(i <= groupsLength)
    {
            lists = append(lists, list(FreemanTransformFromGroup(groups[[i]],var)))
            i = i + 1
    }
    return(lists)
}

#Applying a Freeman transformation to a data set with a variance of var after transformation
FreemanTransformFromGroup = function(group,var)
{
    lists = c()
    groupLength = length(group)
    i = 1
    while(i <= groupLength)
    {
            a = group[[i]] + 1
            b = a**0.5
            d = group[[i]]
            e = d**0.5
            c = b * (var**0.5) + e * (var**0.5)
            lists = append(lists, c)
            i = i + 1
    }
    return(lists)
}

#The inverse Freeman transformation is applied simultaneously to multiple data sets, and the variance before transformation is var
inverseFreemanTransformFromGroups = function(groups,var)
{
    groupsLength = length(groups)
    lists = list()
    i = 1
    while(i <= groupsLength)
    {
            lists = append(lists, list(inverseFreemanTransformFromGroup(groups[[i]],var)))
            i = i + 1
    }
    return(lists)
}

#Applying an inverse Freeman transformation to a dataset with a pre-transformation variance of var
inverseFreemanTransformFromGroup = function(FT_data,var)
{
    groupsLength = length(FT_data)
    i = 1
    lists = c()
    while(i <= groupsLength)
    {
            a = FT_data[[i]] * FT_data[[i]]
            b = (2 * (var**0.5)) ** -2
            d = a**(-1)
            e = b**(-1)
            c = b*a +e*d - 0.5
            c = round(c, 11)
            lists = append(lists, c)
            i = i + 1
    }
    return(lists)
}