# ----------------------------------------------- Anscombe transformation -----------------------------------------------
anscombe_transform_from_groups <- function(groups, var) {
    groups_length <- length(groups)
    lists <- list()
    i <- 1
    while (i <= groups_length) {
        group <- groups[[i]]
        lists <- append(lists, list(anscombe_transform_from_group(group, var)))
        i <- i + 1
    }
    return(lists)
}

# Applying the Anscombe transformation to a data set with a variance of var after transformation
anscombe_transform_from_group <- function(group, var) {
    anscombe_list <- c()
    group_length <- length(group)
    i <- 1
    while (i <= group_length) {
        a <- group[[i]] + 3/8
        b <- a^0.5
        c <- b * 2 * (var^0.5)
        anscombe_list <- append(anscombe_list, c)
        i <- i + 1
    }
    return(anscombe_list)
}

# The inverse Anscombe transformation is applied to multiple data sets simultaneously, and the variance before transformation is var
inverse_anscombe_transform_from_groups <- function(at_datas, var) {
    at_datas <- copy.deepcopy(at_datas)
    groups_length <- length(at_datas)
    i <- 1
    lists <- list()
    while (i <= groups_length) {
        lists <- append(lists, list(inverse_anscombe_transform_from_group(at_datas[[i]], var)))
        i <- i + 1
    }
    return(lists)
}

# Applying the inverse Anscombe transformation to a dataset with a variance of var before transformation
inverse_anscombe_transform_from_group <- function(at_data, var) {
    groups_length <- length(at_data)
    i <- 1
    lists <- c()
    while (i <= groups_length) {
        a <- at_data[[i]]
        b <- a * a
        d <- (2 * (var^0.5))^-2
        c <- d * b - 3/8
        c <- round(c, 11)
        lists <- append(lists, c)
        i <- i + 1
    }
    return(lists)
}

# ----------------------------------------- The inverse Anscombe transformation 2 ((si/2)^2)-1/8 -----------------------------------------
inverse_anscombe_transform2_from_groups <- function(at_datas, var) {
    at_datas <- copy.deepcopy(at_datas)
    groups_length <- length(at_datas)
    i <- 1
    lists <- list()
    while (i <= groups_length) {
        lists <- append(lists, list(inverse_anscombe_transform2_from_group(at_datas[[i]], var)))
        i <- i + 1
    }
    return(lists)
}

# Applying the inverse Anscombe transformation 2 to a dataset with a variance of var before transformation
inverse_anscombe_transform2_from_group <- function(at_data, var) {
    groups_length <- length(at_data)
    i <- 1
    lists <- c()
    while (i <= groups_length) {
        a <- at_data[[i]]
        b <- a * a
        d <- (2 * (var^0.5))^-2
        c <- d * b - 1/8
        c <- round(c, 11)
        lists <- append(lists, c)
        i <- i + 1
    }
    return(lists)
}


# ----------------------------------------- The inverse Anscombe transformation 3 (si^2)/4+sqrt(3/2)/(4*si)-11/(8*(si^2))+5*sqrt(3/2)/(8*(si^3))-1/8 -----------------------------------------
inverse_anscombe_transform3_from_groups <- function(at_datas, var) {
    at_datas <- copy.deepcopy(at_datas)
    groups_length <- length(at_datas)
    i <- 1
    lists <- list()
    while (i <= groups_length) {
        lists <- append(lists, list(inverse_anscombe_transform3_from_group(at_datas[[i]], var)))
        i <- i + 1
    }
    return(lists)
}


# Applying the inverse Anscombe transformation 3 to a dataset with a variance of var before transformation
inverse_anscombe_transform3_from_group <- function(at_data, var) {
    groups_length <- length(at_data)
    i <- 1
    lists <- c()
    while (i <= groups_length) {
        a <- at_data[[i]]
        b <- a * a
        d <- (2 * (var^0.5))^-2
        e <- a^(-1)
        f <- a^(-2)
        g <- a^(-3)
        c <- d * b + (d^-0.5) * ((3/2)^(0.5)) * e - (d^-1) * 11 * f/2 + (d^-1.5) * 5 * ((3/2)^(0.5)) * g/4 - 1/8
        if (a < 2 * ((3/8)^(0.5))) {
            c <- 0
        }
        c <- round(c, 11)
        lists <- append(lists, c)
        i <- i + 1
    }
    return(lists)
}

# ----------------------------------------------- Bartlet -----------------------------------------------
bartlett_transform_from_groups <- function(groups, var) {
    groups_length <- length(groups)
    lists <- list()
    i <- 1
    while (i <= groups_length) {
        lists <- append(lists, list(bartlett_transform_from_group(groups[[i]], var)))
        i <- i + 1
    }
    return(lists)
}

# Applying a Bartlett transformation to a data set with a variance of var after transformation
bartlett_transform_from_group <- function(group, var) {
    lists <- c()
    group_length <- length(group)
    i <- 1
    while (i <= group_length) {
        a <- group[[i]] + 0.5
        b <- a^0.5
        c <- b * 2 * (var^0.5)
        lists <- append(lists, c)
        i <- i + 1
    }
    return(lists)
}

# The inverse Bartlett transformation is applied simultaneously to multiple data sets, and the variance before transformation is var
inverse_bartlett_transform_from_groups <- function(groups, var) {
    groups_length <- length(groups)
    lists <- list()
    i <- 1
    while (i <= groups_length) {
        lists <- append(lists, list(inverse_bartlett_transform_from_group(groups[[i]], var)))
        i <- i + 1
    }
    return(lists)
}

# Applying an inverse Bartlett transformation to a dataset with a pre-transformation variance of var
inverse_bartlett_transform_from_group <- function(bt_data, var) {
    groups_length <- length(bt_data)
    i <- 1
    lists <- c()
    while (i <= groups_length) {
        a <- bt_data[[i]] * bt_data[[i]]
        b <- (2 * (var^0.5))^-2
        c <- b * a - 0.5
        c <- round(c, 11)
        lists <- append(lists, c)
        i <- i + 1
    }
    return(lists)
}

# ----------------------------------------------- Applying Bartlett transformation 2 bi=2*sqrt(yi) -----------------------------------------------
bartlett_transform2_from_groups <- function(groups, var) {
    groups_length <- length(groups)
    lists <- list()
    i <- 1
    while (i <= groups_length) {
        lists <- append(lists, list(bartlett_transform2_from_group(groups[[i]], var)))
        i <- i + 1
    }
    return(lists)
}


# Applying a Bartlett transformation 2 to a data set with a variance of var after transformation
bartlett_transform2_from_group <- function(group, var) {
    lists <- c()
    group_length <- length(group)
    i <- 1
    while (i <= group_length) {
        a <- group[[i]]
        b <- a^0.5
        c <- b * 2 * (var^0.5)
        lists <- append(lists, c)
        i <- i + 1
    }
    return(lists)
}


# ----------------------------------------------- The inverse Bartlett transformation 2 (bi^2)/4 -----------------------------------------------
inverse_bartlett_transform2_from_groups <- function(groups, var) {
    groups_length <- length(groups)
    lists <- list()
    i <- 1
    while (i <= groups_length) {
        lists <- append(lists, list(inverse_bartlett_transform2_from_group(groups[[i]], var)))
        i <- i + 1
    }
    return(lists)
}


# Applying an inverse Bartlett transformation 2 to a dataset with a pre-transformation variance of var
inverse_bartlett_transform2_from_group <- function(bt_data, var) {
    groups_length <- length(bt_data)
    i <- 1
    lists <- c()
    while (i <= groups_length) {
        a <- bt_data[[i]] * bt_data[[i]]
        b <- (2 * (var^0.5))^-2
        c <- b * a
        c <- round(c, 11)
        lists <- append(lists, c)
        i <- i + 1
    }
    return(lists)
}


# ----------------------------------------------- Fisz -----------------------------------------------
fisz_transform_from_groups <- function(scaling_coes, wavelet_coes, var) {
    groups_length <- length(scaling_coes)
    lists <- list()
    i <- 1
    while (i <= groups_length) {
        lists <- append(lists, list(fisz_transform_from_group(scaling_coes[[i]], wavelet_coes[[i]], var)))
        i <- i + 1
    }
    return(lists)
}

# Applying the Fisz transformation to a data set, the variance after transformation is var
fisz_transform_from_group <- function(scaling_coe, wavelet_coe, var) {
    lists <- list()
    group_length <- length(scaling_coe)
    j <- 1
    while (j <= group_length) {
        i <- 1
        level_length <- length(scaling_coe[[j]])
        coe_list <- c()
        while (i <= level_length) {
            if (scaling_coe[[j]][i] == 0) {
                coe_list <- append(coe_list, 0)
            } else {
                if (scaling_coe[[j]][i] < 0) {
                  print("fisz_transform_from_group")
                }
                coe_list <- append(coe_list, (var^0.5) * wavelet_coe[[j]][[i]]/(scaling_coe[[j]][i]^0.5))
            }
            i <- i + 1
        }
        lists <- append(lists, list(coe_list))
        j <- j + 1
    }
    return(lists)
}

# The inverse Fisz transformation is applied simultaneously to multiple data sets, and the variance before transformation is var
inverse_fisz_transform_from_groups <- function(scaling_coes, fisz_coes, var) {
    groups_length <- length(scaling_coes)
    c_list <- list()
    d_list <- list()
    lists <- list()
    i <- 1
    while (i <= groups_length) {
        a <- inverse_fisz_transform_from_group(scaling_coes[[i]], fisz_coes[[i]], var)
        c_list <- append(c_list, list(a[[1]]))
        d_list <- append(d_list, list(a[[2]]))
        i <- i + 1
    }

    lists <- append(lists, list(c_list))
    lists <- append(lists, list(d_list))
    return(lists)
}

# Apply the Fisz transformation to a data set with a variance of var before transformation
inverse_fisz_transform_from_group <- function(scaling_coe, fisz_coe, var) {
    lists <- list()
    c_lists <- list()
    d_lists <- list()
    group_length <- length(scaling_coe)
    j <- group_length
    while (j > 0) {
        level_length <- length(scaling_coe[[j]])
        d_list <- c()
        i <- 1
        while (i <= level_length) {
            d_list <- append(d_list, fisz_get_d(scaling_coe[[j]][i], fisz_coe[[j]][i], var))
            i <- i + 1
        }
        i <- 1
        while (i <= level_length && j > 1) {
            scaling_coe[[j - 1]][2 * i - 1] <- scaling_coe[[j]][i] + d_list[i]
            scaling_coe[[j - 1]][2 * i] <- scaling_coe[[j]][i] - d_list[i]
            if (scaling_coe[[j - 1]][2 * i - 1] < 0) {
                scaling_coe[[j - 1]][2 * i - 1] <- 0
            }
            if (scaling_coe[[j - 1]][2 * i] < 0) {
                scaling_coe[[j - 1]][2 * i] <- 0
            }
            i <- i + 1
        }
        d_lists <- append(d_lists, list(d_list))
        j <- j - 1
    }
    d_listx <- list()
    i <- group_length
    while (i >= 1) {
        d_listx <- append(d_listx, list(d_lists[[i]]))
        i <- i - 1
    }
    lists <- append(lists, list(scaling_coe))
    lists <- append(lists, list(d_listx))
    return(lists)
}
# Wavelet coefficients in Poisson space are calculated from scale coefficients and wavelet coefficients in Gaussian space
fisz_get_d <- function(c, f, var) {
    f <- f/(var^0.5)
    res <- f * (c^0.5)
    res <- round(res, 11)
    return(res)
}

# ----------------------------------------------- Freeman -----------------------------------------------
freeman_transform_from_groups <- function(groups, var) {
    groups_length <- length(groups)
    lists <- list()
    i <- 1
    while (i <= groups_length) {
        lists <- append(lists, list(freeman_transform_from_group(groups[[i]], var)))
        i <- i + 1
    }
    return(lists)
}

# Applying a Freeman transformation to a data set with a variance of var after transformation
freeman_transform_from_group <- function(group, var) {
    lists <- c()
    group_length <- length(group)
    i <- 1
    while (i <= group_length) {
        a <- group[[i]] + 1
        b <- a^0.5
        d <- group[[i]]
        e <- d^0.5
        c <- b * (var^0.5) + e * (var^0.5)
        lists <- append(lists, c)
        i <- i + 1
    }
    return(lists)
}

# The inverse Freeman transformation is applied simultaneously to multiple data sets, and the variance before transformation is var
inverse_freeman_transform_from_groups <- function(groups, var) {
    groups_length <- length(groups)
    lists <- list()
    i <- 1
    while (i <= groups_length) {
        lists <- append(lists, list(inverse_freeman_transform_from_group(groups[[i]], var)))
        i <- i + 1
    }
    return(lists)
}

# Applying an inverse Freeman transformation to a dataset with a pre-transformation variance of var
inverse_freeman_transform_from_group <- function(ft_data, var) {
    groups_length <- length(ft_data)
    i <- 1
    lists <- c()
    while (i <= groups_length) {
        a <- ft_data[[i]] * ft_data[[i]]
        b <- (2 * (var^0.5))^-2
        d <- a^(-1)
        e <- b^(-1)
        c <- b * a + e * d - 0.5
        c <- round(c, 11)
        lists <- append(lists, c)
        i <- i + 1
    }
    return(lists)
}
