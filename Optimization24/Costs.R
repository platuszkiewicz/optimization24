KP_TZ1 = function(x) {
    var <- Variables(x)
    result <- c()

    for (i in 1:24) {
        result <- c( result, steamCostTZ1[i] * var@mST_TZ1_in[i])
    }

    return (result)
}


KP_TZ2 = function(x) {
    var <- Variables(x)
    result <- c()

    for (i in 1:24) {
        result <- c(result, steamCostTZ2[i] * var@mST_TZ2_in[i])
    }

    return(result)
}

KP_TZ5 = function(x) {
    var <- Variables(x)
    result <- c()

    for (i in 1:24) {
        result <- c(result, steamCostTZ5[i] * var@mST_TZ5_in[i])
    }

    return(result)
}

grad_KP_TZ1 = function(x) {
    var <- Variables(x)
    grad <- c()

    for (i in 1:24) {
        for (i in 1:24) {
            grad[(24 * 0 + 0) + 1 * i] = steamCostTZ1[i]
            grad[(24 * 1 + 0) + 1 * i] = 0
            grad[(24 * 2 + 0) + 1 * i] = 0
            grad[(24 * 3 + 0) + 1 * i] = 0
            grad[(24 * 4 + 0) + 1 * i] = 0
            grad[(24 * 5 + 0) + 1 * i] = 0
            grad[(24 * 6 + 0) + 1 * i] = 0
            grad[(24 * 7 + 0) + 1 * i] = 0
            grad[(24 * 8 + 0) + 1 * i] = 0
            grad[(24 * 9 + 0) + 1 * i] = 0
            grad[(24 * 10 + 0) + 1 * i] = 0
            grad[(24 * 11 + 0) + 1 * i] = 0
            grad[(24 * 12 + 0) + 1 * i] = 0
            grad[(24 * 13 + 0) + 1 * i] = 0
        }
    }

    return (grad)
}

grad_KP_TZ2 = function(x) {
    var <- Variables(x)
    grad <- c()

    for (i in 1:24) {
        for (i in 1:24) {
            grad[(24 * 0 + 0) + 1 * i] = 0
            grad[(24 * 1 + 0) + 1 * i] = 0
            grad[(24 * 2 + 0) + 1 * i] = 0
            grad[(24 * 3 + 0) + 1 * i] = 0
            grad[(24 * 4 + 0) + 1 * i] = steamCostTZ2[i]
            grad[(24 * 5 + 0) + 1 * i] = 0
            grad[(24 * 6 + 0) + 1 * i] = 0
            grad[(24 * 7 + 0) + 1 * i] = 0
            grad[(24 * 8 + 0) + 1 * i] = 0
            grad[(24 * 9 + 0) + 1 * i] = 0
            grad[(24 * 10 + 0) + 1 * i] = 0
            grad[(24 * 11 + 0) + 1 * i] = 0
            grad[(24 * 12 + 0) + 1 * i] = 0
            grad[(24 * 13 + 0) + 1 * i] = 0
        }
    }

    return (grad)
}

grad_KP_TZ5 = function(x) {
    var <- Variables(x)
    grad <- c()

    for (i in 1:24) {
        for (i in 1:24) {
            grad[(24 * 0 + 0) + 1 * i] = 0
            grad[(24 * 1 + 0) + 1 * i] = 0
            grad[(24 * 2 + 0) + 1 * i] = 0
            grad[(24 * 3 + 0) + 1 * i] = 0
            grad[(24 * 4 + 0) + 1 * i] = 0
            grad[(24 * 5 + 0) + 1 * i] = 0
            grad[(24 * 6 + 0) + 1 * i] = 0
            grad[(24 * 7 + 0) + 1 * i] = 0
            grad[(24 * 8 + 0) + 1 * i] = 0
            grad[(24 * 9 + 0) + 1 * i] = steamCostTZ5[i]
            grad[(24 * 10 + 0) + 1 * i] = 0
            grad[(24 * 11 + 0) + 1 * i] = 0
            grad[(24 * 12 + 0) + 1 * i] = 0
            grad[(24 * 13 + 0) + 1 * i] = 0
        }
    }

    return (grad)
}