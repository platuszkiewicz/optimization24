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
    
    return (grad)
}

grad_KP_TZ2 = function(x) {
    var <- Variables(x)
    grad <- c()

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
    
    return (grad)
}

grad_KP_TZ5 = function(x) {
    var <- Variables(x)
    grad <- c()
    
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
    
    return (grad)
}

ZAKUP_FACTOR <- 1.2
ODDANIE_FACTOR <- 0.98

KE = function(x) {
    var <- Variables(x)
    result <- c()
    PELec <- (PEL_TZ1(x) + PEL_TZ2(x) + PEL_TZ5(x))

    for (i in 1:24) {
        PELzakup <- 0 
        PELoddanie <- 0

        cEEzakup <- (c_RDN[i] * ZAKUP_FACTOR)
        cEEoddanie <- (c_RDN[i] * ODDANIE_FACTOR)

        if (PELec[i] > zap_el[i]) { # oddanie
            PELoddanie = PELec[i] - zap_el[i]
            PELzakup = 0
        } else {                    # zakup
            PELoddanie = 0 
            PELzakup = zap_el[i] - PELec[i]
        }

        result<- c(result, (PELzakup*cEEzakup) - (PELoddanie*cEEoddanie))
    }

    return(result)
}

grad_KE = function(x) {

    PELec <- (PEL_TZ1(x) + PEL_TZ2(x) + PEL_TZ5(x))

    grad <- (grad_PEL_TZ1(x) + grad_PEL_TZ2(x) + grad_PEL_TZ5(x))

    for (i in 1:(24 * X_LENGTH)) {
        h <- (i %% 24)
        if (h == 0) {
            h = 24
        }

        v <- (i %/% 24) + 1
        if (i %% 24 == 0) {
            v <- (v - 1)
        }

        cEEzakup <- c_RDN[h] * ZAKUP_FACTOR
        cEEoddanie <- c_RDN[h] * ODDANIE_FACTOR

        if (PELec[h] > zap_el[h]) {
            grad[i] <- -(cEEoddanie * grad[i])
        } else {
            grad[i] <- -(cEEzakup * grad[i])
        }
    }

    return(grad)
}