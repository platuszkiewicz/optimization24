# koszty paliwa
cFU1_K7 = c(rep(250, times = 24))
cFU2_K7 = c(rep(235, times = 24))

cEN_BIO  = 4 # koszty środowiskowe [PLN / tona biomasy]
cWA_DEMI = 3 # koszty wody DEMI    [PLN / tona wody]

KP_TZ1 = function(x) {
    var <- Variables(x)
    result <- c()

    for (i in 1:24) {
        if (configEC@TZ1[i] == TRUE) {
            mFU1_K7[i] <<- (0.0227 * var@mST_TZ1_in[i] + 3.7972) * 3.6 # t/h
            mFU2_K7[i] <<- (0.0568 * var@mST_TZ1_in[i] - 8.1435) * 3.6 # t/h
            result <- c(result, cFU1_K7[i] * mFU1_K7[i] + cFU2_K7[i] * mFU2_K7[i] + cEN_BIO * (mFU1_K7[i] + mFU2_K7[i]) + cWA_DEMI * var@mST_TZ1_in[i])
        } else {
            result <- c(result,0)
        }
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

KP_TZ4 = function(x) {
    var <- Variables(x)
    result <- c()

    for (i in 1:24) {
        result <- c(result, steamCostTZ4[i] * var@mST_TZ4_in[i])
    }

    return(result)
}

KP_TZ5 = function(x) {
    var <- Variables(x)
    result <- c()

    for (i in 1:24) {
        result <- c(result, steamCostTZ5[i] * mST_KS4[i]) #var@mST_TZ5_in[i])
    }

    return(result)
}

grad_KP_TZ1 = function(x) {
    var <- Variables(x)
    grad <- c()

    if (configEC@TZ1[i] == TRUE) {
        for (i in 1:24) {
            grad[(24 * 0 + 0) + 1 * i] = cFU1_K7[i] * (0.0227) * 3.6 + cFU2_K7[i] * (0.0568) * 3.6 + cEN_BIO * (0.0227 + 0.0568) * 3.6 + cWA_DEMI
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
            grad[(24 * 14 + 0) + 1 * i] = 0
            grad[(24 * 15 + 0) + 1 * i] = 0
            grad[(24 * 16 + 0) + 1 * i] = 0
            grad[(24 * 17 + 0) + 1 * i] = 0
            grad[(24 * 18 + 0) + 1 * i] = 0
            grad[(24 * 19 + 0) + 1 * i] = 0
            grad[(24 * 20 + 0) + 1 * i] = 0
        }
    } else {
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
            grad[(24 * 9 + 0) + 1 * i] = 0
            grad[(24 * 10 + 0) + 1 * i] = 0
            grad[(24 * 11 + 0) + 1 * i] = 0
            grad[(24 * 12 + 0) + 1 * i] = 0
            grad[(24 * 13 + 0) + 1 * i] = 0
            grad[(24 * 14 + 0) + 1 * i] = 0
            grad[(24 * 15 + 0) + 1 * i] = 0
            grad[(24 * 16 + 0) + 1 * i] = 0
            grad[(24 * 17 + 0) + 1 * i] = 0
            grad[(24 * 18 + 0) + 1 * i] = 0
            grad[(24 * 19 + 0) + 1 * i] = 0
            grad[(24 * 20 + 0) + 1 * i] = 0
        }
    }
    
    return (grad)
}

grad_KP_TZ4 = function(x) {
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
        grad[(24 * 9 + 0) + 1 * i] = 0
        grad[(24 * 10 + 0) + 1 * i] = steamCostTZ4[i]
        grad[(24 * 11 + 0) + 1 * i] = 0
        grad[(24 * 12 + 0) + 1 * i] = 0
        grad[(24 * 13 + 0) + 1 * i] = 0
        grad[(24 * 14 + 0) + 1 * i] = 0
        grad[(24 * 15 + 0) + 1 * i] = 0
        grad[(24 * 16 + 0) + 1 * i] = 0
        grad[(24 * 17 + 0) + 1 * i] = 0
        grad[(24 * 18 + 0) + 1 * i] = 0
        grad[(24 * 19 + 0) + 1 * i] = 0
        grad[(24 * 20 + 0) + 1 * i] = 0
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
        grad[(24 * 4 + 0) + 1 * i] = 0
        grad[(24 * 5 + 0) + 1 * i] = steamCostTZ2[i]
        grad[(24 * 6 + 0) + 1 * i] = 0
        grad[(24 * 7 + 0) + 1 * i] = 0
        grad[(24 * 8 + 0) + 1 * i] = 0
        grad[(24 * 9 + 0) + 1 * i] = 0
        grad[(24 * 10 + 0) + 1 * i] = 0
        grad[(24 * 11 + 0) + 1 * i] = 0
        grad[(24 * 12 + 0) + 1 * i] = 0
        grad[(24 * 13 + 0) + 1 * i] = 0
        grad[(24 * 14 + 0) + 1 * i] = 0
        grad[(24 * 15 + 0) + 1 * i] = 0
        grad[(24 * 16 + 0) + 1 * i] = 0
        grad[(24 * 17 + 0) + 1 * i] = 0
        grad[(24 * 18 + 0) + 1 * i] = 0
        grad[(24 * 19 + 0) + 1 * i] = 0
        grad[(24 * 20 + 0) + 1 * i] = 0
    }

    return(grad)
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
        grad[(24 * 9 + 0) + 1 * i] = 0
        grad[(24 * 10 + 0) + 1 * i] = 0 #steamCostTZ5[i]
        grad[(24 * 11 + 0) + 1 * i] = 0
        grad[(24 * 12 + 0) + 1 * i] = 0
        grad[(24 * 13 + 0) + 1 * i] = 0
        grad[(24 * 14 + 0) + 1 * i] = 0
        grad[(24 * 15 + 0) + 1 * i] = 0
        grad[(24 * 16 + 0) + 1 * i] = 0
        grad[(24 * 17 + 0) + 1 * i] = 0
        grad[(24 * 18 + 0) + 1 * i] = 0
        grad[(24 * 19 + 0) + 1 * i] = 0
        grad[(24 * 20 + 0) + 1 * i] = 0
    }
    
    return (grad)
}

ZAKUP_FACTOR <- 1.00
ODDANIE_FACTOR <- 0.98

cEE_DISTR <- 30  # PLN / MWh
cCER_GREEN <- 65 # PLN / MWh

#cEE_DISTR <- 0  # PLN / MWh
#cCER_GREEN <- 0 # PLN / MWh

KE = function(x) {
    var <- Variables(x)
    result <- c()
    PELec <- (PEL_TZ1(x) + PEL_TZ2(x) + PEL_TZ4(x) + PEL_TZ5(x))

    for (i in 1:24) {
        PELzakup <- 0 
        PELoddanie <- 0

        cEEzakup <- (c_RDN[i] * ZAKUP_FACTOR) + cEE_DISTR
        cEEoddanie <- (c_RDN[i] * ODDANIE_FACTOR) + cCER_GREEN

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

    PELec <- (PEL_TZ1(x) + PEL_TZ2(x) + PEL_TZ4(x) + PEL_TZ5(x))

    grad <- (grad_PEL_TZ1(x) + grad_PEL_TZ2(x) + grad_PEL_TZ4(x) + grad_PEL_TZ5(x))

    for (i in 1:(24 * X_LENGTH)) {
        h <- (i %% 24)
        if (h == 0) {
            h = 24
        }

        v <- (i %/% 24) + 1
        if (i %% 24 == 0) {
            v <- (v - 1)
        }

        cEEzakup <- c_RDN[h] * ZAKUP_FACTOR + cEE_DISTR
        cEEoddanie <- c_RDN[h] * ODDANIE_FACTOR + cCER_GREEN

        if (PELec[h] > zap_el[h]) {
            grad[i] <- -(cEEoddanie * grad[i])
        } else {
            grad[i] <- -(cEEzakup * grad[i])
        }
    }

    return(grad)
}