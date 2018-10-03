############################################
### Turbines - charakterystyki turbin (nowe)
############################################

PEL_TZ1 = function(x) {
    var <- Variables(x)

    PEL <- c()
    for (i in 1:24) {
        PEL <- c(PEL, -6.072319 +
        0.105464 * var@mST_TZ1_in[i] +
        - 0.018970 * var@mST_TZ1_up25[i] +
        0.012165 * var@mST_TZ1_up13[i] +
        0.044843  * var@mST_TZ1_up06[i])
    }

    return (PEL)
}

PEL_TZ2 = function(x) {
    var <- Variables(x)
    pST_k <- 0.03
    tWA_kin <- 20

    PEL <- c()
    for (i in 1:24) {
        PEL <- c(PEL, -4.0995420 +
        0.1201080 * var@mST_TZ2_in[i] +
        - 0.0482238* var@mST_TZ2_up25[i] +
        - 0.0052286* var@mST_TZ2_up13[i] +
        0.0241272* var@mST_TZ2_up06[i] +
        0.1811973* var@mST_TZ2_kond[i] +
        9.5356414* pST_k +
        - 0.0453943* tWA_kin)
    }

    return (PEL)
}

PEL_TZ5 = function(x) {
    var <- Variables(x)
    pST_k <- 0.06
    tWA_kin <- 20

    PEL <- c()
    for (i in 1:24) {
        PEL <- c(PEL, -7.127007 +
        0.259987 * var@mST_TZ5_in[i] +
        - 0.156107 * var@mST_TZ5_up25[i] +
        - 0.123528 * var@mST_TZ5_up13[i] +
        - 0.074769 * var@mST_TZ5_up06[i] +
        0.063627 * var@mST_TZ5_kond[i] +
        - 46.844909 * pST_k +
        - 0.011842 * tWA_kin)
    }

    return(PEL)
}

## ## ## ## ## ## ## ## ## Pochodne

grad_PEL_TZ1 = function(x) {
    grad <- c()

    for (i in 1:24) {
        grad[(24 * 0 + 0) + 1 * i] = 0.105464
        grad[(24 * 1 + 0) + 1 * i] = - 0.018970
        grad[(24 * 2 + 0) + 1 * i] = 0.012165
        grad[(24 * 3 + 0) + 1 * i] = 0.044843
        grad[(24*4+0)+1*i] = 0
        grad[(24*5+0)+1*i] = 0
        grad[(24*6+0)+1*i] = 0
        grad[(24*7+0)+1*i] = 0
        grad[(24*8+0)+1*i] = 0
        grad[(24*9+0)+1*i] = 0
        grad[(24*10+0)+1*i] = 0
        grad[(24*11+0)+1*i] = 0
        grad[(24*12+0)+1*i] = 0
        grad[(24*13+0)+1*i] = 0
        grad[(24*14+0)+1*i] = 0
    }

    return (grad)
}

grad_PEL_TZ2 = function(x) {
    grad <- c()

    for (i in 1:24)
    {
        grad[(24 * 0 + 0) + 1 * i] = 0
        grad[(24 * 1 + 0) + 1 * i] = 0
        grad[(24 * 2 + 0) + 1 * i] = 0
        grad[(24 * 3 + 0) + 1 * i] = 0
        grad[(24 * 4 + 0) + 1 * i] = 0.1201080
        grad[(24 * 5 + 0) + 1 * i] = - 0.0482238
        grad[(24 * 6 + 0) + 1 * i] = - 0.0052286
        grad[(24 * 7 + 0) + 1 * i] = 0.0241272
        grad[(24 * 8 + 0) + 1 * i] = 0.1811973
        grad[(24 * 9 + 0) + 1 * i] = 0
        grad[(24 * 10 + 0) + 1 * i] = 0
        grad[(24 * 11 + 0) + 1 * i] = 0
        grad[(24 * 12 + 0) + 1 * i] = 0
        grad[(24 * 13 + 0) + 1 * i] = 0
        grad[(24 * 14 + 0) + 1 * i] = 0
    }

    return (grad)
}

grad_PEL_TZ5 = function(x) {
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
        grad[(24 * 9 + 0) + 1 * i] = 0.259987
        grad[(24 * 10 + 0) + 1 * i] = - 0.156107
        grad[(24 * 11 + 0) + 1 * i] = - 0.123528
        grad[(24 * 12 + 0) + 1 * i] = - 0.074769
        grad[(24 * 13 + 0) + 1 * i] = 0.063627
        grad[(24 * 14 + 0) + 1 * i] = 0
    }

    return (grad)
}