#####################################
### Turbines - charakterystyki turbin
#####################################

PEL_TZ1 = function(x) {
    var <- Variables(x)

    PEL <- c()
    for (i in 1:24) {
        D_I = var@mST_TZ1_in[i]
        D_II = (var@mST_TZ1_in[i] - var@mST_TZ1_up25[i])
        D_III = (var@mST_TZ1_in[i] - var@mST_TZ1_up25[i] - var@mST_TZ1_up13[i])

        # Uwaga. Usuniecie max() gwarantuje zgodnosc pochodnej
        PEL_I = c(0.0386 * D_I + 3.2269)
        PEL_II = c(-0.0003419 * D_II * D_II + 0.1941 * D_II - 20.497)
        PEL_III = c(0.000806 * D_III * D_III - 0.22816 * D_III + 22.80545)

        PEL <- c(PEL, PEL_I + PEL_II + PEL_III)
    }

    # nowy model
    #for (i in 1:24) {
        ##PEL <- c(PEL, 0.1532344 * var@mST_TZ1_in[i] - 0.0682228 * var@mST_TZ1_up25[i] - 0.0353944 * var@mST_TZ1_up13[i] - 6.3862641)
        #PEL <- c(PEL, 0.1532344 * var@mST_TZ1_in[i] - 0.0682228 * var@mST_TZ1_up25[i] - 0.035394 * var@mST_TZ1_up13[i])
    #}

    return (PEL)
}

PEL_TZ2 = function(x) {
    var <- Variables(x)

    PEL <- c()
    for (i in 1:24) {
        D_I = var@mST_TZ2_in[i]
        D_II = var@mST_TZ2_in[i] - var@mST_TZ2_up25[i]
        D_III = var@mST_TZ2_in[i] - var@mST_TZ2_up25[i] - var@mST_TZ2_up13[i]
        D_IV = var@mST_TZ2_in[i] - var@mST_TZ2_up25[i] - var@mST_TZ2_up13[i] - var@mST_TZ2_up06[i]

        PEL_I = c(0.0466 * D_I + 4.4824)
        PEL_II = c(0.1015 * D_II - 10.569)
        PEL_III = c(0.00005 * D_III * D_III - 0.0064 * D_III + 0.6309)
        PEL_IV = c(0.1673 * D_IV - 2.1814)

        PEL <- c(PEL, PEL_I + PEL_II + PEL_III+PEL_IV)
    }

    return (PEL)
}

PEL_TZ5 = function(x) {
    var <- Variables(x)

    PEL <- c()
    for (i in 1:24) {
        D_I = var@mST_TZ5_in[i]
        D_II = var@mST_TZ5_in[i] - var@mST_TZ5_up25[i]
        D_III = var@mST_TZ5_in[i] - var@mST_TZ5_up25[i] - var@mST_TZ5_up13[i]
        D_IV = var@mST_TZ5_in[i] - var@mST_TZ5_up25[i] - var@mST_TZ5_up13[i] - var@mST_TZ5_up06[i]

        PEL_I = c(0.0551 * D_I - 0.1043)
        PEL_II = c(0.1072 * D_II - 11.004)
        PEL_III = c(0.00006 * D_III * D_III - 0.424)
        PEL_IV = c(-0.0007 * D_IV * D_IV + 0.3306 * D_IV - 10.078)

        PEL <- c(PEL, PEL_I + PEL_II + PEL_III + PEL_IV)
    }

    return(PEL)
}

## ## ## ## ## ## ## ## ## Pochodne

grad_PEL_TZ1 = function(x) {
    #PEL_I = c(0.0386 * D_I + 3.2269)
    #PEL_II = c(-0.0003419 * D_II * D_II + 0.1941 * D_II - 20.497)
    #PEL_III = c(0.000806 * D_III * D_III - 0.22816 * D_III + 22.80545)
    var <- Variables(x)

    grad <- c()
    for (i in 1:24) {
        grad <- c(grad, c(
           0.00454 - 0.0009282 * var@mST_TZ1_up25[i] - 0.001612 * var@mST_TZ1_up13[i] + 0.0009282 * var@mST_TZ1_in[i],
           0.03406 - 0.0009282 * var@mST_TZ1_in[i] + 0.001612 * var@mST_TZ1_up13[i] + 0.0009282 * var@mST_TZ1_up25[i],
           0.22816 - 0.001612 * var@mST_TZ1_in[i] + 0.001612 * var@mST_TZ1_up25[i] + 0.001612 * var@mST_TZ1_up13[i],
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
           ))
    }

    for (i in 1:24) {
        grad[(24*0+0)+1*i] = 0.00454 - 0.0009282 * var@mST_TZ1_up25[i] - 0.001612 * var@mST_TZ1_up13[i] + 0.0009282 * var@mST_TZ1_in[i]
        grad[(24*1+0)+1*i] = 0.03406 - 0.0009282 * var@mST_TZ1_in[i] + 0.001612 * var@mST_TZ1_up13[i] + 0.0009282 * var@mST_TZ1_up25[i]
        grad[(24*2+0)+1*i] = 0.22816 - 0.001612 * var@mST_TZ1_in[i] + 0.001612 * var@mST_TZ1_up25[i] + 0.001612 * var@mST_TZ1_up13[i]
        grad[(24*3+0)+1*i] = 0
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
    }

    #nowy model
    #for (i in 1:24) {
        #grad[(24 * 0 + 0) + 1 * i] = 0.1532344
        #grad[(24*1+0)+1*i] = - 0.0682228
        #grad[(24 * 2 + 0) + 1 * i] =- 0.035394
        #grad[(24*3+0)+1*i] = 0
        #grad[(24*4+0)+1*i] = 0
        #grad[(24*5+0)+1*i] = 0
        #grad[(24*6+0)+1*i] = 0
        #grad[(24*7+0)+1*i] = 0
        #grad[(24*8+0)+1*i] = 0
        #grad[(24*9+0)+1*i] = 0
        #grad[(24*10+0)+1*i] = 0
        #grad[(24*11+0)+1*i] = 0
        #grad[(24*12+0)+1*i] = 0
        #grad[(24*13+0)+1*i] = 0
    #}
    return (grad)
}

grad_PEL_TZ2 = function(x) {
    var <- Variables(x)

    grad <- c()
    for (i in 1:24) {
        grad <- c(grad, c(
           0, 0, 0, 0,
           0.309 - 0.0001 * var@mST_TZ2_up25[i] - 0.0001 * var@mST_TZ2_up13[i] + 0.0001 * var@mST_TZ2_in[i],
           -0.2624 + 0.0001 * var@mST_TZ2_up25[i] + 0.0001 * var@mST_TZ2_up13[i] - 0.0001 * var@mST_TZ2_in[i],
           -0.1609 + 0.0001 * var@mST_TZ2_up25[i] + 0.0001 * var@mST_TZ2_up13[i] - 0.0001 * var@mST_TZ2_in[i],
           -0.1673,
           0, 0, 0, 0, 0, 0))
    }

    for (i in 1:24)
    {
        grad[(24 * 0 + 0) + 1 * i] = 0
        grad[(24 * 1 + 0) + 1 * i] = 0
        grad[(24 * 2 + 0) + 1 * i] = 0
        grad[(24 * 3 + 0) + 1 * i] = 0
        grad[(24 * 4 + 0) + 1 * i] = 0.309 - 0.0001 * var@mST_TZ2_up25[i] - 0.0001 * var@mST_TZ2_up13[i] + 0.0001 * var@mST_TZ2_in[i]
        grad[(24 * 5 + 0) + 1 * i] = -0.2624 + 0.0001 * var@mST_TZ2_up25[i] + 0.0001 * var@mST_TZ2_up13[i] - 0.0001 * var@mST_TZ2_in[i]
        grad[(24 * 6 + 0) + 1 * i] = -0.1609 + 0.0001 * var@mST_TZ2_up25[i] + 0.0001 * var@mST_TZ2_up13[i] - 0.0001 * var@mST_TZ2_in[i]
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

grad_PEL_TZ5 = function(x) {
    var <- Variables(x)

    grad <- c()
    for (i in 1:24) {
        dx0 = 0.1623 + 0.00012 * var@mST_TZ5_in[i] - 0.00012 * var@mST_TZ5_up25[i] - 0.00012 * var@mST_TZ5_up13[i] + 0.3306 - 0.0014 * var@mST_TZ5_in[i] + 0.0014 * var@mST_TZ5_up25[i] + 0.0014 * var@mST_TZ5_up13[i] + 0.0014 * var@mST_TZ5_up06[i]
        dx1 = -0.1072 + 0.00012 * var@mST_TZ5_up25[i] - 0.00012 * var@mST_TZ5_in[i] + 0.00012 * var@mST_TZ5_up13[i] - 0.3306 - 0.0014 * var@mST_TZ5_up25[i] + 0.0014 * var@mST_TZ5_in[i] - 0.0014 * var@mST_TZ5_up13[i] - 0.0014 * var@mST_TZ5_up06[i]
        dx2 = 0.00012 * var@mST_TZ5_up13[i] - 0.00012 * var@mST_TZ5_in[i] + 0.00012 * var@mST_TZ5_up25[i] - 0.3306 - 0.0014 * var@mST_TZ5_up13[i] + 0.0014 * var@mST_TZ5_in[i] - 0.0014 * var@mST_TZ5_up25[i] - 0.0014 * var@mST_TZ5_up06[i]
        dx3 = -0.3306 - 0.0014 * var@mST_TZ5_up06[i] + 0.0014 * var@mST_TZ5_in[i] - 0.0014 * var@mST_TZ5_up25[i] - 0.0014 * var@mST_TZ5_up13[i]

        grad <- c(grad, c(0, 0, 0, 0, 0, 0, 0, 0, 0, dx0, dx1, dx2, dx3, 0))
    }

    for (i in 1:24) {
        dx0 = 0.1623 + 0.00012 * var@mST_TZ5_in[i] - 0.00012 * var@mST_TZ5_up25[i] - 0.00012 * var@mST_TZ5_up13[i] + 0.3306 - 0.0014 * var@mST_TZ5_in[i] + 0.0014 * var@mST_TZ5_up25[i] + 0.0014 * var@mST_TZ5_up13[i] + 0.0014 * var@mST_TZ5_up06[i]
        dx1 = -0.1072 + 0.00012 * var@mST_TZ5_up25[i] - 0.00012 * var@mST_TZ5_in[i] + 0.00012 * var@mST_TZ5_up13[i] - 0.3306 - 0.0014 * var@mST_TZ5_up25[i] + 0.0014 * var@mST_TZ5_in[i] - 0.0014 * var@mST_TZ5_up13[i] - 0.0014 * var@mST_TZ5_up06[i]
        dx2 = 0.00012 * var@mST_TZ5_up13[i] - 0.00012 * var@mST_TZ5_in[i] + 0.00012 * var@mST_TZ5_up25[i] - 0.3306 - 0.0014 * var@mST_TZ5_up13[i] + 0.0014 * var@mST_TZ5_in[i] - 0.0014 * var@mST_TZ5_up25[i] - 0.0014 * var@mST_TZ5_up06[i]
        dx3 = -0.3306 - 0.0014 * var@mST_TZ5_up06[i] + 0.0014 * var@mST_TZ5_in[i] - 0.0014 * var@mST_TZ5_up25[i] - 0.0014 * var@mST_TZ5_up13[i]

        grad[(24 * 0 + 0) + 1 * i] = 0
        grad[(24 * 1 + 0) + 1 * i] = 0
        grad[(24 * 2 + 0) + 1 * i] = 0
        grad[(24 * 3 + 0) + 1 * i] = 0
        grad[(24 * 4 + 0) + 1 * i] = 0
        grad[(24 * 5 + 0) + 1 * i] = 0
        grad[(24 * 6 + 0) + 1 * i] = 0
        grad[(24 * 7 + 0) + 1 * i] = 0
        grad[(24 * 8 + 0) + 1 * i] = 0
        grad[(24 * 9 + 0) + 1 * i] = dx0
        grad[(24 * 10 + 0) + 1 * i] = dx1
        grad[(24 * 11 + 0) + 1 * i] = dx2
        grad[(24 * 12 + 0) + 1 * i] = dx3
        grad[(24 * 13 + 0) + 1 * i] = 0
    }

    return (grad)
}