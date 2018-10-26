###########################################
### IterFunc - wywo³ywana za ka¿d¹ iteracj¹
###########################################
iterCounter <- 0

iterFunc <- function(x) {
    var <- Variables(x)
    iterCounter <<- iterCounter + 1
    cat("\nIteration #", iterCounter)
    
    grad_mST_K7 <- c()
    grad_mST_K1 <- c()
    grad_mST_K6 <- c()
    grad_mST_K4 <- c()
    grad_mST_K5 <- c()
    grad_mST_KOTLY_EC<- c()
    
    for (i in 1:24) {
        grad_mST_K7[[i]]= c(rep(0, times = X_LENGTH))
        grad_mST_K1[[i]]= c(rep(0, times = X_LENGTH))
        grad_mST_K6[[i]]= c(rep(0, times = X_LENGTH))
        grad_mST_K4[[i]]= c(rep(0, times = X_LENGTH))
        grad_mST_K5[[i]]= c(rep(0, times = X_LENGTH))

    }

    ####################################### Aktualizuj wydajnoœæ kot³ów #######################################

    for (i in 1:24) {
        mST_K7[i] <<- var@mST_TZ1_in[i] + mST_SRS_TZ1[i]

        grad_mST_K7[[i]] = c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

        # TODO: mo¿na uproscic wykonywanie?
        if (var@mST_TZ2_in[i] + mST_SRS_TZ2_13[i] + mST_SRS_TZ2_06[i] > mST_K6.max) {
            mST_K6[i] <<- mST_K6.max
            grad_mST_K6[[i]] = c(rep(0,times=X_LENGTH))
            mST_K1[i] <<- var@mST_TZ2_in[i] + mST_SRS_TZ2_13[i] + mST_SRS_TZ2_06[i] - mST_K6[i]
            grad_mST_K1[[i]] = c(0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
        } else {
            # TODO: throw exception
        }

        if (var@mST_TZ4_in[i] + mST_SRS_TZ4[i] > mST_K5.max) {
            mST_K5[i] <<- mST_K5.max
            grad_mST_K5[[i]] = c(rep(0, times = X_LENGTH))
            mST_K4[i] <<- var@mST_TZ4_in[i] + mST_SRS_TZ4[i] - mST_K5[i]
            grad_mST_K4[[i]] = c(0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0)
        } else {
            # TODO: throw exception
        }
    }

    ############################################ Potrzeby w³asne ############################################

    ### Potrzeby w³asne - 6 bar

    mST_KOTLY_EC <-  mST_K7 + mST_K1 + mST_K6 + mST_K4 + mST_K5
    for (i in 1:24) {
        grad_mST_KOTLY_EC[[i]] <- grad_mST_K7[[i]] + grad_mST_K1[[i]] + grad_mST_K6[[i]] + grad_mST_K4[[i]] + grad_mST_K5[[i]]
    }

    mST_06_ODG_POD <- 0.0835 * mST_KOTLY_EC + 2.4172
    grad_mST_06_ODG_POD <- c()
    for (i in 1:24) {
        grad_mST_06_ODG_POD[[i]] = 0.0835 * grad_mST_KOTLY_EC[[i]]
    }

    mST_06_ODG_WST = 0.4131 * mST_06_ODG_POD + 0.144 * var@mST_TZ2_kond - 8.4461
    grad <- c()
    for (i in 1:24) {
        grad[[i]] = c(0,0,0,0,0,0,0,0,0,0.144,0,0,0,0,0,0,0,0,0,0,0)
    }
    grad_mST_06_ODG_WST <- c()
    for (i in 1:24) {
        grad_mST_06_ODG_WST[[i]] = 0.4131 * 0.0835 * grad_mST_KOTLY_EC[[i]] + grad[[i]]
    }

    ### Potrzeby w³asne - 13 bar i 25 bar (TG-2 i TG-4)

    mST_25_TG <- c(rep(0, times = 24))
    mST_13_TG <- c(rep(0, times = 24))
    grad_mST_25_TG <- c()
    grad_mST_13_TG <- c()
    for (i in 1:24) {
        if (configEC@K7[i] == TRUE && configEC@K1[i] == TRUE && configEC@K6[i] == TRUE && configEC@K4[i] == TRUE && configEC@K5[i] == TRUE) {
            mST_25_TG[i] <- 0.5 *( 0.0199 * mST_KOTLY_EC[i] + 19.972)
            mST_13_TG[i] <- 0.5 *( 0.0199 * mST_KOTLY_EC[i] + 19.972)

            grad_mST_25_TG[[i]] = c(0.5 * 0.0199,
             0,
             0,
             0,
             0,
             0.5 * 0.0199 + 0.5 * 0.0199,
             0,
             0,
             0,
             0,
             0.5 * 0.0199 + 0.5 * 0.0199,
             0,
             0,
             0,
             0,
             0,
             0,
             0,
             0,
             0,
             0)

            grad_mST_13_TG[[i]] = c(0.5 * 0.0199,
             0,
             0,
             0,
             0,
             0.5 * 0.0199 + 0.5 * 0.0199,
             0,
             0,
             0,
             0,
             0.5 * 0.0199 + 0.5 * 0.0199,
             0,
             0,
             0,
             0,
             0,
             0,
             0,
             0,
             0,
             0)

        } else if (configEC@K7[i] == TRUE && configEC@K1[i] == TRUE && configEC@K6[i] == TRUE && configEC@K4[i] == FALSE && configEC@K5[i] == FALSE) {
            mST_25_TG[i] <- 0.5 *( 0.0141 * mST_KOTLY_EC[i] + 23.28)
            mST_13_TG[i] <- 0.5 *( 0.0141 * mST_KOTLY_EC[i] + 23.28)

            grad_mST_25_TG[[i]] = c(0*0.5 * 0.0141,
             0,
             0,
             0,
             0,
             0*(0.5 * 0.0141 + 0.5 * 0.0141),                
             0,
             0,
             0,
             0,
             0*(0.5 * 0.0141 + 0.5 * 0.0141),
             0,
             0,
             0,
             0,
             0,
             0,
             0,
             0,
             0,
             0)

            grad_mST_13_TG[[i]] = c(0*0.5 * 0.0141,
             0,
             0,
             0,
             0,
             0*(0.5 * 0.0141 + 0.5 * 0.0141),
             0,
             0,
             0,
             0,
             0*(0.5 * 0.0141 + 0.5 * 0.0141),
             0,
             0,
             0,
             0,
             0,
             0,
             0,
             0,
             0,
             0)
        }
    }

    PWP_EC_25 <<- mST_25_TG
    grad_PWP_EC_25 <<- grad_mST_25_TG
    PWP_EC_13 <<- mST_13_TG
    grad_PWP_EC_13 <<- grad_mST_13_TG
    PWP_EC_06 <<- mST_06_ODG_POD + mST_06_ODG_WST
    grad_PWP_EC_06 <<- c()
    for (i in 1:24) {
        grad_PWP_EC_06[[i]] <<- grad_mST_06_ODG_POD[[i]] + grad_mST_06_ODG_WST[[i]]
    }

    cat("\nPWP_EC_25: ", sprintf("%1.1f", round(PWP_EC_25, digits = 1)), 
        "\nPWP_EC_13: ", sprintf("%1.1f", round(PWP_EC_13, digits = 1)),
        "\nPWP_EC_06: ", sprintf("%1.1f", round(PWP_EC_06, digits = 1)))
}