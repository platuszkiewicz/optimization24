#######################################################
### Dependencies - zale¿noœci które musz¹ byæ spe³nione
#######################################################

# -----------------------------------------------------
# Ograniczenia - równania f(x) = 0
# -----------------------------------------------------
equalities = function(x) {
    var <- Variables(x) # x - wektor zmiennych manipulacyjnych

    eq <- c()

    EQ_LENGTH = 8
    grad_d1 <- c()
    grad_d2 <- c()
    grad_d3 <- c()
    grad_d4 <- c()
    grad_d5 <- c()
    grad_d6 <- c()
    grad_d7 <- c()
    grad_d8 <- c()

    mST_25_KS4 <- F_mST_25_KS4()
    grad_mST_25_KS4 <- G_mST_25_KS4()

    for (i in 1:24) {
        # bilanse masy w turbinach
        d1 = var@mST_TZ1_in[i] - var@mST_TZ1_up25[i] - var@mST_TZ1_up13[i] - var@mST_TZ1_up06[i] - var@mST_TZ1_wyd[i]
        grad_d1[[i]] = c(1, -1, -1, -1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0,0,0)
        d2 = var@mST_TZ2_in[i] - var@mST_TZ2_up25[i] - var@mST_TZ2_up13[i] - var@mST_TZ2_up06[i] - var@mST_TZ2_kond[i]
        grad_d2[[i]] = c(0, 0, 0, 0,0, 1, -1, -1, -1, -1, 0, 0, 0, 0, 0,0)
        d6 = var@mST_TZ5_in[i] - var@mST_TZ5_up25[i] - var@mST_TZ5_up13[i] - var@mST_TZ5_up06[i] - var@mST_TZ5_kond[i]
        grad_d6[[i]] = c(0, 0, 0, 0,0, 0, 0, 0, 0, 0, 1, -1, -1, -1, -1,0)

        # zapotrzebowanie na parê (suma na kolektory)
        d3 = var@mST_TZ1_up25[i] + var@mST_TZ2_up25[i] + var@mST_TZ5_up25[i] - zap_par_25[i]
        grad_d3[[i]] = c(0, 1, 0, 0,0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0,0)
        d4 = var@mST_TZ1_up13[i] + var@mST_TZ2_up13[i] + var@mST_TZ5_up13[i] - zap_par_13[i]
        grad_d4[[i]] = c(0, 0, 1, 0, 0,0, 0, 1, 0, 0, 0, 0, 1, 0, 0,0)
        d5 = var@mST_TZ1_up06[i] + var@mST_TZ2_up06[i] + var@mST_TZ5_up06[i] - zap_par_06[i]
        grad_d5[[i]] = c(0, 0, 0, 1,0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0,0)

        # KS4
        d7 = var@mST_TZ5_in[i] - mST_KS4[i] - var@mST_KS4_swing[i]
        grad_d7[[i]] = c(0, 0, 0, 0,0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,-1)

        # TZ5 up25
        d8 = var@mST_TZ5_up25[i] - mST_25_KS4[i]
        grad_d8[[i]] = c(0, 0, 0, 0,0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,0) - grad_mST_25_KS4[[i]]

        eq[(24 * 0 + 0) + 1 * i] = d1
        eq[(24 * 1 + 0) + 1 * i] = d2
        eq[(24 * 2 + 0) + 1 * i] = d3
        eq[(24 * 3 + 0) + 1 * i] = d4
        eq[(24 * 4 + 0) + 1 * i] = d5
        eq[(24 * 5 + 0) + 1 * i] = d6
        eq[(24 * 6 + 0) + 1 * i] = d7
        eq[(24 * 7 + 0) + 1 * i] = d8
    }

    # swing: jedno równanie dopisane na koñcu - na pozycji (EQ_LENGTH * 24) + 1
    eq[length(eq) + 1] = sum(var@mST_KS4_swing) # suma mST_KS4_swing[1] + mST_KS4_swing[2] + ... = 0

    GRADS <- list()

    r <- c()
    eqg <- c()
    for (i in 1:(EQ_LENGTH * 24)) {
        w <- c()

        r <- (i %/% 24) + 1
        if (i%%24 == 0) {
            r <- r - 1
        }

        h_r <- (i %% 24)
        if (h_r == 0) {
            h_r = 24
        }

        GRADS[[1]] <- grad_d1[[h_r]] #c(1, -1, -1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        GRADS[[2]] <- grad_d2[[h_r]] #c(0, 0, 0, 0, 1, -1, -1, -1, -1, 0, 0, 0, 0, 0)
        GRADS[[6]] <- grad_d6[[h_r]] #c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -1, -1, -1, -1)
        GRADS[[3]] <- grad_d3[[h_r]] #c(0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0)
        GRADS[[4]] <- grad_d4[[h_r]] #c(0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0)
        GRADS[[5]] <- grad_d5[[h_r]] #c(0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0)
        GRADS[[7]] <- grad_d7[[h_r]] #c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
        GRADS[[8]] <- grad_d8[[h_r]] #c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0)

        for (j in 1:(X_LENGTH * 24)) {
            h_x <- j %% 24
            if (h_x == 0) {
                h_x = 24
            }

            v_x <- (j %/% 24) + 1
            if (h_x == 24) {
                v_x <- v_x - 1
            }

            if (h_x == h_r) {
                w <- c(w, GRADS[[r]][v_x])
            } else {
                w <- c(w, 0)
            }
        }
        eqg <- rbind(eqg,w)
    }

    ## swing: pochodna równania mST_KS4_swing[1] + mST_KS4_swing[2] + ... = 0 po ka¿dej z 24 zmiennych jest równa 1
    eqg <- rbind(eqg, c(rep(0, times = (X_LENGTH - 1) * 24), rep(1, times = 24)))

    return(list("constraints" = eq,
        "jacobian" = eqg))
}

# Ograniczenia - nierównoœci f(x) < 0
inequalities = function(x) {

    # x - wektor zmiennych manipulacyjnych
    var <- Variables(x)

    ieq <- c()

    IEQ_LENGTH = 4
    grad_d1 <- c()
    grad_d2 <- c()
    grad_d3 <- c()
    grad_d4 <- c()

    mST_25_KS4 <- F_mST_25_KS4()
    grad_mST_25_KS4 <- G_mST_25_KS4()

    # ograniczenia gdzie wyznaczam gradient dla jednej godziny
    for (i in 1:24) {
        # ograniczenia turbin
        d1 = (var@mST_TZ1_up25[i] + var@mST_TZ1_up13[i]) - 60
        grad_d1[[i]] = c(0, 1, 1, 0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0)
        d2 = (var@mST_TZ2_up06[i] + var@mST_TZ2_kond[i]) - 260
        grad_d2[[i]] = c(0, 0, 0, 0,0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0,0)
        d3 = 0.9 * (zap_par_25[i] - mST_25_KS4[i]) - var@mST_TZ1_up25[i]
        grad_d3[[i]] = c(0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        d4 = (var@mST_TZ1_up06[i] + var@mST_TZ1_wyd[i]) - 240
        grad_d4[[i]] = c(0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        if (i == 1 || i == 24) {
            d5 = var@mST_TZ1_in[i] - mST_TZ1_in.max
        } else {
            d5 = var@mST_TZ1_in[i] - var@mST_TZ1_in[i - 1] - 35

        }

        ieq[(24 * 0 + 0) + 1 * i] = d1
        ieq[(24 * 1 + 0) + 1 * i] = d2
        ieq[(24 * 2 + 0) + 1 * i] = d3
        ieq[(24 * 3 + 0) + 1 * i] = d4
    }

    GRADS <- list()

    r <- c()
    ieqg <- c()
    for (i in 1:(IEQ_LENGTH * 24)) {
        w <- c()

        r <- (i %/% 24) + 1
        if (i %% 24 == 0) {
            r <- r - 1
        }

        h_r <- (i %% 24)
        if (h_r == 0) {
            h_r = 24
        }

        GRADS[[1]] <- grad_d1[[h_r]]
        GRADS[[2]] <- grad_d2[[h_r]]
        GRADS[[3]] <- grad_d3[[h_r]]
        GRADS[[4]] <- grad_d4[[h_r]]

        for (j in 1:(X_LENGTH * 24)) {
            h_x <- j %% 24
            if (h_x == 0) {
                h_x = 24
            }

            v_x <- (j %/% 24) + 1
            if (h_x == 24) {
                v_x <- v_x - 1
            }

            if (h_x == h_r) {
                w <- c(w, GRADS[[r]][v_x])
            } else {
                w <- c(w, 0)
            }
        }
        ieqg <- rbind(ieqg, w)
    }

    # ograniczenia gdzie wyznaczam gradient pomiêdzy godzinami

    IEQ_LENGTH_2 = 2
    grad_d5_Previous <- c()
    grad_d5_Current <- c()
    grad_d5_Next <- c()
    grad_d6_Previous <- c()
    grad_d6_Current <- c()
    grad_d6_Next <- c()

    for (i in 1:24) {
        # dopuszczalna prêdkoœæ zmiany obci¹¿enia K7
        if (i == 1) {
            d5 = var@mST_TZ1_in[i] - mST_TZ1_in.max
            grad_d5_Previous[[i]] = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
            grad_d5_Current[[i]] = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
            grad_d5_Next[[i]] = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        } else {
            d5 = var@mST_TZ1_in[i] - var@mST_TZ1_in[i - 1] - 30
            grad_d5_Previous[[i]] = c(-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
            grad_d5_Current[[i]] = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
            grad_d5_Next[[i]] = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        }
        if (i == 1) {
            d6 = var@mST_TZ1_in[i] - mST_TZ1_in.max
            grad_d6_Previous[[i]] = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
            grad_d6_Current[[i]] = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
            grad_d6_Next[[i]] = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        } else {
            d6 = - var@mST_TZ1_in[i] + var@mST_TZ1_in[i - 1] - 30
            grad_d6_Previous[[i]] = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
            grad_d6_Current[[i]] = c(-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
            grad_d6_Next[[i]] = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        }

        ieq[(24 * (IEQ_LENGTH + 0)) + 1 * i] = d5
        ieq[(24 * (IEQ_LENGTH + 1)) + 1 * i] = d6
    }

    GRADS_PREVIOUS <- list()
    GRADS_CURRENT <- list()
    GRADS_NEXT <- list()

    r <- c()
    for (i in 1:(IEQ_LENGTH_2*24)) {
        w <- c()

        r <- (i %/% 24) + 1
        if (i %% 24 == 0) {
            r <- r - 1
        }

        h_r <- (i %% 24)
        if (h_r == 0) {
            h_r = 24
        }

        GRADS_PREVIOUS[[1]] <- grad_d5_Previous[[h_r]]
        GRADS_CURRENT[[1]] <- grad_d5_Current[[h_r]]
        GRADS_NEXT[[1]] <- grad_d5_Next[[h_r]]
        GRADS_PREVIOUS[[2]] <- grad_d6_Previous[[h_r]]
        GRADS_CURRENT[[2]] <- grad_d6_Current[[h_r]]
        GRADS_NEXT[[2]] <- grad_d6_Next[[h_r]]

        for (j in 1:(X_LENGTH * 24)) {
            h_x <- j %% 24
            if (h_x == 0) {
                h_x = 24
            }

            v_x <- (j %/% 24) + 1
            if (h_x == 24) {
                v_x <- v_x - 1
            }

            if (h_x == (h_r - 1)) {
                w <- c(w, GRADS_PREVIOUS[[r]][v_x])
            } else if (h_x == h_r) {
                w <- c(w, GRADS_CURRENT[[r]][v_x])
            } else if (h_x == h_r + 1) {
                w <- c(w, GRADS_NEXT[[r]][v_x])
            } else {
                w <- c(w, 0)
            }

        }
        ieqg <- rbind(ieqg, w)
    }

    return(list("constraints" = ieq,
   "jacobian" = ieqg))
}