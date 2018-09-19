#######################################################
### Dependencies - zale¿noœci które musz¹ byæ spe³nione
#######################################################

# Ograniczenia - równania
equalities = function(x) {
    # x - wektor zmiennych manipulacyjnych
    var <- Variables(x)

    eq <- c()
    eq_grad <- c()

    EQ_LENGTH = 7
    grad_d1 <- c()
    grad_d2 <- c()
    grad_d3 <- c()
    grad_d4 <- c()
    grad_d5 <- c()
    grad_d6 <- c()
    grad_d7 <- c()

    for (i in 1:24) {
        # bilanse masy w turbinach
        d1 = var@mST_TZ1_in[i] - var@mST_TZ1_up25[i] - var@mST_TZ1_up13[i] - var@mST_TZ1_up06[i]
        grad_d1[[i]] = c(1, -1, -1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        d2 = var@mST_TZ2_in[i] - var@mST_TZ2_up25[i] - var@mST_TZ2_up13[i] - var@mST_TZ2_up06[i] - var@mST_TZ2_kond[i]
        grad_d2[[i]] = c(0, 0, 0, 0, 1, -1, -1, -1, -1, 0, 0, 0, 0, 0)
        d6 = var@mST_TZ5_in[i] - var@mST_TZ5_up25[i] - var@mST_TZ5_up13[i] - var@mST_TZ5_up06[i] - var@mST_TZ5_kond[i]
        grad_d6[[i]] = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -1, -1, -1, -1)

        # zapotrzebowanie na parê (suma na kolektory)
        d3 = var@mST_TZ1_up25[i] + var@mST_TZ2_up25[i] + var@mST_TZ5_up25[i] - zap_par_25[i]
        grad_d3[[i]] = c(0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0)
        d4 = var@mST_TZ1_up13[i] + var@mST_TZ2_up13[i] + var@mST_TZ5_up13[i] - zap_par_13[i]
        grad_d4[[i]] = c(0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0)
        d5 = var@mST_TZ1_up06[i] + var@mST_TZ2_up06[i] + var@mST_TZ5_up06[i] - zap_par_06[i]
        grad_d5[[i]] = c(0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0)

        # KS4
        d7 = var@mST_TZ5_in[i] - mST_KS4[i]
        grad_d7[[i]] = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0)

        eq[(24 * 0 + 0) + 1 * i] = d1
        eq[(24 * 1 + 0) + 1 * i] = d2
        eq[(24 * 2 + 0) + 1 * i] = d3
        eq[(24 * 3 + 0) + 1 * i] = d4
        eq[(24 * 4 + 0) + 1 * i] = d5
        eq[(24 * 5 + 0) + 1 * i] = d6
        eq[(24 * 6 + 0) + 1 * i] = d7
    }

    GRADS <- list()

    r <- c()
    eqg <- c()
    for (i in 1:(EQ_LENGTH * 24)) {
        w <- c()

        r <- (i %/% 24) + 1
        if (i%%24 == 0) { # if (h_x == 24) {
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

        return(list("constraints" = eq,
    "jacobian" = eqg))
        }

# Ograniczenia - nierównoœci
inequalities = function(x) {
    # x - wektor zmiennych manipulacyjnych
    var <- Variables(x)

    ieq <- c()
    ieq_grad <- c()

    # ograniczenia gdzie wyznaczam gradient dla jednej godziny
    for (i in 1:24) {
        # ograniczenia turbin
        d1 = var@mST_TZ1_up25[i] + var@mST_TZ1_up13[i] - 60
        grad_d1 = c(0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) 
        d2 = var@mST_TZ2_up06[i] + var@mST_TZ2_kond[i] - 260
        grad_d2 = c(0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0)
        d3 = 0.4 * zap_par_25[i] - var@mST_TZ1_up25[i]
        grad_d3 = c(0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        ieq <- c(ieq, c(d1, d2, d3))

        for (j in 1:24) {
            if (i == j) {
                ieq_grad <- c(ieq_grad, c(grad_d1, grad_d2, grad_d3))
            } else {
                ieq_grad <- c(ieq_grad, rep(0,times = length(x)/24*3))
            }
        }    
    }

    # ograniczenia gdzie wyznaczam gradient dla wszystkich godzin
    # ograniczenia proceduralne
    d4 = -(PEL_TZ1(x) + PEL_TZ2(x) + PEL_TZ5(x) - prod_limit - 1)
    grad_d4 = -(grad_PEL_TZ1(x) + grad_PEL_TZ2(x) + grad_PEL_TZ5(x) - 0)

    ieq <- c(ieq, c(d4))
    ieq_grad <- c(ieq_grad, rep(grad_d4,24))

    return(list("constraints" = ieq,
   "jacobian" = ieq_grad))
}