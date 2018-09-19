#######################################################
### Dependencies - zale¿noœci które musz¹ byæ spe³nione
#######################################################

# Ograniczenia - równania
equalities = function(x) {
    # x - wektor zmiennych manipulacyjnych
    var <- Variables(x)

    eq <- c()
    eq_grad <- c()

    temp <- 0
    for (i in 1:24) {
        # bilanse masy w turbinach
        d1 = var@mST_TZ1_in[i] - var@mST_TZ1_up25[i] - var@mST_TZ1_up13[i] - var@mST_TZ1_up06[i]
        grad_d1 = c(1, -1, -1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        d2 = var@mST_TZ2_in[i] - var@mST_TZ2_up25[i] - var@mST_TZ2_up13[i] - var@mST_TZ2_up06[i] - var@mST_TZ2_kond[i]
        grad_d2 = c(0, 0, 0, 0, 1, -1, -1, -1, -1, 0, 0, 0, 0, 0)
        d6 = var@mST_TZ5_in[i] - var@mST_TZ5_up25[i] - var@mST_TZ5_up13[i] - var@mST_TZ5_up06[i] - var@mST_TZ5_kond[i]
        grad_d6 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -1, -1, -1, -1)

        # zapotrzebowanie na parê (suma na kolektory)
        d3 = var@mST_TZ1_up25[i] + var@mST_TZ2_up25[i] + var@mST_TZ5_up25[i] - zap_par_25[i]
        grad_d3 = c(0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0)
        d4 = var@mST_TZ1_up13[i] + var@mST_TZ2_up13[i] + var@mST_TZ5_up13[i] - zap_par_13[i]
        grad_d4 = c(0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0)
        d5 = var@mST_TZ1_up06[i] + var@mST_TZ2_up06[i] + var@mST_TZ5_up06[i] - zap_par_06[i]
        grad_d5 = c(0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0)

        # KS4
        d7 = var@mST_TZ5_in[i] - mST_KS4[i]
        grad_d7 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0)

        eq[(24 * 0 + 0) + 1 * i] = d1
        eq[(24 * 1 + 0) + 1 * i] = d2
        eq[(24 * 2 + 0) + 1 * i] = d3
        eq[(24 * 3 + 0) + 1 * i] = d4
        eq[(24 * 4 + 0) + 1 * i] = d5
        eq[(24 * 5 + 0) + 1 * i] = d6
        eq[(24 * 6 + 0) + 1 * i] = d7

        #  eq[7*(i-1)+1] = d1
        #  eq[7*(i-1)+2] = d2
        #eq[7*(i-1)+3] = d3
        #eq[7*(i-1)+4] = d4
        #eq[7*(i-1)+5] = d5
        #eq[7*(i-1)+6] = d6
        #eq[7*(i-1)+7] = d7

        for (j in (1:(X_LENGTH * 24))) {
            #24
            h_x <- j %% 24
            if (h_x == 0) {
                h_x = 24 # godzina
            }

            v_x <- (j %/% 24) + 1
            if (h_x == 24) {
                v_x <- v_x - 1
            }

            ##

            temp <- temp + 1
            temp2 <- (((temp - 1) / 24 * 14) %/% (336)) + 1
            v_eq_grad <- (temp) %/% (24) + 1
            if ((temp) %% (24) == 0) {
                v_eq_grad <- v_eq_grad - 1
            }

            h_eq_grad <- (temp) %% (24)
            if (h_eq_grad == 0) {
                h_eq_grad = 24
            }
            a <- j %/% 24 + 1
            if (a == X_LENGTH + 1) {
                a <- a - 1
            }

            #eq_grad[X_LENGTH * 24 * 24 * 0 + (i-1) * X_LENGTH * 24 + j]=ifelse(h_x==i ,grad_d1[v_x],0)
            #eq_grad[X_LENGTH * 24 * 24 * 1 + (i-1) * X_LENGTH * 24 + j]=ifelse(h_x==i ,grad_d2[v_x],0)
            #eq_grad[X_LENGTH * 24 * 24 * 2 + (i-1) * X_LENGTH * 24 + j]=ifelse(h_x==i ,2,0)
            #eq_grad[X_LENGTH * 24 * 24 * 3 + (i-1) * X_LENGTH * 24 + j]=ifelse(h_x==i ,2,0)
            #eq_grad[X_LENGTH * 24 * 24 * 4 + (i-1) * X_LENGTH * 24 + j]=ifelse(h_x==i ,2,0)
            #eq_grad[X_LENGTH * 24 * 24 * 5 + (i-1) * X_LENGTH * 24 + j]=ifelse(h_x==i ,2,0)
            #eq_grad[X_LENGTH * 24 * 24 * 6 + (i-1) * X_LENGTH * 24 + j]=ifelse(h_x==i ,2,0)    

        }

    }

    eq2 <- c(var@mST_TZ1_in[i] - var@mST_TZ1_up25[i] - var@mST_TZ1_up13[i] - var@mST_TZ1_up06[i],
            var@mST_TZ2_in[i] - var@mST_TZ2_up25[i] - var@mST_TZ2_up13[i] - var@mST_TZ2_up06[i] - var@mST_TZ2_kond[i])
    eq_a <- c(var@mST_TZ1_in[i] - var@mST_TZ1_up25[i] - var@mST_TZ1_up13[i] - var@mST_TZ1_up06[i])
    grad_a <- c(1, rep(0, 23),
                -1, rep(0, 23),
                -1, rep(0, 23),
                -1, rep(0, 23),
                rep(0, 24),
                rep(0, 24),
                rep(0, 24),
                rep(0, 24),
                rep(0, 24),
                rep(0, 24),
                rep(0, 24),
                rep(0, 24),
                rep(0, 24),
                rep(0, 24)
                )
    grad_b <- c(rep(0, 24),
                rep(0, 24),
                rep(0, 24),
                rep(0, 24),
                1, rep(0, 23),
                -1, rep(0, 23),
                -1, rep(0, 23),
                -1, rep(0, 23),
                -1, rep(0, 23),
                rep(0, 24),
                rep(0, 24),
                rep(0, 24),
                rep(0, 24),
                rep(0, 24)
                )
    eq_grad <- rbind(eq_grad, grad_a)
    eq_grad <- rbind(eq_grad, grad_b)

    GRADS <- list()
    GRADS[[1]] <- c(1, -1, -1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    GRADS[[2]] <- c(0, 0, 0, 0, 1, -1, -1, -1, -1, 0, 0, 0, 0, 0)
    GRADS[[6]] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -1, -1, -1, -1)
    GRADS[[3]] <- c(0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0)
    GRADS[[4]] <- c(0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0)
    GRADS[[5]] <- c(0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0)
    GRADS[[7]] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0)
    r <- c()
    eqg <- c()
    for (i in 1:(7 * 24)) {
        w <- c()

        for (j in 1:(X_LENGTH * 24)) {
            h_x <- j %% 24
            if (h_x == 0) {
                h_x = 24
            }

            v_x <- (j %/% 24) + 1
            if (h_x == 24) {
                v_x <- v_x - 1
            }

            r <- (i %/% 24) + 1
            if (r == 2) {
                breakp <- 2
            }
            if (h_x == 24) {
                r <- r - 1
            }

            h_r <- (i %% 24)
            if (h_r == 0){
                h_r = 24
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