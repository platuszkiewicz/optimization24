# biblioteka optymalizacji
library("nloptr")

# ustawienia obliczeñ
#calcOptions.swing <- FALSE

# modu³y modelu
source("Input.R")         # dane wejœciowe
source("Variables.R")     # zmienne manipulacyjne
source("Constraints.R")   # ograniczenia zmiennych manipulacyjnych
source("Dependencies.R")  # zale¿noœci (dodatkowe ograniczenia) zmiennych manipulacyjnych
source("Turbines2.R")     # charakterystyki turbin
source("Costs.R")         # koszty
source("Auxiliaries.R")   # potrzeby w³asne

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  

# FC1 - maksymalizuj mar¿ê

fc1 = function(x) {
    #return(-(sum(PEL_TZ1(x) * c_RDN + PEL_TZ2(x) * c_RDN + PEL_TZ5(x) * c_RDN) -
    #sum(KP_TZ1(x) + KP_TZ2(x) + KP_TZ5(x))))

    return(sum(KE(x) + KP_TZ1(x) + KP_TZ2(x) + KP_TZ5(x)))
}

grad_fc1 = function(x) {
    #return(-((grad_PEL_TZ1(x) * c_RDN + grad_PEL_TZ2(x) * c_RDN + grad_PEL_TZ5(x) * c_RDN) -
    #(grad_KP_TZ1(x) + grad_KP_TZ2(x) + grad_KP_TZ5(x))))

    return((grad_KE(x) + grad_KP_TZ1(x) + grad_KP_TZ2(x) + grad_KP_TZ5(x)))
}

# optymalizacja
local_opts <- list("algorithm" = "NLOPT_LD_MMA",
"xtol_rel" = 1.0e-08,
  "xtol_rel" = 0.000001,
  "ftol_rel" = 0.000001,
  "ftol_abs" = 0.000001,
  "xtol_abs" = 0.000001) 

opts <- list("algorithm" = "NLOPT_LD_SLSQP",
         "check_derivatives" = FALSE,     # poka¿ raport ze sprawdzania pochodnych
         "check_derivatives_tol" = 1e-03, # dok³adnoœæ sprawdzania pochodnych
         "maxeval" = 1000,                # 1000
         "maxtime" = 35,                  # [s]
         "local_opts" = local_opts,
         "xtol_rel" = 1.0e-14,
         "ftol_rel" = 1.0e-07,
         "ftol_abs" = 1.0e-07,
         "xtol_abs" = 1.0e-14)

#opts <- list("algorithm" = "NLOPT_LD_AUGLAG", #NLOPT_GN_ISRES
         #"xtol_rel" = 1.0e-7,        # 1.0e-5
         #"check_derivatives" = FALSE, # poka¿ raport ze sprawdzania pochodnych
         #"maxeval" = 5000,           # 1000
         #"maxtime" = 55,              # [s]
         #"local_opts" = local_opts,
         #"ftol_rel" = 0.0)

optim <- nloptr(x0 = constraintsDefault,
    eval_f = fc1,
    eval_grad_f = grad_fc1,
    lb = constraintsLB,
    ub = constraintsUB,
    eval_g_eq = equalities,
    eval_g_ineq = inequalities,
    opts = opts)
optim

printVariables(optim$solution,24, TRUE)

cat("\nEqualities:", (equalities(optim$solution))$constraints)
cat("\nEqualities abs sum:", sum(abs((equalities(optim$solution))$constraints)))
cat("\nInequalities:", (inequalities(optim$solution))$constraints)
cat("\nKoszt Energii:", sum(KE(optim$solution)))
cat("\nKoszt Pary:", sum(KP_TZ1(optim$solution) + KP_TZ2(optim$solution) + KP_TZ5(optim$solution)))
cat("\n Koszty - przychody (FC):", sum(KP_TZ1(optim$solution) + KP_TZ2(optim$solution) + KP_TZ5(optim$solution) + KE(optim$solution)))

# zmiana(test) dla jednej godziny
if (FALSE) {
        cat("\n#   TEST   #")
        h_test <- 4
        x_test <- c()
        x_test <- c(x_test, rep(0, h_test - 1), optim$solution[h_test + 24 * 0]+20, rep(0, times = 24 - h_test)) #TZ1
        x_test <- c(x_test, rep(0, h_test - 1), optim$solution[h_test + 24 * 1], rep(0, times = 24 - h_test))
        x_test <- c(x_test, rep(0, h_test - 1), optim$solution[h_test + 24 * 2], rep(0, times = 24 - h_test))
        x_test <- c(x_test, rep(0, h_test - 1), optim$solution[h_test + 24 * 3]+20, rep(0, times = 24 - h_test))
        x_test <- c(x_test, rep(0, h_test - 1), optim$solution[h_test + 24 * 4], rep(0, times = 24 - h_test)) #TZ2
        x_test <- c(x_test, rep(0, h_test - 1), optim$solution[h_test + 24 * 5], rep(0, times = 24 - h_test))
        x_test <- c(x_test, rep(0, h_test - 1), optim$solution[h_test + 24 * 6], rep(0, times = 24 - h_test))
        x_test <- c(x_test, rep(0, h_test - 1), optim$solution[h_test + 24 * 7], rep(0, times = 24 - h_test))
        x_test <- c(x_test, rep(0, h_test - 1), optim$solution[h_test + 24 * 8], rep(0, times = 24 - h_test))
        x_test <- c(x_test, rep(0, h_test - 1), optim$solution[h_test + 24 * 9], rep(0, times = 24 - h_test)) #TZ5
        x_test <- c(x_test, rep(0, h_test - 1), optim$solution[h_test + 24 * 10], rep(0, times = 24 - h_test))
        x_test <- c(x_test, rep(0, h_test - 1), optim$solution[h_test + 24 * 11], rep(0, times = 24 - h_test))
        x_test <- c(x_test, rep(0, h_test - 1), optim$solution[h_test + 24 * 12]-20, rep(0, times = 24 - h_test))
        x_test <- c(x_test, rep(0, h_test - 1), optim$solution[h_test + 24 * 13]+20, rep(0, times = 24 - h_test))
        x_test <- c(x_test, rep(0, h_test - 1), optim$solution[h_test + 24 * 14], rep(0, times = 24 - h_test)) # swing

        printVariables(x_test, h_test, FALSE)

        a <- equalities(x_test)$constraints
        constraints_test <- c()

        constraints_test <- c(constraints_test, a[h_test + 24 * 0])
        constraints_test <- c(constraints_test, a[h_test + 24 * 1])
        constraints_test <- c(constraints_test, a[h_test + 24 * 2])
        constraints_test <- c(constraints_test, a[h_test + 24 * 3])
        constraints_test <- c(constraints_test, a[h_test + 24 * 4])
        constraints_test <- c(constraints_test, a[h_test + 24 * 5])
        constraints_test <- c(constraints_test, a[h_test + 24 * 6])
        constraints_test <- c(constraints_test, a[h_test + 24 * 7])
        constraints_test <- c(constraints_test, a[length(a)])

        cat("\nConstraints:", constraints_test)
        cat("\nConstraints abs sum:", sum(abs(constraints_test)))
    }