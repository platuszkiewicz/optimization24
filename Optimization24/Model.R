# biblioteka optymalizacji
library("nloptr")

# modu³y modelu
source("Input.R")         # dane wejœciowe
source("Variables.R")     # zmienne manipulacyjne
source("Constraints.R")   # ograniczenia zmiennych manipulacyjnych
source("Dependencies.R")  # zale¿noœci (dodatkowe ograniczenia) zmiennych manipulacyjnych
source("Turbines.R")      # charakterystyki turbin
source("Costs.R")         # koszty

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  

# FC1 - maksymalizuj mar¿ê
pl1 <- 0
fc1 = function(x) {
    return(-(sum(PEL_TZ1(x) * c_RDN + PEL_TZ2(x) * c_RDN + PEL_TZ5(x) * c_RDN) -
           sum(KP_TZ1(x) + KP_TZ2(x) + KP_TZ5(x))))
}

grad_fc1 = function(x) {
    return(-((grad_PEL_TZ1(x) * c_RDN + grad_PEL_TZ2(x) * c_RDN + grad_PEL_TZ5(x) * c_RDN) -
           (grad_KP_TZ1(x) + grad_KP_TZ2(x) + grad_KP_TZ5(x))))
}

# optymalizacja
local_opts <- list("algorithm" = "NLOPT_LD_MMA",
"xtol_rel" = 1.0e-10)
  #"xtol_rel" = 0.000001,
  #"ftol_rel" = 0.000001,
  #"ftol_abs" = 0.000001,
  #"xtol_abs" = 0.000001) 

opts <- list("algorithm" = "NLOPT_LD_SLSQP",
         "check_derivatives" = FALSE, # poka¿ raport ze sprawdzania pochodnych
         "maxeval" = 1000,           # 1000
         "maxtime" = 15,              # [s]
         "local_opts" = local_opts,
         "xtol_rel" = 1.0e-14,
         "ftol_rel" = 1.0e-07,
         "ftol_abs" = 1.0e-07,
         "xtol_abs" = 1.0e-14)

#opts <- list("algorithm" = "NLOPT_LD_AUGLAG", #NLOPT_GN_ISRES
         #"xtol_rel" = 3.0e-1,        # 1.0e-5
         #"check_derivatives" = FALSE, # poka¿ raport ze sprawdzania pochodnych
         #"maxeval" = 5000,           # 1000
         #"maxtime" = 17,              # [s]
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

printVariables(optim$solution,24)

cat("\nEqualities:", (equalities(optim$solution))$constraints)
cat("\nEqualities abs sum:", sum(abs((equalities(optim$solution))$constraints)))
cat("\nInequalities:", (inequalities(optim$solution))$constraints)

# zmiana dla jednej godziny
x_test <- c()
x_test <- c(x_test, optim$solution[1+24*0] - 3, rep(0, times = 23)) #TZ1
x_test <- c(x_test, optim$solution[1+24*1], rep(0,times=23))
x_test <- c(x_test, optim$solution[1+24*2], rep(0,times=23))
x_test <- c(x_test, optim$solution[1+24*3]-3, rep(0,times=23))
x_test <- c(x_test, optim$solution[1+24*4], rep(0,times=23)) #TZ2
x_test <- c(x_test, optim$solution[1+24*5], rep(0,times=23))
x_test <- c(x_test, optim$solution[1+24*6], rep(0,times=23))
x_test <- c(x_test, optim$solution[1+24*7]+3, rep(0,times=23))
x_test <- c(x_test, optim$solution[1+24*8]-3, rep(0,times=23))
x_test <- c(x_test, optim$solution[1+24*9], rep(0,times=23)) #TZ5
x_test <- c(x_test, optim$solution[1+24*10], rep(0,times=23))
x_test <- c(x_test, optim$solution[1+24*11], rep(0,times=23))
x_test <- c(x_test, optim$solution[1+24*12], rep(0,times=23))
x_test <- c(x_test, optim$solution[1+24*13], rep(0,times=23))
cat(x_test)
printVariables(x_test, 1)


a <- equalities(x_test)$constraints
constraints_test <- c()

    constraints_test <- c(constraints_test,a[1+24*0])
    constraints_test <- c(constraints_test,a[1+24*1])
    constraints_test <- c(constraints_test,a[1+24*2])
    constraints_test <- c(constraints_test,a[1+24*3])
    constraints_test <- c(constraints_test,a[1+24*4])
    constraints_test <- c(constraints_test,a[1+24*5])
    constraints_test <- c(constraints_test,a[1+24*6])

cat("\nConstraints:", constraints_test)
cat("\nConstraints abs sum:", sum(abs(constraints_test)))