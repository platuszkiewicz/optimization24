# biblioteka optymalizacji
library("nloptr")

# modu³y modelu
source("Input.R")         # dane wejœciowe
source("Variables.R")     # zmienne manipulacyjne
source("Constraints.R")   # ograniczenia zmiennych manipulacyjnych
source("Dependencies.R")  # zale¿noœci (dodatkowe ograniczenia) zmiennych manipulacyjnych
source("Turbines.R")      # charakterystyki turbin

#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  

# FC1 - maksymalizuj mar¿ê
pl1 <- 0
fc1 = function(x) {
    return(-sum(PEL_TZ1(x) * c_RDN + 2 * PEL_TZ2(x) * c_RDN + PEL_TZ5(x) * c_RDN))
}



grad_fc1 = function(x) {
    return(-(grad_PEL_TZ1(x) * c_RDN + 2 * grad_PEL_TZ2(x) * c_RDN + grad_PEL_TZ5(x) * c_RDN))
}

    # optymalizacja
    local_opts <- list("algorithm" = "NLOPT_LD_MMA",
    "xtol_rel" = 1.0e-5,
      "xtol_rel" = 0.000001,
      "ftol_rel" = 0.000001,
      "ftol_abs" = 0.000001,
      "xtol_abs" = 0.000001) 

    opts <- list("algorithm" = "NLOPT_LD_SLSQP",
             "check_derivatives" = FALSE, # poka¿ raport ze sprawdzania pochodnych
             "maxeval" = 995000,           # 1000
             "maxtime" = 6,              # [s]
             "local_opts" = local_opts,
             "xtol_rel" = 1.0e-14,
             "ftol_rel" = 1.0e-07,
             "ftol_abs" = 1.0e-07,
             "xtol_abs" = 1.0e-14)

    #opts <- list("algorithm" = "NLOPT_LD_AUGLAG", #NLOPT_GN_ISRES
             #"xtol_rel" = 3.0e-1,        # 1.0e-5
             #"check_derivatives" = FALSE, # poka¿ raport ze sprawdzania pochodnych
             #"maxeval" = 5000,           # 1000
             #"maxtime" = 7,              # [s]
             #"local_opts" = local_opts,
             #"ftol_rel" = 0.0)

    opt <- nloptr(x0 = constraintsDefault,
    eval_f = fc1,
    eval_grad_f = grad_fc1,
    lb = constraintsLB,
    ub = constraintsUB,
    eval_g_eq = equalities,
    #eval_g_ineq = inequalities,
    opts = opts)
    opt

printVariables(opt$solution)
a <- equalities(opt$solution)
cat("\nConstraints:", a$constraints)



    #cat("\n # # # # # ROZWIAZANIE # # # # # ")
    #cat("\nSolution (FC):", opt$objective)
    #cat("\nObjective (x):", opt$solution)
    #cat("\nPEL TZ1: ", PEL_TZ1(opt$solution), " // PEL TZ2: ", PEL_TZ2(opt$solution), " // PEL TZ5: ", PEL_TZ5(opt$solution), " /// SUM:", PEL_TZ1(opt$solution) + PEL_TZ2(opt$solution) + PEL_TZ5(opt$solution))
    #cat("\nEqualities check: ")
    #cat(equalities(opt$solution)$constraints)
    #cat("\nInequalities check: ")
    #cat(inequalities(opt$solution)$constraints)