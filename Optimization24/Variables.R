#####################################
### Variables - zmienne manipulacyjne
#####################################

X_LENGTH = 15

# definicja (kolejne dopisywaæ na koñcu!)
setClass("Variables",
  slots = list(
	mST_TZ1_in   = "vector",
	mST_TZ1_up25 = "vector",
    mST_TZ1_up13 = "vector",
    mST_TZ1_up06 = "vector",
    mST_TZ2_in   = "vector",
    mST_TZ2_up25 = "vector",
    mST_TZ2_up13 = "vector",
    mST_TZ2_up06 = "vector",
    mST_TZ2_kond = "vector",
    mST_TZ5_in   = "vector",
    mST_TZ5_up25 = "vector",
    mST_TZ5_up13 = "vector",
    mST_TZ5_up06 = "vector",
    mST_TZ5_kond = "vector",
    mST_KS4_swing = "vector"
	),)

# konstruktor
Variables = function(x) {
    new("Variables",
        mST_TZ1_in   = c(x[(24*0+1 ):(24+24*0 )]),
        mST_TZ1_up25 = c(x[(24*1+1 ):(24+24*1 )]),
        mST_TZ1_up13 = c(x[(24*2+1 ):(24+24*2 )]),
        mST_TZ1_up06 = c(x[(24*3+1 ):(24+24*3 )]),
        mST_TZ2_in   = c(x[(24*4+1 ):(24+24*4 )]),
        mST_TZ2_up25 = c(x[(24*5+1 ):(24+24*5 )]),
        mST_TZ2_up13 = c(x[(24*6+1 ):(24+24*6 )]),
        mST_TZ2_up06 = c(x[(24*7+1 ):(24+24*7 )]),
        mST_TZ2_kond = c(x[(24*8+1 ):(24+24*8 )]),
        mST_TZ5_in   = c(x[(24*9+1 ):(24+24*9 )]),
        mST_TZ5_up25 = c(x[(24*10+1):(24+24*10)]),
        mST_TZ5_up13 = c(x[(24*11+1):(24+24*11)]),
        mST_TZ5_up06 = c(x[(24*12+1):(24+24*12)]),
        mST_TZ5_kond = c(x[(24*13+1):(24+24*13)]),
        mST_KS4_swing = c(x[(24*14+1):(24+24*14)]))
}

printVariables = function(x,n,chart) {

    var <- Variables(x)

    cat("\n           TZ1                 TZ2                      TZ5                     PEL")
    cat("\n           in  up25 up13 up06  in  up25 up13 up06 kond  in  up25 up13 up06 kond")

    i_start <- 1
    if (n != 24) {
        i_start <- n
    }

    for (i in i_start:n) {
        cat("\nH: ", sprintf("%2.0f",i)[1] ," // ")
        cat(sprintf("%3.0f",as.integer(round(var@mST_TZ1_in   [i],digits=0)))[1], " ")
        cat(sprintf("%3.0f",as.integer(round(var@mST_TZ1_up25 [i],digits=0)))[1], " ")
        cat(sprintf("%3.0f",as.integer(round(var@mST_TZ1_up13 [i],digits=0)))[1], " ")
        cat(sprintf("%3.0f",as.integer(round(var@mST_TZ1_up06 [i],digits=0)))[1], " ")
        cat(sprintf("%3.0f",as.integer(round(var@mST_TZ2_in   [i],digits=0)))[1], " ")
        cat(sprintf("%3.0f",as.integer(round(var@mST_TZ2_up25 [i],digits=0)))[1], " ")
        cat(sprintf("%3.0f",as.integer(round(var@mST_TZ2_up13 [i],digits=0)))[1], " ")
        cat(sprintf("%3.0f",as.integer(round(var@mST_TZ2_up06 [i],digits=0)))[1], " ")
        cat(sprintf("%3.0f",as.integer(round(var@mST_TZ2_kond [i],digits=0)))[1], " ")
        cat(sprintf("%3.0f",as.integer(round(var@mST_TZ5_in   [i],digits=0)))[1], " ")
        cat(sprintf("%3.0f",as.integer(round(var@mST_TZ5_up25 [i],digits=0)))[1], " ")
        cat(sprintf("%3.0f",as.integer(round(var@mST_TZ5_up13 [i],digits=0)))[1], " ")
        cat(sprintf("%3.0f",as.integer(round(var@mST_TZ5_up06 [i],digits=0)))[1], " ")
        cat(sprintf("%3.0f", as.integer(round(var@mST_TZ5_kond[i], digits = 0)))[1], " ")
        cat(sprintf("%3.0f", as.integer(round(PEL_TZ1(x)[i]+PEL_TZ2(x)[i]+PEL_TZ5(x)[i], digits = 0)))[1], " ")
        cat("/M(-FC):", sprintf("%6.0f",-round(KE(x)[i] + KP_TZ1(x)[i] + KP_TZ2(x)[i] + KP_TZ5(x)[i], digits = 0)))
        cat(" /KE:", sprintf("%5.0f",round(KE(x)[i], digits = 0)))
        cat(" /KP:", sprintf("%5.0f",round(KP_TZ1(x)[i] + KP_TZ2(x)[i] + KP_TZ5(x)[i], digits = 0)))
        cat(" /Z:", round(zap_par_06[i] + zap_par_13[i] + zap_par_25[i], digits = 0), "(",
            sprintf("%2.0f", zap_par_25[i]),
            sprintf("%3.0f", zap_par_13[i]),
            sprintf("%3.0f", zap_par_06[i]), ")",
            sprintf("%3.0f", zap_el[i]))
        cat("/", ifelse(PEL_TZ1(x)[i] + PEL_TZ2(x)[i] + PEL_TZ5(x)[i] > zap_el[i], "ODD", "ZAK"), sprintf("%3.0f", round(zap_el[i] - (PEL_TZ1(x)[i] + PEL_TZ2(x)[i] + PEL_TZ5(x)[i])),digits = 1 ))
        cat("/ SWG:", sprintf("%3.0f", round(var@mST_KS4_swing[i]), digits = 3))
        cat(" /C:", sprintf("%1.0f", round(c_RDN[i]), digits = 2))
            #rep(" ", times = (c_RDN[i] %/% 10 - 7)),
            #ifelse(round(var@mST_TZ2_kond[i], digits = 0) < 70 || round(var@mST_TZ5_kond[i], digits = 0) < 150, "#", "|"))
        #round(c_RDN[i], digits = 2)
    }


    ### PLOT ###
    if (chart == TRUE) {
        if (FALSE) {
            #The data have a common independent variable(x)
            x <- c(1:24)

            # Generate 4 different sets of outputs
            y1 <- var@mST_KS4_swing
            y2 <- c_RDN
            y3 <- var@mST_TZ5_in
            y4 <- var@mST_TZ5_kond
            y <- list(y1, y2, y3, y4)

            # Colors for y[[2]], y[[3]], y[[4]] points and axes
            colors = c("red", "blue", "green")

            # Set the margins of the plot wider
            par(oma = c(0, 2, 2, 3))

            plot(x, y[[1]], yaxt = "n", xlab = "czas", main = "Swing",
         ylab = "")
            lines(x, y[[1]])

            # We use the "pretty" function go generate nice axes
            axis(at = pretty(y[[1]]), side = 2)

            # The side for the axes.  The next one will go on 
            # the left, the following two on the right side
            sides <- list(2, 4, 4)

            # The number of "lines" into the margin the axes will be
            lines <- list(2, NA, 2)

            for (i in 2:4) {
                par(new = TRUE)
                plot(x, y[[i]], axes = FALSE, col = colors[i - 1], xlab = "", ylab = "")
                axis(at = pretty(y[[i]]), side = sides[[i - 1]], line = lines[[i - 1]],
          col = colors[i - 1])
                lines(x, y[[i]], col = colors[i - 1])
            }

            # Profit.

        }

        library(ggplot2)
        library(reshape2)
        # PLOT 1
        swing <- var@mST_KS4_swing
        c_RDN <- c_RDN
        mST_TZ5_in <- var@mST_TZ5_in
        mST_TZ5_kond <- var@mST_TZ5_kond
        mST_KS4 <- mST_KS4
        balance <- zap_el - (PEL_TZ1(x) + PEL_TZ2(x) + PEL_TZ5(x))
        time <- c(1:24)
        dat <- data.frame(time, swing, c_RDN, mST_TZ5_in, mST_TZ5_kond, balance)
        dat.m <- melt(dat, "time")
        plot1 <- ggplot(dat.m, aes(time, value, colour = variable)) + geom_line() +
        facet_wrap(~variable, ncol = 1, scales = "free_y") + theme(legend.position = "none")
        plot(plot1)

        # PLOT 2
        plot2 <- ggplot(dat, aes(x = time))
        plot2 <- plot2 + geom_line(aes(y = mST_TZ5_in, colour = "mST_KS4 (swing)"))

        # adding the relative humidity data, transformed to match roughly the range of the temperature
        plot2 <- plot2 + geom_line(aes(y = mST_KS4, colour = "mST_KS4 (bez swing)"))

        # now adding the secondary axis, following the example in the help file ?scale_y_continuous
        # and, very important, reverting the above transformation
        #p <- p + scale_y_continuous(sec.axis = sec_axis(~. * 1, name = "Relative humidity [%]"))

        # modifying colours and theme options
        plot2 <- plot2 + scale_colour_manual(values = c("blue", "red"))
        plot2 <- plot2 + labs(y = "Steam flow [t/h]",
                x = "Time",
                colour = "Steam flow [t/h]")
        plot2 <- plot2 + theme(legend.position = c(0.8, 0.9))
        plot(plot2)
    }
}