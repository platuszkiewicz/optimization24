#####################################
### Variables - zmienne manipulacyjne
#####################################

X_LENGTH = 14

# definicja (kolejne dopisywa� na ko�cu!)
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
    mST_TZ5_kond = "vector"
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
        mST_TZ5_kond = c(x[(24*13+1):(24+24*13)]))
}

printVariables = function(var) {

    var <- Variables(var)

    for (i in 1:24) {
        cat("\nH: ", i ," // ")
        cat(round(var@mST_TZ1_in   [i],digits=0), "  ")
        cat(round(var@mST_TZ1_up25 [i],digits=0), "  ")
        cat(round(var@mST_TZ1_up13 [i],digits=0), "  ")
        cat(round(var@mST_TZ1_up06 [i],digits=0), "  ")
        cat(round(var@mST_TZ2_in   [i],digits=0), "  ")
        cat(round(var@mST_TZ2_up25 [i],digits=0), "  ")
        cat(round(var@mST_TZ2_up13 [i],digits=0), "  ")
        cat(round(var@mST_TZ2_up06 [i],digits=0), "  ")
        cat(round(var@mST_TZ2_kond [i],digits=0), "  ")
        cat(round(var@mST_TZ5_in   [i],digits=0), "  ")
        cat(round(var@mST_TZ5_up25 [i],digits=0), "  ")
        cat(round(var@mST_TZ5_up13 [i],digits=0), "  ")
        cat(round(var@mST_TZ5_up06 [i],digits=0), "  ")
        cat(round(var@mST_TZ5_kond [i],digits=0), "  ")
    }
}