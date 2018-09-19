########################################################
### Constraints - ograniczenia zmiennych manipulacyjnych
########################################################

# TZ - 1
mST_TZ1_in.def = 200
mST_TZ1_in.min = 135
mST_TZ1_in.max = 265
mST_TZ1_up25.def = 20
mST_TZ1_up25.min = 0
mST_TZ1_up25.max = 20
mST_TZ1_up13.def = 5
mST_TZ1_up13.min = 0
mST_TZ1_up13.max = 60
mST_TZ1_up06.def = 190
mST_TZ1_up06.min = 135
mST_TZ1_up06.max = 240

# TZ - 2
mST_TZ2_in.def = 280
mST_TZ2_in.min = 105
mST_TZ2_in.max = 300
mST_TZ2_up25.def = 8
mST_TZ2_up25.min = 0
mST_TZ2_up25.max = 27
mST_TZ2_up13.def = 43
mST_TZ2_up13.min = 0
mST_TZ2_up13.max = 100
mST_TZ2_up06.def = 202
mST_TZ2_up06.min = 0
mST_TZ2_up06.max = 205
mST_TZ2_kond.def = 52
mST_TZ2_kond.min = 20
mST_TZ2_kond.max = 70

# TZ - 5
mST_TZ5_in.def = 333
mST_TZ5_in.min = 150
mST_TZ5_in.max = 440
mST_TZ5_up25.def = 5
mST_TZ5_up25.min = 0
mST_TZ5_up25.max = 55
mST_TZ5_up13.def = 80
mST_TZ5_up13.min = 0
mST_TZ5_up13.max = 80
mST_TZ5_up06.def = 150
mST_TZ5_up06.min = 35
mST_TZ5_up06.max = 400
mST_TZ5_kond.def = 150
mST_TZ5_kond.min = 50
mST_TZ5_kond.max = 150

constraintsDefault = c()
constraintsLB = c()
constraintsUB = c()

#for (i in 1:24) {
    #constraintsDefault <- c(constraintsDefault, mST_TZ1_in.def, mST_TZ1_up25.def, mST_TZ1_up13.def, mST_TZ1_up06.def, 
     #mST_TZ2_in.def, mST_TZ2_up25.def, mST_TZ2_up13.def, mST_TZ2_up06.def, mST_TZ2_kond.def,
     #mST_TZ5_in.def, mST_TZ5_up25.def, mST_TZ5_up13.def, mST_TZ5_up06.def, mST_TZ5_kond.def)

    #constraintsLB <- c(constraintsLB, mST_TZ1_in.min, mST_TZ1_up25.min, mST_TZ1_up13.min, mST_TZ1_up06.min,
     #mST_TZ2_in.min, mST_TZ2_up25.min, mST_TZ2_up13.min, mST_TZ2_up06.min, mST_TZ2_kond.min,
     #mST_TZ5_in.min, mST_TZ5_up25.min, mST_TZ5_up13.min, mST_TZ5_up06.min, mST_TZ5_kond.min)

    #constraintsUB <- c(constraintsUB, mST_TZ1_in.max, mST_TZ1_up25.max, mST_TZ1_up13.max, mST_TZ1_up06.max, 
     #mST_TZ2_in.max, mST_TZ2_up25.max, mST_TZ2_up13.max, mST_TZ2_up06.max, mST_TZ2_kond.max,
     #mST_TZ5_in.max, mST_TZ5_up25.max, mST_TZ5_up13.max, mST_TZ5_up06.max, mST_TZ5_kond.max)
#}

constraintsDefault <- c(constraintsDefault, rep(mST_TZ1_in.def, 24))
constraintsDefault <- c(constraintsDefault, rep(mST_TZ1_up25.def, 24))
constraintsDefault <- c(constraintsDefault, rep(mST_TZ1_up13.def, 24))
constraintsDefault <- c(constraintsDefault, rep(mST_TZ1_up06.def, 24))
constraintsDefault <- c(constraintsDefault, rep(mST_TZ2_in.def, 24))
constraintsDefault <- c(constraintsDefault, rep(mST_TZ2_up25.def, 24))
constraintsDefault <- c(constraintsDefault, rep(mST_TZ2_up13.def, 24))
constraintsDefault <- c(constraintsDefault, rep(mST_TZ2_up06.def, 24))
constraintsDefault <- c(constraintsDefault, rep(mST_TZ2_kond.def, 24))
constraintsDefault <- c(constraintsDefault, rep(mST_TZ5_in.def, 24))
constraintsDefault <- c(constraintsDefault, rep(mST_TZ5_up25.def, 24))
constraintsDefault <- c(constraintsDefault, rep(mST_TZ5_up13.def, 24))
constraintsDefault <- c(constraintsDefault, rep(mST_TZ5_up06.def, 24))
constraintsDefault <- c(constraintsDefault, rep(mST_TZ5_kond.def, 24))

constraintsLB <- c(constraintsLB, rep(mST_TZ1_in.min, 24))
constraintsLB <- c(constraintsLB, rep(mST_TZ1_up25.min, 24))
constraintsLB <- c(constraintsLB, rep(mST_TZ1_up13.min, 24))
constraintsLB <- c(constraintsLB, rep(mST_TZ1_up06.min, 24))
constraintsLB <- c(constraintsLB, rep(mST_TZ2_in.min, 24))
constraintsLB <- c(constraintsLB, rep(mST_TZ2_up25.min, 24))
constraintsLB <- c(constraintsLB, rep(mST_TZ2_up13.min, 24))
constraintsLB <- c(constraintsLB, rep(mST_TZ2_up06.min, 24))
constraintsLB <- c(constraintsLB, rep(mST_TZ2_kond.min, 24))
constraintsLB <- c(constraintsLB, rep(mST_TZ5_in.min, 24))
constraintsLB <- c(constraintsLB, rep(mST_TZ5_up25.min, 24))
constraintsLB <- c(constraintsLB, rep(mST_TZ5_up13.min, 24))
constraintsLB <- c(constraintsLB, rep(mST_TZ5_up06.min, 24))
constraintsLB <- c(constraintsLB, rep(mST_TZ5_kond.min, 24))

constraintsUB <- c(constraintsUB, rep(mST_TZ1_in.max, 24))
constraintsUB <- c(constraintsUB, rep(mST_TZ1_up25.max, 24))
constraintsUB <- c(constraintsUB, rep(mST_TZ1_up13.max, 24))
constraintsUB <- c(constraintsUB, rep(mST_TZ1_up06.max, 24))
constraintsUB <- c(constraintsUB, rep(mST_TZ2_in.max, 24))
constraintsUB <- c(constraintsUB, rep(mST_TZ2_up25.max, 24))
constraintsUB <- c(constraintsUB, rep(mST_TZ2_up13.max, 24))
constraintsUB <- c(constraintsUB, rep(mST_TZ2_up06.max, 24))
constraintsUB <- c(constraintsUB, rep(mST_TZ2_kond.max, 24))
constraintsUB <- c(constraintsUB, rep(mST_TZ5_in.max, 24))
constraintsUB <- c(constraintsUB, rep(mST_TZ5_up25.max, 24))
constraintsUB <- c(constraintsUB, rep(mST_TZ5_up13.max, 24))
constraintsUB <- c(constraintsUB, rep(mST_TZ5_up06.max, 24))
constraintsUB <- c(constraintsUB, rep(mST_TZ5_kond.max, 24))