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
mST_TZ1_wyd.def = 0
mST_TZ1_wyd.min = ifelse(calcOptions.wydmuch == TRUE, 0, 0)
mST_TZ1_wyd.max = ifelse(calcOptions.wydmuch == TRUE, 100, 0)


# TZ - 2
mST_TZ2_in.def = 300
mST_TZ2_in.min = 105
mST_TZ2_in.max = 300
mST_TZ2_up25.def = 0
mST_TZ2_up25.min = 0
mST_TZ2_up25.max = 27
mST_TZ2_up13.def = 50
mST_TZ2_up13.min = 0
mST_TZ2_up13.max = 100
mST_TZ2_up06.def = 180
mST_TZ2_up06.min = 0
mST_TZ2_up06.max = 205
mST_TZ2_kond.def = 70
mST_TZ2_kond.min = 20
mST_TZ2_kond.max = 70

# TZ - 4
mST_TZ4_in.def =  100
mST_TZ4_in.min =  80
mST_TZ4_in.max =  300
mST_TZ4_up25.def =0
mST_TZ4_up25.min =0
mST_TZ4_up25.max =20
mST_TZ4_up13.def =0
mST_TZ4_up13.min =0
mST_TZ4_up13.max =80
mST_TZ4_up06.def =100
mST_TZ4_up06.min =0
mST_TZ4_up06.max =200
mST_TZ4_wyd.def = 0
mST_TZ4_wyd.min = ifelse(calcOptions.wydmuch == TRUE, 0, 0)
mST_TZ4_wyd.max = ifelse(calcOptions.wydmuch == TRUE, 100, 0)

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

# Swing
mST_KS4_swing.def = 0
mST_KS4_swing.min = 0#ifelse(calcOptions.swing == TRUE,-50,0)
mST_KS4_swing.max = 0#ifelse(calcOptions.swing == TRUE, 50,0)

constraintsDefault = c()
constraintsLB = c()
constraintsUB = c()

for (i in 1:24) {
    constraintsDefault[0*24+i] <- ifelse(configEC@TZ1[i] == TRUE, mST_TZ1_in.def, 0)
    constraintsDefault[1*24+i] <- ifelse(configEC@TZ1[i] == TRUE, mST_TZ1_up25.def, 0)
    constraintsDefault[2*24+i] <- ifelse(configEC@TZ1[i] == TRUE, mST_TZ1_up13.def, 0)
    constraintsDefault[3*24+i] <- ifelse(configEC@TZ1[i] == TRUE, mST_TZ1_up06.def, 0)
    constraintsDefault[4*24+i] <- ifelse(configEC@TZ1[i] == TRUE, mST_TZ1_wyd.def, 0)
    constraintsDefault[5*24+i] <- ifelse(configEC@TZ2[i] == TRUE, mST_TZ2_in.def, 0)
    constraintsDefault[6*24+i] <- ifelse(configEC@TZ2[i] == TRUE, mST_TZ2_up25.def, 0)
    constraintsDefault[7*24+i] <- ifelse(configEC@TZ2[i] == TRUE, mST_TZ2_up13.def, 0)
    constraintsDefault[8*24+i] <- ifelse(configEC@TZ2[i] == TRUE, mST_TZ2_up06.def, 0)
    constraintsDefault[9*24+i] <- ifelse(configEC@TZ2[i] == TRUE, mST_TZ2_kond.def, 0)
    constraintsDefault[10*24+i] <- ifelse(configEC@TZ4[i] == TRUE, mST_TZ4_in.def, 0)
    constraintsDefault[11*24+i] <- ifelse(configEC@TZ4[i] == TRUE, mST_TZ4_up25.def, 0)
    constraintsDefault[12*24+i] <- ifelse(configEC@TZ4[i] == TRUE, mST_TZ4_up13.def, 0)
    constraintsDefault[13*24+i] <- ifelse(configEC@TZ4[i] == TRUE, mST_TZ4_up06.def, 0)
    constraintsDefault[14*24+i] <- ifelse(configEC@TZ4[i] == TRUE, mST_TZ4_wyd.def, 0)
    constraintsDefault[15*24+i] <- ifelse(configEC@TZ5[i] == TRUE, mST_TZ5_in.def, 0)
    constraintsDefault[16*24+i] <- ifelse(configEC@TZ5[i] == TRUE, mST_TZ5_up25.def, 0)
    constraintsDefault[17*24+i] <- ifelse(configEC@TZ5[i] == TRUE, mST_TZ5_up13.def, 0)
    constraintsDefault[18*24+i] <- ifelse(configEC@TZ5[i] == TRUE, mST_TZ5_up06.def, 0)
    constraintsDefault[19*24+i] <- ifelse(configEC@TZ5[i] == TRUE, mST_TZ5_kond.def, 0)
    constraintsDefault[20*24+i] <- ifelse(calcOptions.swing == TRUE, mST_KS4_swing.def, 0)

    constraintsLB[0*24+i] <- ifelse(configEC@TZ1[i] == TRUE, mST_TZ1_in.min, 0)
    constraintsLB[1*24+i] <- ifelse(configEC@TZ1[i] == TRUE, mST_TZ1_up25.min, 0)
    constraintsLB[2*24+i] <- ifelse(configEC@TZ1[i] == TRUE, mST_TZ1_up13.min, 0)
    constraintsLB[3*24+i] <- ifelse(configEC@TZ1[i] == TRUE, mST_TZ1_up06.min, 0)
    constraintsLB[4*24+i] <- ifelse(configEC@TZ1[i] == TRUE, mST_TZ1_wyd.min, 0)
    constraintsLB[5*24+i] <- ifelse(configEC@TZ2[i] == TRUE, mST_TZ2_in.min, 0)
    constraintsLB[6*24+i] <- ifelse(configEC@TZ2[i] == TRUE, mST_TZ2_up25.min, 0)
    constraintsLB[7*24+i] <- ifelse(configEC@TZ2[i] == TRUE, mST_TZ2_up13.min, 0)
    constraintsLB[8*24+i] <- ifelse(configEC@TZ2[i] == TRUE, mST_TZ2_up06.min, 0)
    constraintsLB[9*24+i] <- ifelse(configEC@TZ2[i] == TRUE, mST_TZ2_kond.min, 0)
    constraintsLB[10*24+i] <- ifelse(configEC@TZ4[i] == TRUE, mST_TZ4_in.min, 0)
    constraintsLB[11*24+i] <- ifelse(configEC@TZ4[i] == TRUE, mST_TZ4_up25.min, 0)
    constraintsLB[12*24+i] <- ifelse(configEC@TZ4[i] == TRUE, mST_TZ4_up13.min, 0)
    constraintsLB[13*24+i] <- ifelse(configEC@TZ4[i] == TRUE, mST_TZ4_up06.min, 0)
    constraintsLB[14*24+i] <- ifelse(configEC@TZ4[i] == TRUE, mST_TZ4_wyd.min, 0)
    constraintsLB[15*24+i] <- ifelse(configEC@TZ5[i] == TRUE, mST_TZ5_in.min, 0)
    constraintsLB[16*24+i] <- ifelse(configEC@TZ5[i] == TRUE, mST_TZ5_up25.min, 0)
    constraintsLB[17*24+i] <- ifelse(configEC@TZ5[i] == TRUE, mST_TZ5_up13.min, 0)
    constraintsLB[18*24+i] <- ifelse(configEC@TZ5[i] == TRUE, mST_TZ5_up06.min, 0)
    constraintsLB[19*24+i] <- ifelse(configEC@TZ5[i] == TRUE, mST_TZ5_kond.min, 0)
    constraintsLB[20*24+i] <- ifelse(calcOptions.swing == TRUE, mST_KS4_swing.min, 0)

    constraintsUB[0*24+i] <- ifelse(configEC@TZ1[i] == TRUE, mST_TZ1_in.max, 0)
    constraintsUB[1*24+i] <- ifelse(configEC@TZ1[i] == TRUE, mST_TZ1_up25.max, 0)
    constraintsUB[2*24+i] <- ifelse(configEC@TZ1[i] == TRUE, mST_TZ1_up13.max, 0)
    constraintsUB[3*24+i] <- ifelse(configEC@TZ1[i] == TRUE, mST_TZ1_up06.max, 0)
    constraintsUB[4*24+i] <- ifelse(configEC@TZ1[i] == TRUE, mST_TZ1_wyd.max, 0)
    constraintsUB[5*24+i] <- ifelse(configEC@TZ2[i] == TRUE, mST_TZ2_in.max, 0)
    constraintsUB[6*24+i] <- ifelse(configEC@TZ2[i] == TRUE, mST_TZ2_up25.max, 0)
    constraintsUB[7*24+i] <- ifelse(configEC@TZ2[i] == TRUE, mST_TZ2_up13.max, 0)
    constraintsUB[8*24+i] <- ifelse(configEC@TZ2[i] == TRUE, mST_TZ2_up06.max, 0)
    constraintsUB[9*24+i] <- ifelse(configEC@TZ2[i] == TRUE, mST_TZ2_kond.max, 0)
    constraintsUB[10*24+i] <- ifelse(configEC@TZ4[i] == TRUE, mST_TZ4_in.max, 0)
    constraintsUB[11*24+i] <- ifelse(configEC@TZ4[i] == TRUE, mST_TZ4_up25.max, 0)
    constraintsUB[12*24+i] <- ifelse(configEC@TZ4[i] == TRUE, mST_TZ4_up13.max, 0)
    constraintsUB[13*24+i] <- ifelse(configEC@TZ4[i] == TRUE, mST_TZ4_up06.max, 0)
    constraintsUB[14*24+i] <- ifelse(configEC@TZ4[i] == TRUE, mST_TZ4_wyd.max, 0)
    constraintsUB[15*24+i] <- ifelse(configEC@TZ5[i] == TRUE, mST_TZ5_in.max, 0)
    constraintsUB[16*24+i] <- ifelse(configEC@TZ5[i] == TRUE, mST_TZ5_up25.max, 0)
    constraintsUB[17*24+i] <- ifelse(configEC@TZ5[i] == TRUE, mST_TZ5_up13.max, 0)
    constraintsUB[18*24+i] <- ifelse(configEC@TZ5[i] == TRUE, mST_TZ5_up06.max, 0)
    constraintsUB[19*24+i] <- ifelse(configEC@TZ5[i] == TRUE, mST_TZ5_kond.max, 0)
    constraintsUB[20*24+i] <- ifelse(calcOptions.swing == TRUE, mST_KS4_swing.max, 0)
}

# dodatkowe ograniczenia na swing zwi¹zane z przebiegiem ceny na RDN
if (calcOptions.swing == TRUE) {
    swingVariableIndex = 21
    for (i in 1:24) {
        if (c_RDN[i] < 0.93 * mean(c_RDN)) {
            constraintsLB[((swingVariableIndex - 1) * 24) + i] = -50
            constraintsUB[((swingVariableIndex - 1) * 24) + i] = -20
            constraintsDefault[((swingVariableIndex - 1) * 24) + i] = -50
        } else {
            constraintsLB[((swingVariableIndex - 1) * 24) + i] = 20
            constraintsUB[((swingVariableIndex - 1) * 24) + i] = 50
            constraintsDefault[((swingVariableIndex - 1) * 24) + i] = 50
        }
    }
}