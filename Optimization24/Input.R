###############################
### Input - wielko�ci wej�ciowe
###############################

configEC = new("Configuration",
    K7         = c(rep(TRUE, times = 24)),
    SRS_TZ1    = c(rep(FALSE, times = 24)),
    TZ1        = c(rep(TRUE, times = 24)),
    ###
    K1         = c(rep(TRUE, times = 24)),
    K6         = c(rep(TRUE, times = 24)),
    TZ2        = c(rep(TRUE, times = 24)),
    SRS_TZ2_06 = c(rep(FALSE, times = 24)),
    SRS_TZ2_13 = c(rep(FALSE, times = 24)),
    ###
    K4         = c(rep(FALSE, times = 24)),
    K5         = c(rep(FALSE, times = 24)),
    TZ4        = c(rep(FALSE, times = 24)),
    SRS_TZ4    = c(rep(FALSE, times = 24)),
    ###
    KS4        = c(rep(TRUE, times = 24)),
    TZ5        = c(rep(TRUE, times = 24)),
    SRS_TZ5_06 = c(rep(FALSE, times = 24)),
    SRS_TZ5_13 = c(rep(FALSE, times = 24)),
    SRS_TZ5_25 = c(rep(FALSE, times = 24))
)

# Zapotrzebowanie - Zestaw 1
zap_par_25 = c(46, 46, 46, 46, 46, 46, 47, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 45, 45, 45)
zap_par_13 = c(94, 94, 94, 94, 94, 94, 97, 90, 98, 98, 98, 98, 98, 98, 101, 103, 98, 97, 97, 97, 97, 88, 91, 92)
zap_par_06 = c(526, 526, 526, 526, 537, 538, 536, 489, 550, 550, 550, 550, 550, 551, 550, 557, 558, 555, 556, 556, 556, 499, 490, 492)
zap_el = c(145, 145, 145, 145, 144, 144, 145, 130, 149, 149, 149, 147, 147, 147, 149, 153, 153, 151, 152, 150, 150, 134, 133, 133)
mST_KS4 = c(352, 352, 352, 352, 349, 349, 349, 330 + 29, 373, 373, 373, 373, 373, 373, 373, 373, 373, 373, 373, 373, 373, 352, 352, 352)

### Zapotrzebowanie - Zestaw 2
#zap_par_25 = c(46, 46, 46, 46, 46, 46, 45, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 47, 47, 47, 45, 46, 46)
#zap_par_13 = c(96, 97, 97, 97, 97, 97, 101, 96, 90, 89, 89, 89, 89, 92, 95, 95, 90, 90, 94, 94, 94, 95, 98, 100)
#zap_par_06 = c(565, 567, 567, 567, 567, 569, 564, 547, 550, 534, 534, 534, 534, 555, 554, 547, 549, 549, 543, 552, 548, 550, 566, 568)
#zap_el = c(149, 150, 150, 150, 150, 151, 150, 143, 143, 139, 139, 139, 139, 144, 144, 142, 142, 142, 141, 145, 145, 146, 149, 150)
#mST_KS4 = c(356.7, 359.4, 359.4, 359.4, 359.4, 358.5, 357.4, 320.2, 320.2, 307.3, 307.3, 307.3, 307.3, 325.3, 325.3, 322.3, 322.3, 322.3, 343.7, 343.7, 343.7, 354.7, 354.7, 354.7)

## Zapotrzebowanie - Zestaw 3 (05-10-2018)
#zap_par_25 = c(45, 45, 45, 45, 46, 46, 45, 46, 45, 45, 45, 45, 45, 45, 45, 46, 45, 45, 45, 45, 45, 45, 45, 45)
#zap_par_13 = c(99, 99, 99, 99, 99, 97, 100, 102, 86, 86, 86, 86, 86, 86, 89, 92, 87, 87, 87, 87, 87, 87, 90, 92)
#zap_par_06 = c(541 + 30, 551, 551, 549, 547, 530, 531, 536, 509, 509, 509, 509, 508, 511, 512, 518, 512, 508, 508, 508, 508, 508, 508, 517)
#zap_el = c(141, 144, 144, 144, 143, 139, 139, 139, 129, 129, 129, 129, 129, 130, 130, 132, 131, 130, 130, 128, 130, 130, 130, 134)
#mST_KS4 = c(376.7, 376.7, 376.7, 376.7, 375, 371.5, 371.5, 371.5, 292.5, 292.5, 292.5, 292.5, 292.5, 295.7, 295.7, 302.6, 297.4, 296.1, 296.1, 296.1, 296.1, 296.1, 296.1, 296.1)

# Ceny - Zestaw 1 M=340, SD=85.7 (du�a rozpi�to��)
#c_RDN = c(239.26, 236.30, 233.07, 234.21, 236.65, 249.85, 281.72, 331.59,
          #359.07, 386.51, 384.94, 451.63, 469.59, 470.23, 388.68, 359.14,
          #336.80, 329.16, 336.35, 456.54, 505.25, 326.65, 279.27, 271.54)
#swing30: 374394.447927569 no: 375924.293001922 (1529)
#swing50: 373780.068197463 no: 375924.293001922 (2144)

# Ceny - Zestaw 2 (PN 01-10-2018) M=277, SD=59.7
#c_RDN = c(196.93, 197.35, 197.18, 195.86, 195.65, 203.67, 269.34, 285.72,
          #325.94, 327.74, 318.39, 326.79, 326.79, 326.84, 319.18, 301.49,
          #288.38, 295.20, 313.97, 404.43, 327.02, 264.47, 237.81, 203.75) 
#swing30: 382604.474647003 no: 383656.531218157 (1052)
#swing50: 382295.710137903 no: 383656.531218157 (1360)

# Ceny - Zestaw 3 (ND 23-09-2018) M=219, SD=36.6
#c_RDN = c(197.60, 192.19, 190.56, 187.95, 187.05, 188.16, 192.15, 196.45,
          #204.64, 225.94, 229.25, 226.02, 224.95, 212.20, 207.78, 209.38,
          #215.63, 223.63, 252.00, 342.19, 301.31, 243.76, 208.86, 203.30)
#swing30: 389765.034065118 no: 390253.05449404 (488)
#swing50: 389536.535436012 no: 390253.05449404 (716)

# Ceny - Zestaw 4 (CZ 11-01-2018) M=216, SD=70.2
c_RDN = c(142.66, 143.48, 140.37, 136.22, 136.65, 145.79, 170.99, 224.14,
          234.56, 239.16, 243.78, 254.43, 263.97, 253.61, 236.94, 237.04,
          398.36, 376.39, 259.29, 242.38, 214.15, 188.93, 163.77, 148.26)
#swing30: 390513.493156982 no: 391661.343007206 (1147)
#swing50: 390083.194825026 no: 391661.343007206 (1578)

# Ceny - Zestaw 5 (PN 07-05-2018) M=325, SD=144.9 (b. du�a rozpi�to��)
#c_RDN = c(194.50, 195.30, 192.23, 196.42, 196.44, 197.27, 211.86, 259.72,
          #433.94, 528.61, 480.61, 590.72, 609.52, 597.15, 432.21, 377.05,
          #306.85, 271.38, 260.07, 265.31, 355.67, 255.51, 204.74, 197.34)
#swing30: 375373.64155739 no: 377988.826128776 (2615)
#swing50: 374010.31194115 no: 377988.826128776 (3978)

# Ceny - Zestaw 6 (CZ 14-12-2017) M =146, SD=30.8 (niskie ceny)
#c_RDN = c(112.00, 105.00, 90.00, 85.21, 100.00, 112.00, 125.00, 151.68,
          #159.40, 162.40, 163.00, 167.16, 166.73, 168.66, 167.91, 170.85,
          #192.70, 185.91, 172.13, 164.90, 159.80, 151.56, 140.41, 133.00)

# Ceny - Zestaw 7 (PT 05-10-2018) M=300, SD=85.7
#c_RDN = c(217,200,198,198,198,216,272,311,
          #334,334,334,334,334,334,334,334,
          #317,334,350,599,347,289,250,225)

#c_RDN = c_RDN - c(rep(90, times = 24)) #  ca�y profil cen w gore/dol

#cminus = - 10                          #  zr�nicuj profil cen         
#cplus = +50
#c_RDN = c_RDN + c(rep(cminus,times=8),
                  #rep(cplus, times = 8),
                  #rep(cminus, times = 8))

costTZ1 = 25  # PLN / t pary
costTZ2 = 20  # PLN / t pary
costTZ4 = 70  # PLN / t pary
costTZ5 = 15  # PLN / t pary
steamCostTZ1 = rep(costTZ1,times=24)
steamCostTZ2 = rep(costTZ2,times=24)
steamCostTZ4 = rep(costTZ4,times=24)
steamCostTZ5 = rep(costTZ5, times = 24)

#c_RDN[18] = c_RDN[18] + 100
#c_RDN[19] = c_RDN[18] + 100
zap_par_25 = zap_par_25 - c(rep(44, times = 24)) # 30-KS4, 14-EC
zap_par_13 = zap_par_13 - c(rep(16, times = 24)) # 16-EC
zap_par_06 = zap_par_06 - c(rep(45, times = 24)) # 45-EC