F_mST_25_KS4 = function() {

    mST <- c()
    for (i in 1:24) {
        mST_i = 0

        if (mST_KS4[i] < 150) {
            mST_i = 0
        } else if (mST_KS4[i] < 250) {
            mST_i = 0.0833 * mST_KS4[i] + 4.8535
        } else if (mST_KS4[i] < 350) {
            mST_i = 0.0312 * mST_KS4[i] + 20.165
        } else {
            mST_i = 0.0266 * mST_KS4[i] + 20.336
        }

        mST <- c(mST, mST_i)
    }

    return (mST)
}

G_mST_25_KS4 = function(x) {
    grad <- c()

    for (i in 1:24) {
        grad[[i]]=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    }

    return (grad)
}


