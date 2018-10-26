penalty <- function(x) {
    var <- Variables(x)
    result <- c()

    for (i in 1:24) {
        result <- c(result, var@mST_TZ1_up13[i] * 10)
    }

    return(result)
}

grad_penalty <- function(x) {
    var <- Variables(x)
    grad <- c()

    for (i in 1:24) {
        grad[(24 * 0 + 0) + 1 * i] = 0
        grad[(24 * 1 + 0) + 1 * i] = 0
        grad[(24 * 2 + 0) + 1 * i] = 10
        grad[(24 * 3 + 0) + 1 * i] = 0
        grad[(24 * 4 + 0) + 1 * i] = 0
        grad[(24 * 5 + 0) + 1 * i] = 0
        grad[(24 * 6 + 0) + 1 * i] = 0
        grad[(24 * 7 + 0) + 1 * i] = 0
        grad[(24 * 8 + 0) + 1 * i] = 0
        grad[(24 * 9 + 0) + 1 * i] = 0
        grad[(24 * 10 + 0) + 1 * i] = 0
        grad[(24 * 11 + 0) + 1 * i] = 0
        grad[(24 * 12 + 0) + 1 * i] = 0
        grad[(24 * 13 + 0) + 1 * i] = 0
        grad[(24 * 14 + 0) + 1 * i] = 0
        grad[(24 * 15 + 0) + 1 * i] = 0
        grad[(24 * 16 + 0) + 1 * i] = 0
        grad[(24 * 17 + 0) + 1 * i] = 0
        grad[(24 * 18 + 0) + 1 * i] = 0
        grad[(24 * 19 + 0) + 1 * i] = 0
        grad[(24 * 20 + 0) + 1 * i] = 0
    }

    return(grad)
}