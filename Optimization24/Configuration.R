#####################################
### Konfiguracja 
#####################################

# definicja
setClass("Configuration",
  slots = list(
	K7         = "vector",
	SRS_TZ1    = "vector",
    TZ1        = "vector",
    K1         = "vector",
    K6         = "vector",
    TZ2        = "vector",
    SRS_TZ2_06 = "vector",
    SRS_TZ2_13 = "vector",
    K4         = "vector",
    K5         = "vector",
    TZ4        = "vector",
    SRS_TZ4    = "vector",
    KS4        = "vector",
    TZ5        = "vector",
    SRS_TZ5_06 = "vector",
    SRS_TZ5_13 = "vector",
    SRS_TZ5_25 = "vector"
  ),
)

# konstruktor
Configuration = function(x) {
    new("Configuration",
        K7         = c(x[(24 * 0 + 1):(24 + 24 * 0)]),
        SRS_TZ1    = c(x[(24 * 1 + 1):(24 + 24 * 1)]),
        TZ1        = c(x[(24 * 2 + 1):(24 + 24 * 2)]),
        K1         = c(x[(24 * 3 + 1):(24 + 24 * 3)]),
        K6         = c(x[(24 * 4 + 1):(24 + 24 * 4)]),
        TZ2        = c(x[(24 * 5 + 1):(24 + 24 * 5)]),
        SRS_TZ2_06 = c(x[(24 * 6 + 1):(24 + 24 * 6)]),
        SRS_TZ2_13 = c(x[(24 * 7 + 1):(24 + 24 * 7)]),
        K4         = c(x[(24 * 8 + 1):(24 + 24 * 8)]),
        K5         = c(x[(24 * 9 + 1):(24 + 24 * 9)]),
        TZ4        = c(x[(24 * 10 + 1):(24 + 24 * 10)]),
        SRS_TZ4    = c(x[(24 * 11 + 1):(24 + 24 * 11)]),
        KS4        = c(x[(24 * 12 + 1):(24 + 24 * 12)]),
        TZ5        = c(x[(24 * 13 + 1):(24 + 24 * 13)]),
        SRS_TZ5_06 = c(x[(24 * 14 + 1):(24 + 24 * 14)]),
        SRS_TZ5_13 = c(x[(24 * 15 + 1):(24 + 24 * 15)]),
        SRS_TZ5_25 = c(x[(24 * 16 + 1):(24 + 24 * 15)]))
}       