# bez swingu
calcOptions.swing <- FALSE
source("Model.R")
no_swing_costs <- optim$objective
# ze swingiem
calcOptions.swing <- TRUE
source("Model.R")
with_swing_costs <- optim$objective
# porównanie
cat("\nNo swing costs: ", no_swing_costs)
cat("\nWith swing costs: ", with_swing_costs)
cat("\nDelta: ", no_swing_costs - with_swing_costs)
cat("\n sd(c_RDN)", sd(c_RDN))