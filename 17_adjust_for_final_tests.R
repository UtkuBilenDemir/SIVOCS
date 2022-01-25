# NOTE TO SELF: Remember, the correct and efficient way 
# to that thing you insist to do wrongly is
# as.numeric(levels(f))[f]


# Data Frame
source("./02_analysis/02_static_responses.R")

#fata.questions[fata.questions["age"] == "", "age"] <- NA

# Find the factor columns
fac_cols <- which(sapply(FUN = is.factor, fata.questions))


# ------------ Refactor the factor columns
# TODO: We are working with fata.questions right now, 
# consider doing the same changes with fata
ny <- c(0, 1)
ony <- c(NA, 0, 1)
nature <- c(NA, 3, 2, 0, 1)
effects <- c(NA, 2, 1, 3, NA, 0)

levels(fata.questions[, fac_cols[1]]) <- c(NA, 0, 1, 2, 3, 4)
levels(fata.questions[, fac_cols[2]])  <- c(NA, 0, 1, 2)
levels(fata.questions[, fac_cols[3]])  <- ny
levels(fata.questions[, fac_cols[4]]) <- ny
levels(fata.questions[, fac_cols[5]]) <- ny
levels(fata.questions[, fac_cols[6]]) <- ny
levels(fata.questions[, fac_cols[7]]) <- ny
levels(fata.questions[, fac_cols[8]]) <- ny
levels(fata.questions[, fac_cols[9]]) <- nature
levels(fata.questions[, fac_cols[10]]) <- nature
levels(fata.questions[, fac_cols[11]]) <- nature
levels(fata.questions[, fac_cols[12]]) <- nature
levels(fata.questions[, fac_cols[12]]) <- nature
levels(fata.questions[, fac_cols[13]]) <- nature
levels(fata.questions[, fac_cols[14]]) <- nature
levels(fata.questions[, fac_cols[15]]) <- nature
levels(fata.questions[, fac_cols[16]]) <- ony
levels(fata.questions[, fac_cols[17]]) <- ony
levels(fata.questions[, fac_cols[18]]) <- effects
levels(fata.questions[, fac_cols[19]]) <- effects
levels(fata.questions[, fac_cols[20]]) <- effects
levels(fata.questions[, fac_cols[21]]) <- effects
levels(fata.questions[, fac_cols[22]]) <- effects
levels(fata.questions[, fac_cols[23]]) <- effects
levels(fata.questions[, fac_cols[24]]) <- effects
levels(fata.questions[, fac_cols[25]]) <- ny
levels(fata.questions[, fac_cols[26]]) <- ny
levels(fata.questions[, fac_cols[27]]) <- ny

# Split META and FEATURE variables


as.numeric(levels(fata.questions[, fac_cols[18]]))[fata.questions[, fac_cols[18]]]
