 outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
 head(outcome)
 outcome[, 11] <- as.numeric(outcome[, 11])
 names(outcome)
 hist(outcome[, 11], main = "Hospital 30 Day Death Mortality Rates from Heart Attack",
      xlab= "Motrality Rate")
 