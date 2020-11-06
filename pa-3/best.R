best <- function(state, outcome) {
  options(warn=-1)
  ## Read outcome data
  outcomes <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
  index <- NULL
  ## Check that state and outcome are valid
  if (outcome == 'heart failure') {
    index <- 17
  } else if (outcome == 'heart attack') {
    index <- 11
  } else if (outcome == "pneumonia") {
    index <- 23
  }
  if (is.null(index)) {
    stop("invalid outcome")
  }
  outcome_rates <- outcomes[outcomes$State == state, c(2, index)]
  if (nrow(outcome_rates) == 0) {
    stop("invalid state")
  }
  outcome_rates[,2] <- as.numeric(outcome_rates[,2])
  outcome_rates <- outcome_rates[!is.na(outcome_rates[,2]),]
  outcome_rates<-outcome_rates[order(outcome_rates[,2], outcome_rates[,1]),]
  ## Return hospital name in that state with lowest 30-day death
  outcome_rates[1,1]
  ## rate
}