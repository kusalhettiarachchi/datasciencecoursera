rankall <-  function(outcome, num = "best") {
  options(warn=-1)
  result<-data.frame()
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
  rankHospital <- function(state, index, num = 'best') {
    outcome_rates <- outcomes[outcomes$State == state, c(2, index)]
    outcome_rates[,2] <- as.numeric(outcome_rates[,2])
    outcome_rates <- outcome_rates[!is.na(outcome_rates[,2]),]
    outcome_rates <- outcome_rates[order(outcome_rates[,2], outcome_rates[,1]),]
    ## Return hospital name in that state with the given rank
    if (num == 'best') {
      num<-1
    } else if (num == 'worst') {
      num<-nrow(outcome_rates)
    } 
    if (num > nrow(outcome_rates)) {
      result<<-rbind(result, c(NA, state))
      return()
    }
    result<<-rbind(result, c(outcome_rates[num,1], state))
  }
  lapply(sort(unique(outcomes$State)), rankHospital, index = index, num = num)
  names(result)<-c("hospital", "state")
  rownames(result) <- result$state
  result
}