best <- function(state, outcome) {
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (!(state %in% outcomeData$State)) {
    stop("invalid state")
  }
  
  valid_outcomes <- list (
    "heart attack"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
    "heart failure"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", 
    "pneumonia"="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  )
  if (!(outcome %in% names(valid_outcomes))) {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
  stateOutcomeData <- outcomeData[outcomeData$State == state,]
  outcome_column_list <- valid_outcomes[outcome]
  outcome_column <- outcome_column_list[[1]]
  
  stateOutcomeData[which.min(stateOutcomeData[[outcome_column]]),2]
}