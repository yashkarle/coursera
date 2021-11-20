library("dplyr")

rankhospital <- function(state, outcome, num="best") {
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
  
  ## Return hospital name in that state with the given rank 30-day death rate
  stateOutcomeData <- outcomeData[outcomeData$State == state,]
  outcome_column_list <- valid_outcomes[outcome]
  outcome_column <- outcome_column_list[[1]]
  
  stateOutcomeData <- stateOutcomeData[order(as.numeric(stateOutcomeData[[outcome_column]]), stateOutcomeData$Hospital.Name), ]
  stateOutcomeData <- stateOutcomeData[!(stateOutcomeData[[outcome_column]]=="Not Available"),]
  #stateOutcomeData <- mutate(stateOutcomeData, row_num=row_number())
  
  if (num == "best") {
    num <- 1
  }
  if (num == "worst") {
    num <- nrow(stateOutcomeData)
  }
  stateOutcomeData[num, ]['Hospital.Name']
}