library("dplyr")

rankall <- function(outcome, num="best") {
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that outcome specified is valid
  valid_outcomes <- list (
    "heart attack"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
    "heart failure"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", 
    "pneumonia"="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  )
  if (!(outcome %in% names(valid_outcomes))) {
    stop("invalid outcome")
  }
  
  ## For each state, find the hospital of the given rank
  outcome_column_list <- valid_outcomes[outcome]
  outcome_column <- outcome_column_list[[1]]
  
  outcomeData <- outcomeData[order(outcomeData$State, as.numeric(outcomeData[[outcome_column]]), outcomeData$Hospital.Name), ]
  outcomeData <- outcomeData[!(outcomeData[[outcome_column]]=="Not Available"),]
  
  if (num == "worst") {
    rankedOutcomeData <- outcomeData %>% group_by(State) %>% mutate(id=row_number()) %>% filter(row_number()==n())
  } else {
    if (num == "best") {
      num <- 1
    }
    rankedOutcomeData <- outcomeData %>% group_by(State) %>% mutate(id=row_number()) %>% filter(row_number()==num)
  }
  
  ## Return a data frame with the hospital names and the (abbreviated) state name
  rankedOutcomeData %>% select(Hospital.Name, State)
}