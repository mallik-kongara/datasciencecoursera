best <- function(state, outcome) {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  valid_states <- unique(outcome_data$State)
  
  ## Check that state and outcome are valid
  if (!state %in% valid_states) {
    stop("invalid state")
  }
  
  sel_col <- if (outcome == "heart attack") {
    outcome_data[,11] <- suppressWarnings(as.numeric(outcome_data[,11]))
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  }else if (outcome == "heart failure") {
    outcome_data[,17] <- suppressWarnings(as.numeric(outcome_data[,17]))
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  }else if (outcome == "pneumonia") {
    outcome_data[,23] <- suppressWarnings(as.numeric(outcome_data[,23]))
    "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  }else {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  subset_data <- outcome_data[outcome_data$State == state, c(sel_col, "Hospital.Name"),]
  ordered_subset_data <- subset_data[order(subset_data[sel_col], subset_data$Hospital.Name),]
  ordered_subset_data[1,2]
}

## Tests and results:
## > best("TX", "heart attack")
## [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
## > best("TX", "heart failure")
## [1] "FORT DUNCAN MEDICAL CENTER"
## > best("MD", "heart attack")
## [1] "JOHNS HOPKINS HOSPITAL, THE"
## > best("MD", "pneumonia")
## [1] "GREATER BALTIMORE MEDICAL CENTER"
## > best("BB", "heart attack")
## Error in best("BB", "heart attack") : invalid state
## > best("NY", "hert attack")
## Error in best("NY", "hert attack") : invalid outcome