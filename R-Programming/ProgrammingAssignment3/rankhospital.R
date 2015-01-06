rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  valid_states <- unique(outcome_data$State)
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
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  subset_data <- outcome_data[outcome_data$State == state, c(sel_col, "Hospital.Name"),]
  ordered_subset_data <- subset_data[order(subset_data[sel_col], subset_data$Hospital.Name, na.last = NA),]
  
  if (num == "best") {
    ordered_subset_data[1,2]
  }else if (num == "worst") {
    ordered_subset_data[nrow(ordered_subset_data),2]
  }else {
    ordered_subset_data[num,2]
  }
}

## Tests and results:
## > rankhospital("TX", "heart failure", 4)
## [1] "DETAR HOSPITAL NAVARRO"
## > rankhospital("MD", "heart attack", "worst")
## [1] "HARFORD MEMORIAL HOSPITAL"
## > rankhospital("MN", "heart attack", 5000)
## [1] NA