rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  valid_states <- sort(unique(outcome_data$State))
  
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
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  hospital <- rep("", length(valid_states))
  
  for (state in valid_states) {
    subset_data <- outcome_data[outcome_data$State == state, c(sel_col, "Hospital.Name"),]
    ordered_subset_data <- subset_data[order(subset_data[sel_col], subset_data$Hospital.Name, na.last = NA),]
    if (num == "best") {
      hospital[which(valid_states == state)] <- ordered_subset_data[1,2]
    }else if (num == "worst") {
      hospital[which(valid_states == state)] <- ordered_subset_data[nrow(ordered_subset_data),2]
    }else {
      hospital[which(valid_states == state)] <- ordered_subset_data[num,2]
    }
  }

  return(data.frame(hospital=hospital, state=valid_states))

}

