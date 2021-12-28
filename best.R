## Take an abbreviated state (i.e. "MN) and a outcome name and return the name
## of the hospital with the best (lowest) mortality rate. Potential outcomes are
## included in the outcomes vector.

best <- function (state, outcome){
  # load csv file
  data <- read.csv(file.path("Data", "outcome-of-care-measures.csv"))
  
  # verify state is valid
  if (state %in% unique(data$State)){
    
    # subset by hospital name
    state_data <- data[data$State == state,]
    
    # change outcome columns to numerics
    suppressWarnings(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- 
        as.numeric(as.character(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)))
    
    suppressWarnings(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- 
        as.numeric(as.character(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)))
    
    suppressWarnings(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- 
        as.numeric(as.character(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)))
    
    # verify outcome is valid and sort by outcome, if there is a tie for the best
    # outcome between multiple hospitals, return the first hospital by 
    # alphabetical order
    if (outcome == "heart attack"){
      suppressWarnings(best_outcome <- min(na.omit(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)))
      filtered_data <- state_data[state_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == best_outcome
                                  & !is.na(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
    }
    else if (outcome == "heart failure"){
      suppressWarnings(best_outcome <- min(na.omit(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)))
      filtered_data <- state_data[state_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == best_outcome
                                  & !is.na(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
    }
    else if (outcome == "pneumonia"){
      suppressWarnings(best_outcome <- min(na.omit(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)))
      filtered_data <- state_data[state_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == best_outcome
                                  & !is.na(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
    }
    else{
      stop(error="invalid outcome")
    }
    
    
    if (nrow(filtered_data) > 1){
      alphabetical_data <- filtered_data[order(filtered_data$Hospital.Name),]
      alphabetical_data$Hospital.Name[1]
    }
    else{
      filtered_data$Hospital.Name[1]
    }
  }
  else{
    stop(error="invalid state")
  }
}