rankall <- function(outcome, num = "best"){
  # load csv file
  data <- read.csv(file.path("Data", "outcome-of-care-measures.csv"))
  
  # change outcome columns to numerics
  suppressWarnings(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- 
                     as.numeric(as.character(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)))
  
  suppressWarnings(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- 
                     as.numeric(as.character(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)))
  
  suppressWarnings(data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- 
                     as.numeric(as.character(data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)))
  
  hospital <- c()
  state <- c()
  
  for (region in unique(data$State)){
    # subset by state
    state_data <- data[data$State == region,]
    
    # create sorted data frame
    if (outcome == "heart attack"){
      sorted_data <- 
        state_data[order(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                         state_data$Hospital.Name),]
      filtered_data <- sorted_data[!is.na(sorted_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
    }
    else if (outcome == "heart failure"){
      sorted_data <- 
        state_data[order(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                         state_data$Hospital.Name),]
      filtered_data <- sorted_data[!is.na(sorted_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
    }
    else if (outcome == "pneumonia"){
      sorted_data <- 
        state_data[order(state_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,
                         state_data$Hospital.Name),]
      filtered_data <- sorted_data[!is.na(sorted_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
    }
    else{
      stop(error="invalid outcome")
    }
    # append hospital and rank
    if (num == "best"){
      hospital <- append(hospital, filtered_data$Hospital.Name[1])
    }
    else if(num == "worst"){
      hospital <- append(hospital, filtered_data$Hospital.Name[nrow(filtered_data)])
    }
    else if(class(num) == "numeric"){
      hospital <- append(hospital, filtered_data$Hospital.Name[num])
    }
    else{
      stop(error="invalid num")
    }
    state <- append(state, region)
  }
  # return data frame, sorted alphabetically by state
  output <- data.frame(hospital, state, row.names = state)
  output[order(output$state),]
}