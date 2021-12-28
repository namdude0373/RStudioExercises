complete <- function(directory, id = 1:332){
  # return a data frame with each row containing an id and number of complete
  # observations
  
  # create vector of desired file names
  ids <- c()
  for (monitor in id){ # file names are monitor names
    id_padded <- formatC(monitor, width = 3, flag = 0)
    filename <- paste(id_padded, ".csv", sep = "")
    ids <- append(ids, filename)
  }
  
  id <- c() # overwriting id object so data frame names are more easily assigned
  nobs <- c()
  for (file in ids){ # ids is a vector of file names
    file_data <- read.table(file.path("Data", directory, file), header = TRUE, 
                            sep = ",")
    complete_observations <- complete.cases(file_data)
    complete_file_data <- file_data[complete_observations, ]
    # append id and nobs to vectors
    id <- append(id, c(complete_file_data[1,]$ID))
    nobs <- append(nobs, nrow(complete_file_data))
  }
  data.frame(id, nobs) # return data frame
  }