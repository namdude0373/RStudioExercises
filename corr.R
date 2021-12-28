corr <- function(directory, threshold = 0){
  # take a directory containing csv files and return a vector of correlations
  # between the amount of sulfate and nitrate observations for every file
  
  files <- list.files(file.path("Data", directory),full.names = TRUE)
  # create numeric vector so desired summary() output occurs when returning 
  # no results
  corr_vector <- vector("numeric", length = 0)
  for (file in files){
    file_data <- read.table(file, header = TRUE, sep = ",")
    complete_observations <- complete.cases(file_data)
    complete_file_data <- file_data[complete_observations, ]
    if (nrow(complete_file_data) > threshold){
      file_corr <- cor(complete_file_data$nitrate, complete_file_data$sulfate)
      corr_vector <- append(corr_vector, file_corr) 
    }
  }
  corr_vector
}