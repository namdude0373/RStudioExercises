pollutantmean <- function(directory, pollutant, id = 1:332){
  # calculate the mean of any pollutant given a monitor id
  data <- data.frame()
  for (monitor in id){ # file names are monitor names
    id_padded <- formatC(monitor, width = 3, flag = 0)
    filename <- paste(id_padded, ".csv", sep = "")
    file <- read.table(file.path("Data", directory, filename), header = TRUE,
                       sep = ",")
    data <- rbind(data, file) # append csv file to data frame
  }
  print(head(data, n = 10)) # debugging
  mean(data[[pollutant]][!is.na(data[[pollutant]])]) # excluding NA values
}