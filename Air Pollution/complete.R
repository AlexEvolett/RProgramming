complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  files <- list.files(path = directory, full.names = TRUE)
  data_c <- data.frame()
  for (i in id) {
    data <- read.csv(files[i])
    good <- data[complete.cases(data), ]
    data_c <- rbind(data_c, c(i, length(row.names(good))))
    
  }
  colnames(data_c) <- c('id', 'nobs')
  data_c
}