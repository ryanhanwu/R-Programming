complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  csvFiles = list.files(directory, pattern="*.csv")
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  filePaths <- file.path(directory, csvFiles[id])
  nobs <- numeric()
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  for (path in filePaths) {
    csvData <- read.csv(path)
    nobs <- c(nobs, sum(complete.cases(csvData)))
  }
  data.frame(id, nobs)
}