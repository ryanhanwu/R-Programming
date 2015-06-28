pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  csvFiles = list.files(directory, pattern="*.csv")
  filePaths <- file.path(directory, csvFiles[id])
  
  allData <- matrix(NA, 0, 2)
  for (path in filePaths) {
    csvData <- read.csv(path)
    allData <- rbind(allData, csvData)
  }
  
  mean(allData[,pollutant],na.rm=TRUE)

}



