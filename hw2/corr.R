corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  csvFiles = list.files(directory, pattern="*.csv")
  completeDataFrame <- complete(directory)
  nobs <- completeDataFrame$nobs
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  completeIds <- completeDataFrame$id[nobs > threshold]
  filePaths <- file.path(directory, csvFiles[completeIds])
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  corr = numeric()
  for (path in filePaths) {
    csvData <- read.csv(path)
    dff = csvData[complete.cases(csvData), ]
    corr = c(corr, cor(dff$sulfate, dff$nitrate))
  }
  corr
}