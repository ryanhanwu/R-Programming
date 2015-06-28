rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", 
                          colClasses = "character")
  
  ## Check that state and outcome are valid
  targetOutcome <- data.frame(Outcome = c("heart attack", "heart failure", "pneumonia"), 
                              Col = c(11, 17, 23))
  
  #Validate input
  if (nrow(outcomeData[outcomeData$State == state, ]) == 0) 
    stop("invalid state")
  
  if (nrow(outcomeData[targetOutcome$Outcome == outcome, ]) == 0) 
    stop("invalid outcome")
  
  stateData <- outcomeData[outcomeData$State==state, ] #Select State Data
  targetColumn <- targetOutcome[targetOutcome$Outcome == outcome, ]$Col #Select Target Column
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  suppressWarnings(stateData[, targetColumn] <- as.numeric(stateData[, targetColumn])) 
  stateData <- stateData[order(stateData[, targetColumn], 
                                 stateData$Hospital.Name, 
                                 na.last = NA), ] # Remove NA
  
  if (num == "best") 
    num <- 1
  if (num == "worst") 
    num <- nrow(stateData)
  
  stateData[as.numeric(num), ]$Hospital.Name
  
}