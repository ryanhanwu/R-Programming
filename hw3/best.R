best <- function(state, outcome) {
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
  
  state_data <- outcomeData[outcomeData$State==state, ] #Select State Data
  targetColumn <- targetOutcome[targetOutcome$Outcome == outcome, ]$Col #Select Target Column
  
  suppressWarnings(state_data[, targetColumn] <- as.numeric(state_data[, targetColumn])) #as.numeric
  
  ## Return hospital name in that state with lowest 30-day death rate
  state_data[which.min(state_data[, targetColumn]), ]$Hospital.Name 
}