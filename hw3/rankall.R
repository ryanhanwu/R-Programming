rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", 
                          colClasses = "character")
  
  ## Check that state and outcome are valid
  targetOutcome <- data.frame(Outcome = c("heart attack", "heart failure", "pneumonia"), 
                              Col = c(11, 17, 23))
  
  #Validate input
  if (nrow(outcomeData[targetOutcome$Outcome == outcome, ]) == 0) 
    stop("invalid outcome")
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  nameChr <- character(0)
  allStates <- sort(unique(outcomeData$State))
  for (state in allStates) {
    stateData <- outcomeData[outcomeData$State==state, ] #Select State Data
    targetColumn <- targetOutcome[targetOutcome$Outcome == outcome, ]$Col #Select Target Column
    suppressWarnings(stateData[, targetColumn] <- as.numeric(stateData[, targetColumn])) 
    stateData <- stateData[order(stateData[, targetColumn], 
                                 stateData$Hospital.Name, 
                                 na.last = NA), ]
    
    if (num == "best") 
      rankNum <- 1 
    else if (num == "worst") 
      rankNum <- nrow(stateData) 
    else 
      suppressWarnings(rankNum <- as.numeric(num))
    
    nameChr <- c(nameChr, stateData[rankNum, ]$Hospital.Name)
  }
  data.frame(hospital = nameChr, state = allStates)
}