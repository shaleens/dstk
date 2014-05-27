rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  dataset <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if(!(state %in% dataset$State)) {
    stop("invalid state")
  }
  
  if(!(outcome == "heart attack"  || outcome == "heart failure" || outcome == "pneumonia")) {
    stop("invalid outcome")
  }
  
  dataSetColNames <- tolower(gsub("[[:punct:]]", "", names(dataset)))
  names(dataset) <- dataSetColNames
  outcomeByState <- split(dataset, dataset$state)
  deathString <- paste("Hospital 30-Day Death (Mortality) Rates from",outcome, sep=""
  )
  deathString <- tolower(gsub("[[:punct:]]", "", deathString))
  deathString <- gsub(" ","", deathString)
  outcomeByState <- outcomeByState[[state]]
  deathVector <- outcomeByState[[deathString]]
  deathVector <- as.numeric(deathVector)
  outcomeByState <- outcomeByState[order(deathVector, outcomeByState[["hospitalname"]]),]
  #winningHospitals <- outcomeByState[outcomeByState[[deathString]]==outcomeByState[1,][[deathString]],]$hospitalname
  outcomeByState <- outcomeByState[!is.na(as.numeric(outcomeByState[[deathString]])),]
  rowIndex <- 1
  if(num == "best") {
    rowIndex <- 1
  }
  else if(num == "worst") {
    rowIndex <- nrow(outcomeByState)
  }
  else {
    rowIndex <- num
  }
  
  outcomeByState[rowIndex,]$hospitalname
  
}
