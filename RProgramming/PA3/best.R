best <- function(state, outcome) {
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
  outcomeByState <- outcomeByState[order(as.numeric(deathVector), decreasing=F),]
  winningHospitals <- outcomeByState[outcomeByState[[deathString]]==outcomeByState[1,][[deathString]],]$hospitalname
  sort(winningHospitals)[1]
  
}

#dataset[dataset[["Hospital.Name"]]=="GREATER BALTIMORE MEDICAL CENTER",][["Hospital.30.Day.Readmission.Rates.from.Pneumonia"]]
