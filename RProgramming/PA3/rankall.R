rankall <- function(outcome, num = "best") {
  ## get the index depending on the value of num
  getIndex <- function (number, outcomeByState) 
  {
    
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
    rowIndex
  }
  
  ## read outcome data
  dataset <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  state.rank <- NULL
  
  ##generate the string to evaluate the appropriate column
  deathString <- paste("Hospital 30-Day Death (Mortality) Rates from",outcome, sep=""
  )
  deathString <- tolower(gsub("[[:punct:]]", "", deathString))
  deathString <- gsub(" ","", deathString)
  
  if(!(outcome == "heart attack"  || outcome == "heart failure" || outcome == "pneumonia")) {
    stop("invalid outcome")
  }
  
  ##remove punctuations and spaces from the column names 
  dataSetColNames <- tolower(gsub("[[:punct:]]", "", names(dataset)))
  deathString <- gsub(" ","", deathString)
  
  names(dataset) <- dataSetColNames
  
  ##split by state and travers
  outcomeByState <- split(dataset, dataset$state)
  for(state.name in names(outcomeByState)) {
    outcomeForState <- outcomeByState[[state.name]]
    deathVector <- outcomeForState[[deathString]]
    deathVector <- as.numeric(deathVector)
    ##order by deathVector. break ties alphabetically by hospital name
    outcomeForStateOrdered <- outcomeForState[order(deathVector, outcomeForState[["hospitalname"]]),]
    ##remove rows with NAs in state
    outcomeForStateOrdered <- outcomeForStateOrdered[!is.na(as.numeric(outcomeForStateOrdered[[deathString]])),]
    hospital.name <- outcomeForStateOrdered[getIndex(num,outcomeForStateOrdered),]$hospitalname
    state.rank <- rbind(state.rank,c(hospital.name, state.name))
  }
  state.rank <- as.data.frame(state.rank)
  colnames(state.rank) <- c("hospital","state")
  rownames(state.rank) <- state.rank$state
  

 state.rank
}
