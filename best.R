best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state is valid
  stateVal<-unique(data$State)
  if (!state %in% stateVal) {
    stop("invalid state")
  }
  
  ## Check that outcome is valid
  if (outcome=="heart attack") {
    col<-11   
  } else if (outcome=="heart failure") {
    col<-17
  }
  else if(outcome=="pneumonia") {
    col<-23
  }
  else {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  sbt<-subset(data,data$State ==state)
  sbt[, col] <- as.numeric(sbt[, col])
  bestVal<-min(sbt[,col],na.rm=TRUE)
  subset(sbt,sbt[,col]==bestVal,na.rm=TRUE)$Hospital.Name
  
}
