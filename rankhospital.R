rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  ## Check that state and outcome are valid
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
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  sbt<-subset(data,data$State ==state)
  sbt[, col] <- as.numeric(sbt[, col])
  sbt<-sbt[order(sbt[,col],sbt[,2],na.last = NA),] 
  if (num=="best") {
    subset(sbt[1,],na.rm=TRUE)$Hospital.Name 
  } else if (num=="worst") {
    subset(sbt[nrow(sbt),],na.rm=TRUE)$Hospital.Name 
  }
  else {
    subset(sbt[num,],na.rm=TRUE)$Hospital.Name
  }
  
}