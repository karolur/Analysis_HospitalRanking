rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
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
  allStates<-sort(unique(data$State))
  hospitals<-c()
  states<-c()
  data[, col] <- as.numeric(data[, col])
  
  if (num=="best") {
    for (state in allStates) {
      sbt<-subset(data,data$State ==state)
      sbt<-sbt[order(sbt[,col],sbt[,2],na.last = NA),]
      hospital<-subset(sbt[1,])$Hospital.Name
      hospitals<-append(hospitals,hospital)
    }
  } else if (num=="worst") {
    for (state in allStates) {
      sbt<-subset(data,data$State ==state)
      sbt<-sbt[order(sbt[,col],sbt[,2],na.last = NA),]
      hospital<-subset(sbt[nrow(sbt),])$Hospital.Name
      hospitals<-append(hospitals,hospital)
    }
  }
  else {
    for (state in allStates) {
      sbt<-subset(data,data$State ==state)
      sbt<-sbt[order(sbt[,col],sbt[,2],na.last = NA),]
      hospital<-subset(sbt[num,])$Hospital.Name
      hospitals<-append(hospitals,hospital)
    }

  } 
  results<-data.frame(hospital=hospitals,state=allStates)
  results
  
  
}