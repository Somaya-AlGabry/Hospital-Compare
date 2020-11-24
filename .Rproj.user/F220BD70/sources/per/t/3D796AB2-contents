best <- function(state, outcome) {
  #Read data:
  ocdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = NA , stringsAsFactors =  FALSE)

  
  #check that arguments are valid:
  if (!state %in% ocdata$State) {
    stop("invalid state")
  }
  
  if (outcome == "heart attack") {
    colname= "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
  } else if (outcome =="heart failure"){
    colname= "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else if (outcome == "pneumonia") {
    colname = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  } else {
    stop ("invalid outcome")
  }
  
  #order data:
  
  ocdata2 <- subset(ocdata, State == state, select = c("Hospital.Name", colname ))
  ocdata2[,colname] <- as.numeric(ocdata2[,colname]) 
  ocdata2 <- ocdata2[order(ocdata2[,2], ocdata2[,1]),]
  ocdata2 <- na.omit(ocdata2)
  
  #return hospital name:
   ocdata2[1, 1]
   
}


