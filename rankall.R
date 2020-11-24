rankall <- function(outcome, num = "best") {
  #Read data:
  ocdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = NA , stringsAsFactors =  FALSE)
  
  
  #check that outcome is valid:
  
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
  
  ocdata2 <- subset(ocdata, select = c("Hospital.Name", "State", colname ))
  ocdata2[,colname] <- as.numeric(ocdata2[,colname]) 
  ocdata2 <- ocdata2[order(ocdata2$State, ocdata2[,3], ocdata2$Hospital.Name),]
  ocdata2 <- na.omit(ocdata2)
  
  #split data acc to state:
  ocdata3 <- split(ocdata2, ocdata2$State)
  
  #find the hospital with rank "num" in each state:
  hospital_Name <- sapply(ocdata3, function(elt, num = "best"){
    if (num == "best") {
      elt[1,1]
    }
    else if (num == "worst" ){
      elt[length(elt),1]
    }
    else  {
     elt[num,1]
    }
    
  } , num)
  
  # create data frame consists of the hospital name with rank num and the state 
df <- as.data.frame(hospital_Name, names(ocdata3))
df2 <- cbind(df, names(ocdata3))
colnames(df2)<- c("Hospital Name", "State")
df2
}
