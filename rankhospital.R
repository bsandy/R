rankhospital <- function(st,outcome,num){

outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

## Get outcomes for the st 
outcomes <- outcomes[outcomes$State == st,]  

##  Validate st
  if (nrow(outcomes) == 0)
    print("invalid state")
    stop

  names(outcomes)[11] <- "heart attack"  
  names(outcomes)[17] <- "heart failure"  
  names(outcomes)[23] <- "pneumonia"  

##  Validate outcome
  tempcolnames <- NULL
  for (i in 11:ncol(outcomes)) 
    tempcolnames <- c(tempcolnames,colnames(outcomes[i]))  

  if (sum(!is.na(match(tempcolnames,outcome))) == 0)
    print("invalid outcome")
    stop

  outcomes[,11] <- as.numeric(outcomes[,11])
  outcomes[,17] <- as.numeric(outcomes[,17])
  outcomes[,23] <- as.numeric(outcomes[,23])

  outcomes <- outcomes[order(outcomes[outcome],outcomes["Hospital.Name"],na.last = TRUE),]

  if(num == 'best')
    return(outcomes[1,"Hospital.Name"])
  
  if(num != 'worst')
    return(outcomes[num,"Hospital.Name"])
  
  for (i in 1:nrow(outcomes))
    if(!is.na(outcomes[i,outcome]))
      hospital <- outcomes[i,"Hospital.Name"]      
  
  hospital

}
  