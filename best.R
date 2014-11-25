##Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
##outcome name. The function reads the outcome-of-care-measures.csv le and returns a character vector
##with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specifed outcome
##in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
##be one of \heart attack", \heart failure", or \pneumonia". Hospitals that do not have data on a particular
##outcome should be excluded from the set of hospitals when deciding the rankings.

best <- function(st,outcome){

outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

## Get outcomes for the st 
outcomes <- outcomes[outcomes$State == st,]  

##  Validate st
  if (nrow(outcomes) == 0)
    return("invalid state")

  names(outcomes)[11] <- "heart attack"  
  names(outcomes)[17] <- "heart failure"  
  names(outcomes)[23] <- "pneumonia"  

##  Validate outcome
  tempcolnames <- NULL
  for (i in 11:ncol(outcomes)) 
    tempcolnames <- c(tempcolnames,colnames(outcomes[i]))  

  if (sum(!is.na(match(tempcolnames,outcome))) == 0)
    return("invalid outcome")

  outcomes[,11] <- as.numeric(outcomes[,11])
  outcomes[,17] <- as.numeric(outcomes[,17])
  outcomes[,23] <- as.numeric(outcomes[,23])

## outcomes.rank <- rank(outcomes, na.last = TRUE)

  minv <- min(outcomes[,outcome],na.rm = TRUE)

  hospitals <- NULL
  for (i in 1:nrow(outcomes))
    if(!is.na(outcomes[i,outcome]))
      if(outcomes[i,outcome] == minv) 
        hospitals <- c(hospitals,outcomes[i,"Hospital.Name"])      
  min(hospitals)      
}
  